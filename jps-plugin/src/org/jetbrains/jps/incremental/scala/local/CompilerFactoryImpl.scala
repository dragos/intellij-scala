package org.jetbrains.jps.incremental.scala
package local

import java.io.File
import java.net.URLClassLoader

import org.jetbrains.jps.incremental.scala.data.{CompilerData, CompilerJars, HydraData, SbtData}
import org.jetbrains.jps.incremental.scala.local.CompilerFactoryImpl._
import org.jetbrains.jps.incremental.scala.model.IncrementalityType
import sbt.compiler.{AggressiveCompile, AnalyzingCompiler, IC}
import sbt.inc.AnalysisStore
import sbt.{ClasspathOptions, Path, ScalaInstance}
import xsbti.{F0, Logger}

/**
 * @author Pavel Fatin
 */
class CompilerFactoryImpl(sbtData: SbtData) extends CompilerFactory {
  
  def createCompiler(compilerData: CompilerData, client: Client, fileToStore: File => AnalysisStore): Compiler = {

    val scalac: Option[AnalyzingCompiler] = getScalac(sbtData, compilerData.compilerJars, client)

    compilerData.compilerJars match {
      case Some(jars) if jars.dotty.isDefined =>
        return new DottyCompiler(createScalaInstance(jars), jars)
      case _ =>
    }

    compilerData.incrementalType match {
      case IncrementalityType.SBT =>
        val javac = {
          val scala = getScalaInstance(compilerData.compilerJars)
                  .getOrElse(new ScalaInstance("stub", null, new File(""), new File(""), Seq.empty, None))
          val classpathOptions = ClasspathOptions.javac(compiler = false)
          AggressiveCompile.directOrFork(scala, classpathOptions, compilerData.javaHome)
        }
        new SbtCompiler(javac, scalac, fileToStore)
        
      case IncrementalityType.IDEA =>
        if (scalac.isDefined) new IdeaIncrementalCompiler(scalac.get)
        else throw new IllegalStateException("Could not create scalac instance")

    }

  }

  def getScalac(sbtData: SbtData, compilerJars: Option[CompilerJars], client: Client): Option[AnalyzingCompiler] = {
    getScalaInstance(compilerJars).map { scala =>
    val compiledIntefaceJar = getOrCompileInterfaceJar(sbtData.interfacesHome, sbtData.sourceJar,
        sbtData.interfaceJar, scala, sbtData.javaClassVersion, Option(client))

      IC.newScalaCompiler(scala, compiledIntefaceJar, ClasspathOptions.javac(compiler = false))
    }
  }

  private def getScalaInstance(compilerJars: Option[CompilerJars]): Option[ScalaInstance] =
    compilerJars.map(createScalaInstance)
}

object CompilerFactoryImpl {
  private val scalaInstanceCache = new Cache[CompilerJars, ScalaInstance](3)

  def createScalaInstance(jars: CompilerJars): ScalaInstance = {
    scalaInstanceCache.getOrUpdate(jars) {
      val hydraSupportedScalaVersion = "2.11.8"
      val desiredVersion = readProperty(jars.compiler, "compiler.properties", "version.number").getOrElse("0")
      val compilerJar = HydraData.compilerJar(hydraSupportedScalaVersion)

      println("Trying to use hydra:")
      // hard-code the version we support right now
      val useHydra = (desiredVersion == hydraSupportedScalaVersion) && compilerJar.isDefined
      println(
        s"""
           |hydraSupportedScalaVersion: $hydraSupportedScalaVersion
           |desiredVersion: $desiredVersion
           |Hydra lib dir: ${HydraData.hydraLib}
           |Hydra compilerJar: $compilerJar
           |useHydra: $useHydra
         """.stripMargin)

      if (compilerJar.isEmpty)
        println("Hydra files: " + HydraData.hydraFiles.mkString("\n"))
      val classLoader = {
        val hydraJars = HydraData.hydraFiles

        val urls = if (useHydra)
          Path.toURLs(jars.library +: hydraJars)
        else
          Path.toURLs(jars.library +: jars.compiler +: jars.extra)

        new URLClassLoader(urls, sbt.classpath.ClasspathUtilities.rootLoader)
      }

      val version = readScalaVersionIn(classLoader)

      if (useHydra)
        new ScalaInstance(version.getOrElse("unknown"), classLoader,
          jars.library,
          compilerJar.getOrElse(jars.compiler),
          HydraData.otherFiles(hydraSupportedScalaVersion) ++ jars.extra.filterNot(_.getName.contains("scala-reflect")), version)
      else
        new ScalaInstance(version.getOrElse("unknown"), classLoader, jars.library, jars.compiler, jars.extra, version)
    }


  }
  
  def readScalaVersionIn(classLoader: ClassLoader): Option[String] = 
    readProperty(classLoader, "compiler.properties", "version.number")

  def getOrCompileInterfaceJar(home: File,
                                       sourceJar: File,
                                       interfaceJar: File,
                                       scalaInstance: ScalaInstance,
                                       javaClassVersion: String,
                                       client: Option[Client]): File = {

    val scalaVersion = scalaInstance.actualVersion
    val compilerInterfaceSources = if (scalaVersion.contains("hydra")) HydraData.hydraBridge.get else sourceJar

    val interfaceId = "compiler-interface-" + scalaVersion + "-" + javaClassVersion
    val targetJar = new File(home, interfaceId + ".jar")

    if (!targetJar.exists) {
      client.foreach(_.progress("Compiling Scalac " + scalaVersion + " interface"))
      home.mkdirs()
      IC.compileInterfaceJar(interfaceId, compilerInterfaceSources, targetJar, interfaceJar, scalaInstance, NullLogger)
    }

    targetJar
  }
}

object NullLogger extends Logger {
  def error(p1: F0[String]) {}

  def warn(p1: F0[String]) {}

  def info(p1: F0[String]) {}

  def debug(p1: F0[String]) {}

  def trace(p1: F0[Throwable]) {}
}
