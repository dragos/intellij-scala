package org.jetbrains.jps.incremental.scala.data

import java.io.File

object HydraData {

//  val patchVersion = "hydra12"
  val hydraLib = sys.props.get("hydra.home").orElse(sys.env.get("HYDRA_HOME")).map(path => new File(path, "lib")).filter(_.exists)

  val hydraFiles = hydraLib.map(_.listFiles()).toSeq.flatten

  val hydraBridge: Option[File] = hydraFiles.find(_.getName == "hydra-bridge-sources.jar")

  def compilerFileName(version: String) = s".*scala-compiler-$version-hydra\\d\\d\\.jar"

  def compilerJar(version: String): Option[File] = {
    hydraFiles.find(_.getName.matches(compilerFileName(version)))
  }

  def reflectJar(version: String): Option[File] = {
  val reflectFileName = s".*scala-reflect-$version-hydra\\d\\d\\.jar"
    hydraFiles.find(_.getName.matches(reflectFileName))
  }

  def otherFiles(version: String): Seq[File] = {
    //hydraFiles.filterNot(_.getName.contains(compilerFileName(version)))
    hydraFiles.filterNot(_.getName.contains("scala-compiler"))
  }
}
