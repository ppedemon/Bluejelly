/*
 * The Bluejelly project, Copyright 2012.
 *
 * This source code is distributed under the terms of
 * the BSD license, see the LICENSE file for details.
 */

import sbt._
import Keys._

import sbtassembly.Plugin._
import AssemblyKeys._

object BluejellyBuild extends Build {

  import Dist._

  override lazy val settings = super.settings ++ Seq(
    name := "bluejelly",
    version := "1.0",
    scalacOptions ++= Seq("-deprecation","-unchecked")
  )

  lazy val bluejelly = Project(
    id = "bluejelly",
    base = file(".")) aggregate(runtime,utils,asm,l4,bjc)

  lazy val runtime = Project(
    id = "bluejelly-runtime",
    base = file("bluejelly-runtime"),
    settings = Project.defaultSettings ++ assemblySettings ++ Seq(
      libraryDependencies ++= Seq(Deps.asmWeb, Deps.junit),
      fork in Test := true,
      resourceGenerators in Compile <+= (
          resourceManaged in Compile, version) map { (dir,v) =>
        Resources.genCfg(dir, "brt-cfg.properties", "brt.version=%s" format(v))
      },
      compile in Compile <<= (
          streams,
          sourceDirectory in Compile,
          classDirectory in Compile, 
          dependencyClasspath in Compile, 
          compile in Compile) map { (s,sd,cd,cp,a) =>
        val classPath = cd +: (cp.files filter {_.toString contains "asm"})
        Prims.gen(s.log, sd, cd, classPath)
        a
      },
      test in assembly := {},
      assembleArtifact in packageScala := false,
      jarName in assembly <<= (name,version) { (n,v) => "%s-%s.jar" format (n,v) },
      distTask
    )
  )

  lazy val utils = Project(
    id = "bluejelly-utils",
    base = file("bluejelly-utils")
  )

  lazy val asm = Project(
    id = "bluejelly-asm",
    base = file("bluejelly-asm"),
    settings = Project.defaultSettings ++ assemblySettings ++ Seq(
      libraryDependencies ++= Seq(Deps.asmWeb,Deps.scalaTest),
      resourceGenerators in Compile <+= (
          resourceManaged in Compile, version) map { (dir,v) =>
        Resources.genCfg(dir, "bas-cfg.properties", "bas.version=%s" format(v))
      },
      resourceGenerators in Test <+= (
          streams, 
          resourceManaged in Test, 
          classDirectory in (runtime,Compile),
          dependencyClasspath in Test) map { (s,dir,cd,cp) =>
        val out = dir / "brt.properties"
        Resources.genBrtProps(out, cd, cp.files)
        Seq(out)
      },
      test <<= test dependsOn(compile in (runtime,Compile)),
      test in assembly := {},
      jarName in assembly <<= (name,version) { (n,v) => "%s-%s.jar" format (n,v) },
      distTask
    )
  ) dependsOn(utils)

  lazy val l4 = Project(
    id = "bluejelly-l4",
    base = file("bluejelly-l4"),
    settings = Project.defaultSettings ++ assemblySettings ++ Seq(
      libraryDependencies += Deps.scalaTest,
      resourceGenerators in Compile <+= (
          resourceManaged in Compile, version) map { (dir,v) =>
        Resources.genCfg(dir, "l4c-cfg.properties", "l4c.version=%s" format(v))
      },
      resourceGenerators in Test <+= (
          streams, 
          resourceManaged in Test, 
          classDirectory in (runtime,Compile),
          dependencyClasspath in Test) map { (s,dir,cd,cp) =>
        val out = dir / "brt.properties"
        Resources.genBrtProps(out, cd, cp.files)
        Seq(out)
      },
      test <<= test dependsOn(compile in (runtime,Compile)),
      test in assembly := {},
      jarName in assembly <<= (name,version) { (n,v) => "%s-%s.jar" format (n,v) },
      distTask
    ) 
  ) dependsOn(asm,utils)
  
  lazy val bjc = Project(
    id = "bluejelly-bjc",
    base = file("bluejelly-bjc"),
    settings = Project.defaultSettings ++ assemblySettings ++ Seq(
      libraryDependencies += Deps.scalaTest,
      test in assembly := {},
      jarName in assembly <<= (name,version) { (n,v) => "%s-%s.jar" format (n,v) },
      distTask
    )
  ) dependsOn(l4,utils)

  /**
   * Bag of dependency specifications.
   */
  private object Deps {
    val asmWeb    = "asm" % "asm-all" % "3.2"
    val junit     = "com.novocode" % "junit-interface" % "0.10-M1" % "test"
    val scalaTest = "org.scalatest" %% "scalatest" % "1.9.1" % "test"
  }

  /**
   * Auxiliary code generating resources.
   */
  private object Resources {
    // Generate properties file for bluejelly-runtinme classpath,
    // user for testing compiled code
    def genBrtProps(out:File, brtBin:File, brtCp:Seq[File]) {
      val sep = System getProperty "path.separator"
      val cp = brtBin +: (brtCp filter {_.toString contains "asm-all"})
      IO.write(out, "cp=%s" format (cp mkString sep))
    }
    
    // Generate a configuration properties file with the given contents
    def genCfg(dir:File, name:String, contents:String):Seq[File] = {
      val f = dir / name
      IO.write(f, contents)
      Seq(f)
    }
  }

  /**
   * Transform Runtime primitive classes.
   */
  private object Prims {
    private val sep = System getProperty "path.separator"

    // Expand this to include further primitive modules
    private val prims = Seq("Int","Double","BigInt")
 
    def gen(log:Logger, src:File, bin:File, cp:Seq[File]) {
      cleanup(log, bin)
      compile(log, src, bin, cp)
      transform(log, cp)
    }

    private def cleanup(log:Logger, bin:File) {
      val primFiles = prims map { n => bin / "bluejelly" / (n + ".class") }
      IO.delete(primFiles)
      //log.info("Removed prims")
    }

    private def compile(log:Logger, src:File, bin:File, cp:Seq[File]) {
      val cps = cp mkString sep
      val sources = prims map { n => src / "java" / "bluejelly" / (n + ".java")}
      val cmd = Seq("javac","-cp",cps,"-d",bin.toString) ++ sources map {_.toString}
      //log.info("Executing: " + (cmd mkString " "))
      val status = cmd.!
      log.info("Prims.compile returned with status: " + status)
    }

    private def transform(log:Logger, cp:Seq[File]) {
      import scala.sys.process._
      val cps = cp mkString sep
      val gen = "bluejelly.PrimTransformer"
      val cmd = Seq("java","-cp",cps,gen) ++ prims
      //log.info("Executing: " + (cmd mkString " "))
      val status = cmd.!
      log.info("Prims.gen returned with status: " + status)
    }
  }

  /* 
   * Custom task designed to create a distribution zip file for some app,
   * consisting of the app's jar and a launcher script assumed to live
   * under source-directory/scripts.
   *
   * Caveat: IO.zip loses the executable flag of the launcher script :(
   */
  private object Dist {
    val dist = TaskKey[Unit]("dist","generate distribution zip for the current project")

    val distTask = dist <<= (
        assembly,sourceDirectory,name,version,target)  map { (f,d,n,v,t) =>
      val files:Seq[File] = IO.listFiles(d / "scripts") :+ f
      val contents = files x Path.flat
      val out = t / ("%s-%s.zip" format (n,v))
      IO.zip(contents map { case (f,p) => (f, "%s-%s/%s" format (n,v,p)) }, out)
    }
  }
}

