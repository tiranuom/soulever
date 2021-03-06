import sbt._
import Keys._
import com.earldouglas.xsbtwebplugin.PluginKeys._
//import com.github.siasia.PluginKeys._
import com.earldouglas.xsbtwebplugin.WebPlugin._
//import com.github.siasia.WebPlugin._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "org.scalamacros",
    version := "1.0.0",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq(),
    resolvers += Resolver.sonatypeRepo("snapshots"),
    addCompilerPlugin("org.scala-lang.plugins" % "macro-paradise" % "2.0.0-SNAPSHOT" cross CrossVersion.full))
}

object SouleverBuild extends Build {
  import BuildSettings._

  val vaadinVersion = "7.0.2"

  val vaadinServer = "com.vaadin" % "vaadin-server" % vaadinVersion
  val vaadinClient = "com.vaadin" % "vaadin-client" % vaadinVersion
  val vaadinTheme = "com.vaadin" % "vaadin-themes" %  vaadinVersion
  val servletApi = "javax.servlet" % "servlet-api" % "2.4" % "provided"

  val jettyContainer = "org.eclipse.jetty" % "jetty-webapp" % "8.1.0.RC1" % "container"

  lazy val root:Project = Project(
    id  = "root",
    base = file(".")
  ) aggregate(souleverMacro, souleverVaadin, vaadinTest)

  lazy val souleverMacro:Project = Project(
    "soulever-macro",
    file("soulever-macro"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _)
    )
  )

  lazy val souleverVaadin:Project = Project(
    "soulever-vaadin",
    file("soulever-vaadin"),
    settings = buildSettings ++ Seq(
      libraryDependencies ++= Seq(vaadinServer, servletApi)
    )
  ) dependsOn souleverMacro

  lazy val vaadinTest: Project = Project(
    "vaadin-test",
    file("vaadin-test"),
    settings = buildSettings ++ webSettings ++ Seq(
      libraryDependencies ++= Seq(jettyContainer),
      libraryDependencies ++= Seq(vaadinServer, vaadinClient, vaadinTheme, servletApi)
    )
  ) dependsOn souleverVaadin

}
