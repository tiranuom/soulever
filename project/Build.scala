import sbt._
import Keys._
import com.earldouglas.xsbtwebplugin.PluginKeys._
//import com.github.siasia.PluginKeys._
import com.earldouglas.xsbtwebplugin.WebPlugin._
//import com.github.siasia.WebPlugin._

object BuildSettings {
  val buildSettings = Defaults.defaultSettings ++ Seq(
    organization := "com.soulever",
    version := "1.0.1",
    scalaVersion := "2.11.1",
    scalacOptions ++= Seq(),
    resolvers += Resolver.sonatypeRepo("releases"),
    resolvers += Resolver.sonatypeRepo("snapshots")
  )
}

object SouleverBuild extends Build {
  import BuildSettings._
  import com.typesafe.sbt.SbtGit.GitKeys._

  val vaadinVersion = "7.0.2"

  val vaadinServer = "com.vaadin" % "vaadin-server" % vaadinVersion
  val vaadinClient = "com.vaadin" % "vaadin-client" % vaadinVersion
  val vaadinTheme = "com.vaadin" % "vaadin-themes" %  vaadinVersion
  val servletApi = "javax.servlet" % "servlet-api" % "2.4" % "provided"
  val typesafeConfig = "com.typesafe" % "config" % "1.2.0"

  val liftVersion = "2.6-M4"

  val liftweb = "net.liftweb" %% "lift-webkit" % liftVersion %"compile"
  val liftModules = "net.liftmodules" %% "lift-jquery-module_2.6" % "2.8-SNAPSHOT"
  val liftJetty = "org.eclipse.jetty" % "jetty-webapp" % "8.1.7.v20120910" % "container,test"
  val liftJettyPlus = "org.eclipse.jetty" % "jetty-plus" % "8.1.7.v20120910" % "container,test" // For Jetty Config
  val liftServlet = "org.eclipse.jetty.orbit" % "javax.servlet" % "3.0.0.v201112011016" % "container,test" artifacts Artifact("javax.servlet", "jar", "jar")
  val logback = "ch.qos.logback" % "logback-classic" % "1.0.6"
  val specs2 = "org.specs2" %% "specs2" % "2.3.12" % "test"

  val jettyContainer = "org.eclipse.jetty" % "jetty-webapp" % "8.1.0.RC1" % "container"

  lazy val root:Project = Project(
    id  = "root",
    base = file(".")
  ) aggregate(souleverMetamacro, souleverMacro, souleverVaadin, vaadinTest, souleverLift, liftTest)

  lazy val souleverMetamacro:Project = Project(
    "soulever-metamacro",
    file("soulever-metamacro"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(typesafeConfig)
    )
  )

  lazy val souleverMacro:Project = Project(
    "soulever-macro",
    file("soulever-macro"),
    settings = buildSettings ++ Seq(
      libraryDependencies <+= (scalaVersion)("org.scala-lang" % "scala-reflect" % _),
      libraryDependencies ++= Seq(typesafeConfig)
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
      libraryDependencies ++= Seq(vaadinServer, vaadinClient, vaadinTheme, servletApi, typesafeConfig)
    )
  ) dependsOn (souleverVaadin, souleverMacro)

  lazy val souleverLift:Project = Project(
    "soulever-lift",
    file("soulever-lift"),
    settings = buildSettings ++ webSettings ++ Seq(
      libraryDependencies ++= Seq(liftweb, liftModules, liftServlet, liftJetty, liftJettyPlus),
      port in container.Configuration := 8081
    )
  ) dependsOn souleverMacro

  lazy val liftTest:Project = Project(
    "lift-test",
    file("lift-test"),
    settings = buildSettings ++ webSettings ++ Seq(
      libraryDependencies ++= Seq(liftweb, liftModules, liftJetty, liftJettyPlus, liftServlet, logback, specs2),
      port in container.Configuration := 8081
    )
  ) dependsOn (souleverLift, souleverMacro)
}
