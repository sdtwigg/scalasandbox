import sbt._
import Keys._

object BuildSettings {
  val paradiseVersion = "2.0.1"
  val buildSettings = Defaults.defaultSettings ++ Seq(
    resolvers += Resolver.sonatypeRepo("releases"),
    addCompilerPlugin("org.scalamacros" % "paradise" % paradiseVersion cross CrossVersion.full)
  )
}

object MacroBuild extends Build {
  import BuildSettings._

  lazy val main   = Project("sandbox", file("."), settings = buildSettings) dependsOn(macros)
  lazy val macros = Project("macro",   file("macro"), settings = buildSettings)
}
