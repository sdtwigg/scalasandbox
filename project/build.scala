import sbt._
import Keys._

object MacroBuild extends Build {
  lazy val main  = Project("sandbox", file(".")) dependsOn(macro)
  lazy val macro = Project("macro",   file("macro"))
}
