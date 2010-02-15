import sbt._

class ScamongoProject(info: ProjectInfo) extends DefaultProject(info) {
	override def useMavenConfigurations = true
	
	override def compileOptions = super.compileOptions ++ Seq(Unchecked)
	
	val mongo = "org.mongodb" % "mongo-java-driver" % "1.2.1" % "compile->default"

	val liftjson = "net.liftweb" % "lift-json" % "2.0-M2" % "compile->default"
	val liftrecord = "net.liftweb" % "lift-record" % "2.0-M2" % "compile->default"

	val junit = "junit" % "junit" % "4.5"
  val specs = "org.scala-tools.testing" % "specs" % "1.6.2"

	// other repositories
  var nexus = "nexus" at "https://nexus.griddynamics.net/nexus/content/groups/public"
	val scalaToolsSnapshots = "Scala-Tools Maven2 Snapshots Repository" at "http://scala-tools.org/repo-snapshots"
}
