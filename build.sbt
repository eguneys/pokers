name := "template"

version := "0.0.1"

scalaVersion := "2.12.6"


libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.23",
  "org.specs2" %% "specs2-core" % "4.2.0" % "test",
  "com.github.ornicar" %% "scalalib" % "6.6",
  "com.typesafe.akka" %% "akka-http" % "10.1.8",
  "com.typesafe.akka" %% "akka-stream" % "2.5.19"
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-language:_",
  "-Xfatal-warnings",
  "-Ywarn-unused-import", "-Ywarn-value-discard", "-Ywarn-dead-code",
  "-Xlint:missing-interpolator",
  "-Ydelambdafy:method", "-target:jvm-1.8"
)

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
