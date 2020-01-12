name := "scalapoker"

organization := "net.oyunkeyf"

version := "0.0.1"

scalaVersion := "2.13.1"

libraryDependencies ++= List(
  "org.scalaz" %% "scalaz-core" % "7.2.29",
  "org.specs2" %% "specs2-core" % "4.7.0" % "test",
  "org.specs2" %% "specs2-scalaz" % "4.7.0" % "test",
  "com.github.ornicar" %% "scalalib" % "6.8",
)

resolvers ++= Seq(
  "lila-maven" at "https://raw.githubusercontent.com/ornicar/lila-maven/master",
  "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"
)

scalacOptions ++= Seq(
  "-deprecation", "-unchecked", "-feature", "-language:_",
  "-Xfatal-warnings",
  //"-Ywarn-unused:_",
  "-Ywarn-value-discard", "-Ywarn-dead-code",
  "-Xlint:missing-interpolator",
  "-Ydelambdafy:method", "-target:jvm-1.8"
)

publishTo := Some(Resolver.file("file", new File(sys.props.getOrElse("publishTo", ""))))
