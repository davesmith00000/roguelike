resolvers += "Sonatype OSS Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots"

addSbtPlugin("org.scala-js"              % "sbt-scalajs"  % "1.13.0")
addSbtPlugin("io.indigoengine"          %% "sbt-indigo"   % "0.14.1-SNAPSHOT")
addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.2")
addSbtPlugin("ch.epfl.scala"             % "sbt-scalafix" % "0.9.31")
addSbtPlugin("com.github.reibitto"       % "sbt-welcome"  % "0.2.2")
addSbtPlugin("com.typesafe.sbt"          % "sbt-site"     % "1.4.1")
addSbtPlugin("com.typesafe.sbt"          % "sbt-ghpages"  % "0.6.3")
addSbtPlugin("org.scalameta"             % "sbt-scalafmt" % "2.4.6")
