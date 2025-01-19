addSbtPlugin("org.scala-js"        % "sbt-scalajs"  % "1.18.1")
addSbtPlugin("io.indigoengine"    %% "sbt-indigo"   % "0.17.1-SNAPSHOT")
addSbtPlugin("org.typelevel"       % "sbt-tpolecat" % "0.5.2")
addSbtPlugin("ch.epfl.scala"       % "sbt-scalafix" % "0.13.0")
addSbtPlugin("com.github.reibitto" % "sbt-welcome"  % "0.5.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-site"     % "1.4.1")
addSbtPlugin("com.typesafe.sbt"    % "sbt-ghpages"  % "0.6.3")
addSbtPlugin("org.scalameta"       % "sbt-scalafmt" % "2.5.2")

libraryDependencies += "org.typelevel" %% "scalac-options" % "0.1.7"
