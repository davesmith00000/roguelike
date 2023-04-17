import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

lazy val roguelike =
  (project in file("."))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings(
      name         := "roguelike",
      version      := "0.0.1",
      scalaVersion := "3.2.2",
      organization := "roguelike",
      libraryDependencies ++= Seq(
        "org.scalameta" %%% "munit" % "0.7.29" % Test
      ),
      testFrameworks += new TestFramework("munit.Framework"),
      Test / scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      },
      showCursor            := true,
      title                 := "My Generic Roguelike",
      gameAssetsDirectory   := "assets",
      windowStartWidth      := 1280,
      windowStartHeight     := 720,
      disableFrameRateLimit := false,
      electronInstall       := indigoplugin.ElectronInstall.Latest,
      libraryDependencies ++= Seq(
        "io.indigoengine" %%% "indigo-json-circe"    % "0.15.0-RC1",
        "io.indigoengine" %%% "indigo"               % "0.15.0-RC1",
        "io.indigoengine" %%% "indigo-extras"        % "0.15.0-RC1",
        "io.indigoengine" %%% "roguelike-starterkit" % "0.2.1-SNAPSHOT"
      ),
      scalafixOnCompile := true,
      semanticdbEnabled := true,
      semanticdbVersion := scalafixSemanticdb.revision
    )
    .settings(
      code := { "code ." ! }
    )
    .settings(
      Compile / sourceGenerators += Def.task {
        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / "gamedata"
        ) { (gameData: Set[File]) =>
          val outDir = (Compile / sourceManaged).value
          MeleeGen.gen("Melee", "roguelike.model.gamedata", gameData, outDir) ++
            ArmourGen
              .gen("Armour", "roguelike.model.gamedata", gameData, outDir) ++
            ConsumablesGen.gen(
              "Consumables",
              "roguelike.model.gamedata",
              gameData,
              outDir
            ) ++
            RangedGen
              .gen("Ranged", "roguelike.model.gamedata", gameData, outDir) ++
            HostilesGen
              .gen("Hostiles", "roguelike.model.gamedata", gameData, outDir) ++
            KeyMappingsGen
              .gen(
                "KeyMapping",
                "roguelike.model.gamedata",
                gameData,
                outDir
              ) ++
            AssetsGen
              .gen(
                "GameAssets",
                "roguelike.assets",
                gameData,
                outDir
              )
        }

        cachedFun(IO.listFiles(baseDirectory.value / "gamedata").toSet).toSeq
      }.taskValue
    )
    .settings(
      logo := rawLogo + "(v" + version.value.toString + ")",
      usefulTasks := Seq(
        UsefulTask("r", "runGame", "Run the game (requires Electron)"),
        UsefulTask("b", "buildGame", "Build web version"),
        UsefulTask(
          "rf",
          "runGameFull",
          "Run the fully optimised game (requires Electron)"
        ),
        UsefulTask(
          "bf",
          "buildGameFull",
          "Build the fully optimised web version"
        ),
        UsefulTask("p", "publishGame", "Publish game to ghpages"),
        UsefulTask("c", "code", "Launch VSCode")
      ),
      logoColor        := scala.Console.YELLOW,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )
    .enablePlugins(GhpagesPlugin) // Website stuff
    .settings(
      siteSourceDirectory      := target.value / "indigoBuildFull",
      makeSite / includeFilter := "*",
      makeSite / excludeFilter := ".DS_Store",
      git.remoteRepo           := "git@github.com:davesmith00000/roguelike.git",
      ghpagesNoJekyll          := true
    )

// To use indigoBuild or indigoRun, first comment out the line above that says: `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }`
addCommandAlias("runGame", ";compile;fastOptJS;indigoRun")
addCommandAlias("runGameFull", ";compile;fullOptJS;indigoRunFull")
addCommandAlias("buildGame", ";compile;fastOptJS;indigoBuild")
addCommandAlias("buildGameFull", ";compile;fullOptJS;indigoBuildFull")

addCommandAlias(
  "publishGame",
  List(
    "scalafmtCheckAll",
    "buildGameFull",
    "makeSite",
    "ghpagesPushSite"
  ).mkString(";", ";", "")
)

lazy val code =
  taskKey[Unit]("Launch VSCode in the current directory")

// format: off
lazy val rawLogo: String =
"""
                                                   .__        
  _____ ___.__.    ____   ____   ____   ___________|__| ____  
 /     <   |  |   / ___\_/ __ \ /    \_/ __ \_  __ \  |/ ___\ 
|  Y Y  \___  |  / /_/  >  ___/|   |  \  ___/|  | \/  \  \___ 
|__|_|  / ____|  \___  / \___  >___|  /\___  >__|  |__|\___  >
      \/\/      /_____/      \/     \/     \/              \/ 
                                   .__  .__ __                
_______  ____   ____  __ __   ____ |  | |__|  | __ ____       
\_  __ \/  _ \ / ___\|  |  \_/ __ \|  | |  |  |/ // __ \      
 |  | \(  <_> ) /_/  >  |  /\  ___/|  |_|  |    <\  ___/      
 |__|   \____/\___  /|____/  \___  >____/__|__|_ \\___  >     
             /_____/             \/             \/    \/      
"""
