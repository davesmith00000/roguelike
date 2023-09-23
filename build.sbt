import scala.sys.process._
import scala.language.postfixOps

import sbtwelcome._
import indigoplugin._

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalafixDependencies += "com.github.liancheng" %% "organize-imports" % "0.5.0"

lazy val commonSettings: Seq[sbt.Def.Setting[_]] = Seq(
  version      := "0.0.1",
  organization := "purplekingdomgames",
  scalaVersion := "3.3.1",
  libraryDependencies ++= Seq(
    "org.scalameta" %%% "munit" % "0.7.29" % Test
  ),
  testFrameworks += new TestFramework("munit.Framework"),
  scalafixOnCompile := true,
  semanticdbEnabled := true,
  semanticdbVersion := scalafixSemanticdb.revision,
  autoAPIMappings   := true
)

val indigoVersion              = "0.15.0-RC4"
val roguelikeStarterKitVersion = "0.3.0-RC3"

lazy val indigoDeps: Seq[sbt.Def.Setting[_]] = Seq(
  libraryDependencies ++= Seq(
    "io.indigoengine" %%% "indigo-json-circe"    % indigoVersion,
    "io.indigoengine" %%% "indigo"               % indigoVersion,
    "io.indigoengine" %%% "indigo-extras"        % indigoVersion,
    "io.indigoengine" %%% "roguelike-starterkit" % roguelikeStarterKitVersion
  )
)

lazy val roguelikeProject =
  (project in file("."))
    .aggregate(
      roguelike,
      roguelikeGenerated,
      dungeonGenerator,
      dungeonViewer,
      roguelikeShared
    )
    .settings(commonSettings)
    .settings(
      name := "roguelike-game"
    )
    .settings(
      logo := rawLogo + "(v" + version.value.toString + ")",
      usefulTasks := Seq(
        UsefulTask("runGame", "Run the game (requires Electron)").noAlias,
        UsefulTask("runViewer", "Run the Dungeon Viewer (requires Electron)").noAlias,
        UsefulTask("buildGame", "Build web version").noAlias,
        UsefulTask(
          "runGameFull",
          "Run the fully optimised game (requires Electron)"
        ).noAlias,
        UsefulTask(
          "buildGameFull",
          "Build the fully optimised web version"
        ).noAlias,
        UsefulTask("publishGame", "Publish game to ghpages").noAlias,
        UsefulTask("code", "Launch VSCode").noAlias
      ),
      logoColor        := scala.Console.YELLOW,
      aliasColor       := scala.Console.BLUE,
      commandColor     := scala.Console.CYAN,
      descriptionColor := scala.Console.WHITE
    )
    .settings(
      code := { "code ." ! }
    )

lazy val roguelikeGenerated =
  (project in file("roguelike-generated"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings)
    .settings(indigoDeps)
    .settings(
      Compile / sourceGenerators += Def.task {
        IndigoGenerators
          .sbt((Compile / sourceManaged).value, "roguelike.assets")
          .listAssets("GeneratedAssets", roguelikeOptions.assets)
          .embedAseprite("AsepriteDeath", os.pwd / "assets" / "game" / "death.json")
          .embedAseprite("AsepriteEnemy1", os.pwd / "assets" / "game" / "enemy1.json")
          .embedAseprite("AsepriteEnemy2", os.pwd / "assets" / "game" / "ENEMY2.json")
          .embedAseprite("AsepriteEnemy3", os.pwd / "assets" / "game" / "ENEMY3.json")
          .embedAseprite(
            "AsepriteKeyAnimation",
            os.pwd / "assets" / "game" / "Key_animation.json"
          )
          .embedAseprite("AsepritePlayer", os.pwd / "assets" / "game" / "Player.json")
          .embedAseprite(
            "AsepritePotionAnimation",
            os.pwd / "assets" / "game" / "Potion_animation.json"
          )
          .embedAseprite("AsepriteShadows", os.pwd / "assets" / "game" / "shadows.json")
          .embedAseprite(
            "AsepriteTorchAnimation",
            os.pwd / "assets" / "game" / "torch_animation.json"
          )
          .embedGLSLShaders(
            "ShaderInnerGlow",
            os.pwd / "assets" / "shaders" / "noop.vert",
            os.pwd / "assets" / "shaders" / "innerglow.frag",
            false
          )
          .embedGLSLShaders(
            "ShaderText",
            os.pwd / "assets" / "shaders" / "noop.vert",
            os.pwd / "assets" / "shaders" / "text.frag",
            false
          )
          .toSourceFiles
      },
      Compile / sourceGenerators += Def.task {
        IndigoGenerators
          .sbt((Compile / sourceManaged).value, "roguelike.model.gamedata")
          .embedMarkdownTable
          .asEnum("Armour", os.pwd / "roguelike-generated" / "gamedata" / "armour.md", "roguelike.model.items.Item")
          .embedMarkdownTable
          .asEnum("Consumables", os.pwd / "roguelike-generated" / "gamedata" / "consumables.md", "roguelike.model.items.Item")
          .embedMarkdownTable
          .asEnum("Hostiles", os.pwd / "roguelike-generated" / "gamedata" / "hostiles.md")
          // .embedMarkdownTable
          // .asEnum("KeyMapping", os.pwd / "roguelike-generated" / "gamedata" / "key-mappings.md")
          .embedMarkdownTable
          .asEnum("Melee", os.pwd / "roguelike-generated" / "gamedata" / "melee.md", "roguelike.model.items.Item")
          .embedMarkdownTable
          .asEnum("Ranged", os.pwd / "roguelike-generated" / "gamedata" / "ranged.md", "roguelike.model.items.Item")
          .toSourceFiles
      }
    )
    .settings(
      Compile / sourceGenerators += Def.task {
        val cachedFun = FileFunction.cached(
          streams.value.cacheDirectory / "gamedata"
        ) { (gameData: Set[File]) =>
          val outDir = (Compile / sourceManaged).value
            KeyMappingsGen
              .gen(
                "KeyMapping",
                "roguelike.model.gamedata",
                gameData,
                outDir
              )
        }
        cachedFun(IO.listFiles(baseDirectory.value / "gamedata").toSet).toSeq
      }.taskValue
    )

lazy val roguelikeShared =
  (project in file("roguelike-shared"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings)
    .settings(indigoDeps)
    .dependsOn(roguelikeGenerated)

lazy val dungeonGenerator =
  (project in file("dungeon-generator"))
    .enablePlugins(ScalaJSPlugin)
    .settings(commonSettings)
    .settings(indigoDeps)
    .dependsOn(roguelikeShared)

lazy val dungeonViewerOptions: IndigoOptions =
  IndigoOptions.defaults
    .withTitle("Dungeon Viewer")
    .withAssetDirectory("dungeon-viewer/assets")
    .withWindowSize(800, 500)
    .withBackgroundColor("black")

lazy val dungeonViewer =
  (project in file("dungeon-viewer"))
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings(commonSettings)
    .settings(
      name := "dungeon-viewer",
      Test / scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      },
      indigoOptions := dungeonViewerOptions,
      Compile / sourceGenerators += Def.task {
        IndigoGenerators
          .sbt((Compile / sourceManaged).value, "dungeonviewer.generated")
          .listAssets("Assets", dungeonViewerOptions.assets)
          .generateConfig("Config", dungeonViewerOptions)
          .toSourceFiles
      }
    )
    .settings(indigoDeps)
    .dependsOn(dungeonGenerator)

lazy val roguelikeOptions: IndigoOptions =
  IndigoOptions.defaults
    .withTitle("My Generic Roguelike")
    .withWindowSize(1280, 720)
    .withBackgroundColor("#21293f")
    .excludeAssetPaths {
      case p if p.endsWith(".json")     => true
      case p if p.startsWith("shaders") => true
    }

lazy val roguelike =
  project
    .enablePlugins(ScalaJSPlugin, SbtIndigo)
    .settings(commonSettings)
    .settings(
      name := "roguelike",
      Test / scalaJSLinkerConfig ~= {
        _.withModuleKind(ModuleKind.CommonJSModule)
      },
      indigoOptions := roguelikeOptions,
      Compile / sourceGenerators += Def.task {
        IndigoGenerators
          .sbt((Compile / sourceManaged).value, "roguelike.config")
          .generateConfig("Config", roguelikeOptions)
          .toSourceFiles
      }
    )
    .settings(indigoDeps)
    .enablePlugins(GhpagesPlugin) // Website stuff
    .settings(
      siteSourceDirectory      := target.value / "indigoBuildFull",
      makeSite / includeFilter := "*",
      makeSite / excludeFilter := ".DS_Store",
      git.remoteRepo           := "git@github.com:davesmith00000/roguelike.git",
      ghpagesNoJekyll          := true
    )
    .dependsOn(dungeonGenerator)

// To use indigoBuild or indigoRun, first comment out the line above that says: `scalaJSLinkerConfig ~= { _.withModuleKind(ModuleKind.CommonJSModule) }`
addCommandAlias(
  "runGame",
  List(
    "roguelike/compile",
    "roguelike/fastLinkJS",
    "roguelike/indigoRun"
  ).mkString(";", ";", "")
)
addCommandAlias(
  "runGameFull",
  List(
    "roguelike/compile",
    "roguelike/fullOptJS",
    "roguelike/indigoRunFull"
  ).mkString(";", ";", "")
)
addCommandAlias(
  "buildGame",
  List(
    "roguelike/compile",
    "roguelike/fastLinkJS",
    "roguelike/indigoBuild"
  ).mkString(";", ";", "")
)
addCommandAlias(
  "buildGameFull",
  List(
    "roguelike/compile",
    "roguelike/fullOptJS",
    "roguelike/indigoBuildFull"
  ).mkString(";", ";", "")
)
addCommandAlias(
  "runViewer",
  List(
    "dungeonViewer/compile",
    "dungeonViewer/fastLinkJS",
    "dungeonViewer/indigoRun"
  ).mkString(";", ";", "")
)
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
