package roguelike.game

import indigo.*
import indigo.syntax.*
import roguelike.ColorScheme
import roguelike.RogueLikeGame
import roguelike.assets.GameAssets
import roguelike.model.Equipment
import roguelike.model.GameState
import roguelike.model.Inventory
import roguelike.model.MessageLog
import roguelike.model.entity.Collectable
import roguelike.model.entity.Entity
import roguelike.model.entity.Fighter
import roguelike.model.entity.Hostile
import roguelike.model.entity.Player
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Melee
import roguelikestarterkit.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object UIElements:

  val dungeonLevelLine: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
    )
      .moveTo(1, 2)

  val toolTipNeutral: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA(0.3, 0.3, 0.3, 1.0))
    )
  val toolTipAlive: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Blue)
    )
  val toolTipDead: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Red)
    )

  val letters: Batch[String]            = ('a' to 'z').toBatch.map(_.toString)
  val letterPositions: Map[String, Int] = letters.zipWithIndex.toMap

  def renderNameHints(
      squareSize: Point,
      mousePosition: Point,
      entities: js.Array[Entity],
      stairsPosition: Point,
      hoverSquare: Point
  ): Group =
    val offset       = mousePosition + Point(10)
    val squareHeight = squareSize.y

    val tips =
      entities
        .filter(_.position == hoverSquare)
        .zipWithIndex
        .map {
          case (entity: Hostile, row) =>
            val tt = if entity.isAlive then toolTipAlive else toolTipDead
            tt.withText(entity.name.capitalize)
              .moveTo(Point(0, row * squareHeight) + offset)

          case (entity, row) =>
            val tt = toolTipNeutral
            tt.withText(entity.name.capitalize)
              .moveTo(Point(0, row * squareHeight) + offset)
        }
        .toBatch

    val stairs =
      if hoverSquare == stairsPosition then
        Batch(
          toolTipNeutral
            .withText("Down stairs.")
            .moveTo(Point(0, tips.length * squareHeight) + offset)
        )
      else Batch.empty

    Group(tips ++ stairs)

  def renderAreaOfEffect(
      squareSize: Point,
      target: Point,
      gameState: GameState
  ): Group =
    gameState match
      case GameState.LookAround(0) =>
        Group.empty

      case GameState.LookAround(radius) =>
        val pos = target - (Point(radius) * squareSize)
        val size =
          if radius * 2 % 2 == 0 then Size((radius * 2) + 1)
          else Size(radius * 2)

        Group(
          Shape.Box(
            Rectangle(pos, size * squareSize.toSize),
            Fill.Color(RGBA.Green.withAlpha(0.5)),
            Stroke(2, RGBA.Green)
          )
        )

      case _ =>
        Group.empty

  def renderCharacterInfo(player: Player): Text[TerminalText] =
    val powerBonus: String =
      if player.inventory.equipment.powerBonus == 0 then ""
      else s" + ${player.inventory.equipment.powerBonus}"

    val defenseBonus: String =
      if player.inventory.equipment.defenseBonus == 0 then ""
      else s" + ${player.inventory.equipment.defenseBonus}"

    val text =
      s"""Level: ${player.level}
      |XP: ${player.xp}
      |XP for next level: ${player.experienceToNextLevel}
      |Attack: ${player.fighter.power}$powerBonus
      |Defense: ${player.fighter.defense}$defenseBonus
      |Weapon: ${player.inventory.equipment.weapon
          .map(_.name)
          .getOrElse("--none--")}
      |Armour: ${player.inventory.equipment.armour
          .map(_.name)
          .getOrElse("--none--")}
      |""".stripMargin

    Text(
      text,
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
    )
      .moveTo(5, 5)

  def renderControls(
      viewportSize: Size,
      helpControlsText: String
  ): Text[TerminalText] =
    Text(
      helpControlsText,
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(GameAssets.assets.init.AnikkiSquare10x10, RGBA.White, RGBA.Zero)
    ).alignRight
      .moveTo(viewportSize.width - 5, 5)

  // def renderShortLog(viewportSize: Size, shortLogClones: Batch[CloneTiles]) =
  //   val windowSize = Size(500, 60)

  //   Group(
  //     Shape.Box(
  //       Rectangle(Point.zero, windowSize),
  //       Fill.Color(RGBA.Black),
  //       Stroke(2, RGBA.SlateGray)
  //     ) :: shortLogClones
  //   ).moveTo(
  //     (viewportSize.width - windowSize.width) / 2,
  //     viewportSize.height - windowSize.height - 5
  //   )
