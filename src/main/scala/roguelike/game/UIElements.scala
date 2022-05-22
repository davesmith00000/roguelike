package roguelike.game

import indigo.*
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.ColorScheme
import roguelike.RogueLikeGame
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
import roguelike.model.gamedata.KeyMapping
import roguelike.model.gamedata.Melee

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

object UIElements:

  def formatStatus(player: Player): String =
    s"HP: ${Math.max(0, player.fighter.hp)}/${player.fighter.maxHp}"

  val healthStatusLine: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(1, 2)

  val dungeonLevelLine: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(1, 2)

  val toolTipNeutral: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA(0.3, 0.3, 0.3, 1.0))
    )
  val toolTipAlive: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Blue)
    )
  val toolTipDead: Text[TerminalText] =
    Text(
      "",
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Red)
    )

  val letters: Batch[String]             = ('a' to 'z').toBatch.map(_.toString)
  val letterPositions: Map[String, Int] = letters.zipWithIndex.toMap

  def renderBar(player: Player, totalWidth: Int, position: Point): Group =
    val height = RogueLikeGame.charSize.height + 4
    val width  = RogueLikeGame.charSize.width * totalWidth
    val barWidth =
      (player.fighter.hp.toFloat / player.fighter.maxHp.toFloat * width).toInt

    Group(
      Shape.Box(
        Rectangle(0, 0, width, height),
        Fill.Color(ColorScheme.barEmpty.toRGBA)
      ),
      Shape.Box(
        Rectangle(0, 0, barWidth, height),
        Fill.Color(ColorScheme.barFilled.toRGBA)
      ),
      healthStatusLine.withText(formatStatus(player))
    ).moveTo(position)

  def renderMiniMap(vpSize: Size, miniMap: MiniMap): Group =
    val miniMapSize = miniMap.size * 2
    val miniMapBounds =
      Rectangle(miniMapSize)

    Group(
      miniMap,
      Shape.Box(miniMapBounds, Fill.None, Stroke(1, RGBA.White))
    ).moveTo(vpSize.toPoint - miniMapBounds.size.toPoint - Point(5))

  def renderLevel(position: Point, currentFloor: Int): Text[TerminalText] =
    healthStatusLine
      .withText("Dungeon level: " + currentFloor.toString)
      .moveTo(position)

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
      if player.equipment.powerBonus == 0 then ""
      else s" + ${player.equipment.powerBonus}"

    val defenseBonus: String =
      if player.equipment.defenseBonus == 0 then ""
      else s" + ${player.equipment.defenseBonus}"

    val text =
      s"""Level: ${player.level}
      |XP: ${player.xp}
      |XP for next level: ${player.experienceToNextLevel}
      |Attack: ${player.fighter.power}$powerBonus
      |Defense: ${player.fighter.defense}$defenseBonus
      |Weapon: ${player.equipment.weapon.map(_.name).getOrElse("--none--")}
      |Armour: ${player.equipment.armour.map(_.name).getOrElse("--none--")}
      |""".stripMargin

    Text(
      text,
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
    )
      .moveTo(5, 5)

  def renderControls(
      viewportSize: Size,
      helpControlsText: String
  ): Text[TerminalText] =
    Text(
      helpControlsText,
      RoguelikeTiles.Size10x10.Fonts.fontKey,
      TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
    ).alignRight
      .moveTo(viewportSize.width - 5, 5)

  def quitMenu(gameState: GameState, viewportSize: Size): Group =
    gameState match
      case GameState.Quit =>
        val text =
          s"""Quit to main menu?
          |
          |[1] Save
          |[2] Save and Quit
          |[3] Quit
          |""".stripMargin

        val windowSize = Size(300, 150)

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Blue)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case _ =>
        Group.empty

  def levelUpMenu(
      gameState: GameState,
      playerFigher: Fighter,
      viewportSize: Size
  ): Group =
    gameState match
      case GameState.LevelUp =>
        val text =
          s"""Level Up!
          |
          |Congratulations! You level up!
          |Select an attribute to increase.
          |[1] Constitution (+20 HP, from ${playerFigher.maxHp})
          |[2] Strength (+1 attack, from ${playerFigher.power})
          |[3] Agility (+1 defense, from ${playerFigher.defense})
          |""".stripMargin

        val windowSize = Size(350, 100)

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Magenta)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case _ =>
        Group.empty

  def historyViewer(
      gameState: GameState,
      messageLog: MessageLog,
      historyClones: TerminalClones,
      viewportSize: Size
  ): SceneUpdateFragment =
    gameState match
      case GameState.History =>
        val windowSize = Size(500, 310)

        SceneUpdateFragment(
          Group(
            Shape.Box(
              Rectangle(Point.zero, windowSize),
              Fill.Color(RGBA.Black),
              Stroke(2, RGBA.Orange)
            ) :: historyClones.clones
          ).moveTo(((viewportSize - windowSize) / 2).toPoint)
        ).addCloneBlanks(historyClones.blanks)

      case _ =>
        SceneUpdateFragment.empty

  def inventory(
      gameState: GameState,
      inventory: Inventory,
      equipment: Equipment,
      viewportSize: Size
  ): Group =
    val windowSize = Size(350, 200)

    gameState match
      case GameState.Inventory if inventory.items.length > 0 =>
        val items: String =
          inventory.items
            .map { case item =>
              item.name
            }
            .zip(letters)
            .foldLeft(("", 0)) { case ((str, r), (collectableName, letter)) =>
              (str + s"[$letter] $collectableName\n", r + 1)
            }
            ._1

        val text =
          "Select an item to use\n" + items

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Green)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case GameState.Inventory =>
        val text =
          s"""Select an item to use
          |
          |(Empty)
          |""".stripMargin

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Green)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case _ =>
        Group.empty

  def equipMenu(
      gameState: GameState,
      equipment: Equipment,
      viewportSize: Size
  ): Group =
    gameState match
      case GameState.Equipment =>
        val windowSize = Size(350, 200)

        val collectables: String =
          List(
            equipment.armour
              .map(a => "[a] " + a.name)
              .getOrElse("[ ] --No armour equipped--"),
            equipment.weapon
              .map(m => "[b] " + m.name)
              .getOrElse("[ ] --No melee weapon equipped--")
          ).mkString("\n")

        val text =
          "Select an item to unequip\n" + collectables

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Cyan)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case _ =>
        Group.empty

  def dropMenu(
      gameState: GameState,
      inventory: Inventory,
      viewportSize: Size
  ): Group =
    val windowSize = Size(350, 200)

    gameState match
      case GameState.Drop if inventory.items.length > 0 =>
        val collectables: String =
          inventory.items
            .map(_.name)
            .zip(letters)
            .foldLeft(("", 0)) { case ((str, r), (collectableName, letter)) =>
              (str + s"[$letter] $collectableName\n", r + 1)
            }
            ._1

        val text =
          "Select an collectable to drop\n" + collectables

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Red)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case GameState.Drop =>
        val text =
          s"""Select an collectable to drop
          |
          |(Empty)
          |""".stripMargin

        Group(
          Shape.Box(
            Rectangle(Point.zero, windowSize),
            Fill.Color(RGBA.Black),
            Stroke(2, RGBA.Red)
          ),
          Text(
            text,
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          )
            .moveTo(5, 5)
        ).moveTo(((viewportSize - windowSize) / 2).toPoint)

      case _ =>
        Group.empty
