package roguelike.scenes

import indigo.*
import indigo.syntax.*
import roguelike.assets.GameAssets
import roguelike.model.GameState
import roguelike.model.entity.Entity
import roguelike.model.entity.Hostile
import roguelikestarterkit.*

import scala.scalajs.js

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
