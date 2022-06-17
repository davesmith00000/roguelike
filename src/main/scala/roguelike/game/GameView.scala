package roguelike.game

import indigo.*
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.GameGraphics
import roguelike.RogueLikeGame
import roguelike.components.entities.PlayerComponent
import roguelike.components.windows.WindowManager
import roguelike.model.GameState
import roguelike.model.GameTile
import roguelike.model.Model
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel

object GameView:

  def present(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    Outcome(
      drawGameLayer(context, model, viewModel) |+| drawUiLayer(
        context,
        model,
        viewModel
      )
    )

  def drawGameLayer(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): SceneUpdateFragment =

    val gameGridData: Batch[CloneTileData] =
      viewModel.tiles
        .map(p =>
          p._1 match
            case GameTile.Wall if model.gameMap.visible.contains(p._2) =>
              GameGraphics.wallTile(p._2 * viewModel.squareSize)

            case GameTile.Wall =>
              GameGraphics.wallInShadowTile(p._2 * viewModel.squareSize)

            case GameTile.Ground if model.gameMap.visible.contains(p._2) =>
              GameGraphics.floorTile(p._2 * viewModel.squareSize)

            case GameTile.Ground =>
              GameGraphics.floorInShadowTile(p._2 * viewModel.squareSize)

            case GameTile.DownStairs if model.gameMap.visible.contains(p._2) =>
              GameGraphics.stairsTile(p._2 * viewModel.squareSize)

            case GameTile.DownStairs =>
              GameGraphics.stairsInShadowTile(p._2 * viewModel.squareSize)
        )
        .toBatch

    val gameGrid: Batch[CloneTiles] =
      Batch(CloneTiles(GameGraphics.tileClone.id, gameGridData))

    val hover =
      Batch(
        GameGraphics.highlight
          .moveTo(viewModel.hoverSquare * viewModel.squareSize)
      )

    val collectables: Batch[SceneNode] =
      viewModel.collectables.toBatch.map { collectable =>
        collectable.item match
          case Ranged.LightningScroll =>
            GameGraphics.lightningScroll
              .moveTo(collectable.position * viewModel.squareSize)

          case Ranged.FireballScroll =>
            GameGraphics.fireScroll
              .moveTo(collectable.position * viewModel.squareSize)

          case Ranged.ConfusionScroll =>
            GameGraphics.confusionScroll
              .moveTo(collectable.position * viewModel.squareSize)

          // TODO: Scroll in shadow - currently not in list as not visible.

          case Consumables.HealthPotion =>
            GameGraphics.healthPotion
              .moveTo(collectable.position * viewModel.squareSize)

          case Armour.LeatherArmor =>
            GameGraphics.leatherArmour
              .moveTo(collectable.position * viewModel.squareSize)

          case Armour.ChainMail =>
            GameGraphics.chainmail
              .moveTo(collectable.position * viewModel.squareSize)

          case Melee.Dagger =>
            GameGraphics.dagger
              .moveTo(collectable.position * viewModel.squareSize)

          case Melee.Sword =>
            GameGraphics.sword
              .moveTo(collectable.position * viewModel.squareSize)
      }

    val hostilesAndHealthBars: List[(SceneNode, List[SceneNode])] =
      viewModel.hostiles.map { hostile =>
        val color =
          hostile match
            case h: Orc if h.isAlive   => RGBA(0.2, 0.8, 0.0, 1.0)
            case _: Orc                => RGBA(1.0, 0.1, 0.1, 0.75)
            case h: Troll if h.isAlive => RGBA(0.3, 0.6, 0.0, 1.0)
            case _: Troll              => RGBA(0.1, 0.2, 1.0, 0.75)

        val size: Int =
          hostile match
            case h: Orc if h.isAlive   => (viewModel.squareSize.x * 0.25).toInt
            case _: Orc                => (viewModel.squareSize.x * 0.35).toInt
            case h: Troll if h.isAlive => (viewModel.squareSize.x * 0.4).toInt
            case _: Troll              => (viewModel.squareSize.x * 0.5).toInt

        val position = hostile.position * viewModel.squareSize

        val halfSquareWidth = viewModel.squareSize.x / 2

        val healthbar: List[SceneNode] =
          if !hostile.isAlive then Nil
          else
            List(
              Shape.Box(
                Rectangle(
                  position + Point(halfSquareWidth / 2, -6),
                  Size(halfSquareWidth, 5)
                ),
                Fill.Color(RGBA.Red),
                Stroke(1, RGBA.Black)
              ),
              Shape.Box(
                Rectangle(
                  position + Point(halfSquareWidth / 2, -6),
                  Size(
                    (halfSquareWidth * hostile.fighter.hpAsMultiplier).toInt,
                    5
                  )
                ),
                Fill.Color(RGBA.Green),
                Stroke(1, RGBA.Black)
              )
            )

        (
          Shape.Circle(
            position + halfSquareWidth,
            size,
            Fill.Color(color),
            Stroke(2, color.mix(RGBA.Black, 0.5))
          ),
          healthbar
        )
      }.toList

    val lookAround: Batch[SceneNode] =
      model.currentState match
        case GameState.LookAround(_) =>
          Batch(
            GameGraphics.target.moveTo(
              model.lookAtTarget * viewModel.squareSize
            ),
            UIElements.renderAreaOfEffect(
              viewModel.squareSize,
              model.lookAtTarget * viewModel.squareSize,
              model.currentState
            )
          )

        case _ =>
          Batch.empty

    val hostiles   = hostilesAndHealthBars.toBatch.map(_._1)
    val healthBars = hostilesAndHealthBars.map(_._2).flatten.toBatch

    val camera =
      model.currentState match
        case GameState.LookAround(_) =>
          val offset =
            (viewModel.viewportSize.toPoint / viewModel.magnification) / 2
          Camera.Fixed(viewModel.lookAtPosition - offset)

        case _ =>
          val offset =
            (viewModel.viewportSize.toPoint / viewModel.magnification) / 2
          Camera.Fixed(viewModel.playerPosition.display - offset)

    SceneUpdateFragment(
      Layer(
        RogueLikeGame.layerKeyGame,
        gameGrid ++
          hover ++
          collectables ++
          hostiles ++
          PlayerComponent.present(context, model, viewModel) ++
          healthBars ++
          lookAround
      ).withCamera(camera)
        .withMagnification(viewModel.magnification),
      Layer(RogueLikeGame.layerKeyUiOverlay)
        .withCamera(camera)
        .withMagnification(viewModel.magnification)
    )

  def drawUiLayer(
      context: FrameContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): SceneUpdateFragment =
    val vpSize = viewModel.viewportSize

    SceneUpdateFragment(
      Layer(
        RogueLikeGame.layerKeyUi,
        Batch(
          UIElements.renderMiniMap(vpSize, viewModel.miniMap),
          UIElements.renderBar(
            model.player,
            20,
            Point(0, vpSize.height - 40)
          ),
          UIElements.renderLevel(
            Point(0, vpSize.height - 20),
            model.currentFloor
          ),
          UIElements.renderCharacterInfo(model.player),
          UIElements.renderControls(
            vpSize,
            viewModel.helpControlsText
          ),
          UIElements.renderNameHints(
            viewModel.squareSize,
            context.mouse.position,
            model.gameMap.entitiesList,
            model.stairsPosition,
            viewModel.hoverSquare
          )
        ) ++ WindowManager.present(context, model, viewModel)
      )
    ).addCloneBlanks(
      GameGraphics.tileClone :: viewModel.terminals.history.blanks
    )
