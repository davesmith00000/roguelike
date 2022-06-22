package roguelike.game

import indigo.*
import indigo.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.Assets
import roguelike.GameGraphics
import roguelike.RogueLikeGame
import roguelike.components.entities.HostilesManager
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
        .map { p =>
          val pos = (p._2 * viewModel.squareSize) - (viewModel.squareSize / 2)
          p._1 match
            case GameTile.Wall if model.gameMap.visible.contains(p._2) =>
              GameGraphics.wallTile(pos)

            case GameTile.Wall =>
              GameGraphics.wallInShadowTile(pos)

            case GameTile.Ground if model.gameMap.visible.contains(p._2) =>
              GameGraphics.floorTile(pos)

            case GameTile.Ground =>
              GameGraphics.floorInShadowTile(pos)

            case GameTile.DownStairs if model.gameMap.visible.contains(p._2) =>
              GameGraphics.stairsTile(pos)

            case GameTile.DownStairs =>
              GameGraphics.stairsInShadowTile(pos)
        }

    val gameGrid: Batch[CloneTiles] =
      Batch(CloneTiles(GameGraphics.tileClone.id, gameGridData))

    val hover =
      Batch(
        GameGraphics.highlight
          .moveTo(
            (viewModel.hoverSquare * viewModel.squareSize) - (viewModel.squareSize / 2)
          )
      )

    val collectables: Batch[SceneNode] =
      viewModel.collectables.toBatch.map { collectable =>
        val pos = (collectable.position * viewModel.squareSize) - (viewModel.squareSize / 2)
        collectable.item match
          case Ranged.LightningScroll =>
            GameGraphics.lightningScroll
              .moveTo(pos)

          case Ranged.FireballScroll =>
            GameGraphics.fireScroll
              .moveTo(pos)

          case Ranged.ConfusionScroll =>
            GameGraphics.confusionScroll
              .moveTo(pos)

          // TODO: Scroll in shadow - currently not in list as not visible.

          case Consumables.HealthPotion =>
            GameGraphics.healthPotion
              .moveTo(pos)

          case Armour.LeatherArmor =>
            GameGraphics.leatherArmour
              .moveTo(pos)

          case Armour.ChainMail =>
            GameGraphics.chainmail
              .moveTo(pos)

          case Melee.Dagger =>
            GameGraphics.dagger
              .moveTo(pos)

          case Melee.Sword =>
            GameGraphics.sword
              .moveTo(pos)
      }

    val lookAround: Batch[SceneNode] =
      val pos = viewModel.lookAtPosition
      model.gameState match
        case GameState.LookAround(_) =>
          Batch(
            GameGraphics.target.moveTo(
              pos
            ),
            UIElements.renderAreaOfEffect(
              viewModel.squareSize,
              pos,
              model.gameState
            )
          )

        case _ =>
          Batch.empty

    val hostiles =
      HostilesManager.view(
        context,
        HostilesManager.modelLens.get(model),
        HostilesManager.viewModelLens.get(viewModel)
      )

    val camera =
      model.gameState match
        case GameState.LookAround(_) =>
          val offset =
            (viewModel.viewportSize.toPoint / viewModel.magnification) / 2
          Camera.Fixed(
            viewModel.lookAtPosition - offset + (viewModel.squareSize / 2)
          )

        case _ =>
          val offset =
            (viewModel.viewportSize.toPoint / viewModel.magnification) / 2
          Camera.Fixed(viewModel.playerPosition.display(viewModel.squareSize) - offset)

    SceneUpdateFragment(
      Layer(
        RogueLikeGame.layerKeyGame,
        gameGrid ++
          hover ++
          collectables ++
          hostiles ++
          PlayerComponent.present(context, model, viewModel) ++
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
            model.entitiesList,
            model.stairsPosition,
            viewModel.hoverSquare
          )
        ) ++ WindowManager.present(context, model, viewModel)
      )
    ).addCloneBlanks(
      GameGraphics.tileClone :: viewModel.terminals.history.blanks
    )
