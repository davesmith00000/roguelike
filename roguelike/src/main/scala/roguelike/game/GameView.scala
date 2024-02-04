package roguelike.game

import indigo.*
import indigo.scenes.SceneContext
import indigo.shared.materials.Material.Bitmap
import indigo.syntax.*
import roguelike.RogueLikeGame
import roguelike.assets.GameAssets
import roguelike.assets.GameGraphics
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
import roguelike.shaders.Hover
import roguelike.viewmodel.GameViewModel
import roguelike.viewmodel.ViewModel
import roguelikestarterkit.*

object GameView:

  def present(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    for {
      gl <- drawGameLayer(context, model, viewModel)
      ul <- drawUiLayer(context, model, viewModel)
    } yield gl |+| ul

  def drawGameLayer(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =

    val (visible, invisible) = viewModel.tiles
      .map { p =>
        val pos = (p._2 * viewModel.squareSize) - (viewModel.squareSize / 2)
        (p._1, pos, model.gameMap.visible.contains(p._2))
      }
      .partition(_._3)

    val gameGridData: Batch[CloneTileData] =
      visible.map {
        case (GameTile.Wall(code), pos, _)    => GameGraphics.wallTile(code, pos)
        case (GameTile.Ground(style), pos, _) => GameGraphics.floorTile(style, pos)
        case (GameTile.DownStairs, pos, _)    => GameGraphics.stairsTile(pos)
      }

    val gameGridShadowData: Batch[CloneTileData] =
      invisible.map {
        case (GameTile.Wall(code), pos, _)    => GameGraphics.wallTile(code, pos)
        case (GameTile.Ground(style), pos, _) => GameGraphics.floorTile(style, pos)
        case (GameTile.DownStairs, pos, _)    => GameGraphics.stairsTile(pos)
      }

    val gameGrid: Batch[CloneTiles] =
      Batch(
        CloneTiles(GameGraphics.tileShadowClone.id, gameGridShadowData),
        CloneTiles(GameGraphics.tileClone.id, gameGridData)
      )

    val hover =
      Batch(
        Hover.view
          .moveTo(
            (viewModel.hoverSquare * viewModel.squareSize) - (viewModel.squareSize / 2) + Point(2)
          )
      )

    val collectables: Batch[SceneNode] =
      viewModel.collectables.toBatch.map { collectable =>
        val pos =
          (collectable.position * viewModel.squareSize) - (viewModel.squareSize / 2)
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

    val camera =
      model.gameState match
        case GameState.LookAround(_) =>
          Camera.LookAt(
            viewModel.lookAtPosition + (viewModel.squareSize / 2)
          )

        case _ =>
          Camera.LookAt(
            viewModel.playerPosition.moving(viewModel.squareSize)
          )

    for {
      plr <- PlayerComponent.present(context, model, viewModel)
      hostiles <- HostilesManager.view(
        context,
        HostilesManager.modelLens.get(model),
        HostilesManager.viewModelLens.get(viewModel)
      )
    } yield SceneUpdateFragment(
      Layer(
        RogueLikeGame.layerKeyGame,
        gameGrid ++
          hover ++
          collectables ++
          hostiles ++
          plr ++
          lookAround
      ).withCamera(camera)
        .withMagnification(viewModel.magnification),
      Layer(RogueLikeGame.layerKeyUiOverlay)
        .withCamera(camera)
        .withMagnification(viewModel.magnification)
    )

  def drawUiLayer(
      context: SceneContext[Size],
      model: Model,
      viewModel: GameViewModel
  ): Outcome[SceneUpdateFragment] =
    val vpSize = viewModel.viewportSize

    WindowManager.present(context, model, viewModel).map { windows =>
      SceneUpdateFragment(
        Layer(
          RogueLikeGame.layerKeyUi,
          Batch(
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
            ),
            UIElements.renderShortLog(
              vpSize,
              viewModel.terminals.shortLog.clones
            )
          ) ++ windows
        )
      ).addCloneBlanks(
        GameGraphics.tileClone :: GameGraphics.tileShadowClone :: viewModel.terminals.history.blanks ++ viewModel.terminals.shortLog.blanks
      )
    }
