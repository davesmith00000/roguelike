package roguelike

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import indigo.syntax.animations.*
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
import roguelike.assets.GameAssetsNew
import roguelike.model.GameLoadInfo
import roguelike.model.LoadingState
import roguelike.model.Model
import roguelike.model.ModelSaveData
import roguelike.viewmodel.ViewModel

object LogoScene extends Scene[Size, Model, ViewModel]:

  type SceneModel     = Unit
  type SceneViewModel = Unit

  val name: SceneName =
    SceneName("logo scene")

  val modelLens: Lens[Model, Unit] =
    Lens.unit

  val viewModelLens: Lens[ViewModel, Unit] =
    Lens.unit

  val eventFilters: EventFilters =
    EventFilters.Permissive

  val subSystems: Set[SubSystem] =
    Set()

  def updateModel(
      context: SceneContext[Size],
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    case KeyboardEvent.KeyUp(Key.SPACE) =>
      // Hit space to skip!!!
      Outcome(model)
        .addGlobalEvents(
          SceneEvent.JumpTo(LoadingScene.name)
        )

    case _ =>
      Outcome(model)

  def updateViewModel(
      context: SceneContext[Size],
      model: Unit,
      viewModel: Unit
  ): GlobalEvent => Outcome[Unit] =
    _ => Outcome(viewModel)

  def present(
      context: SceneContext[Size],
      model: Unit,
      viewModel: Unit
  ): Outcome[SceneUpdateFragment] =
    val screenCenter = context.startUpData.toPoint / 2

    val multiplier      = 0.3
    val firstLogoDelay  = 1.5.seconds
    val secondLogoDelay = firstLogoDelay + (LogoAnim.AnimDuration * multiplier)

    val logo1: Batch[Outcome[Graphic[Material.ImageEffects]]] =
      LogoAnim
        .logo(firstLogoDelay, screenCenter, false, multiplier)
        .at(context.running - context.sceneStartTime)(Outcome(Logos.indigo))
        .toBatch

    val logo2: Batch[Outcome[Graphic[Material.ImageEffects]]] =
      LogoAnim
        .logo(secondLogoDelay, screenCenter, true, multiplier)
        .at(context.running - context.sceneStartTime)(Outcome(Logos.pkg))
        .toBatch

    (logo1 ++ logo2).sequence.map(SceneUpdateFragment(_))

object LogoAnim:

  val AnimDuration: Seconds = 6.seconds

  def logo(
      delay: Seconds,
      screenCenter: Point,
      emitEventOnComplete: Boolean,
      multiplier: Double
  ): Timeline[Outcome[Graphic[Material.ImageEffects]]] =
    timeline(
      layer(
        startAfter(delay),                             // Pause
        animate(1.seconds * multiplier) { sceneNode => // Fade in
          lerp >>> SignalFunction { d =>
            sceneNode.map(_.moveTo(screenCenter).modifyMaterial(_.withAlpha(d)))
          }
        },
        show(3.seconds * multiplier) { sceneNode => // Show logo
          sceneNode.map(_.moveTo(screenCenter).modifyMaterial(_.withAlpha(1.0)))
        },
        animate(1.seconds * multiplier) { sceneNode => // Fade out
          lerp >>> SignalFunction { d =>
            sceneNode.map(
              _.moveTo(screenCenter).modifyMaterial(_.withAlpha(1.0 - d))
            )
          }
        },
        pause(1.second * multiplier),  // wait
        show(1.seconds * multiplier) { // wait and then optionally fire an event.
          _.addGlobalEvents(
            if emitEventOnComplete then Batch(SceneEvent.JumpTo(LoadingScene.name))
            else Batch.empty
          )
        }
      )
    )

object Logos:

  val pkg: Graphic[Material.ImageEffects] =
    val size = Size(100, 136)

    Graphic(
      size,
      GameAssetsNew.assets.init.pkLogoMaterial
        .stretch
        .toImageEffects
    ).withRef(size.toPoint / 2)
      .modifyMaterial(_.withAlpha(0.0))

  val indigo: Graphic[Material.ImageEffects] =
    val size = Size(100, 105)

    Graphic(
      size,
      GameAssetsNew.assets.init.indigoLogoMaterial
        .stretch
        .toImageEffects
    ).withRef(size.toPoint / 2)
      .modifyMaterial(_.withAlpha(0.0))
