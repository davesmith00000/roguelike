package roguelike

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import indigo.syntax.animations.*
import indigoextras.subsystems.AssetBundleLoader
import indigoextras.subsystems.AssetBundleLoaderEvent
import io.indigoengine.roguelike.starterkit.*
import roguelike.assets.GameAssets
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

    val anims: Batch[Outcome[Graphic[Material.ImageEffects]]] =
      logo(1.seconds, screenCenter, false)
        .at(context.running - context.sceneStartTime)(Outcome(Logos.indigo))
        .toBatch ++
        logo(5.5.seconds, screenCenter, true)
          .at(context.running - context.sceneStartTime)(Outcome(Logos.pkg))
          .toBatch

    anims.sequence.map { a =>
      SceneUpdateFragment(a)
    }

  def logo(
      delay: Seconds,
      screenCenter: Point,
      emitEventOnComplete: Boolean
  ): Timeline[Outcome[Graphic[Material.ImageEffects]]] =
    timeline(
      layer(
        startAfter(delay),
        animate(1.seconds) { logo =>
          lerp >>> SignalFunction { d =>
            logo.map(_.moveTo(screenCenter).modifyMaterial(_.withAlpha(d)))
          }
        },
        show(2.seconds) { logo =>
          logo.map(_.moveTo(screenCenter))
        },
        animate(1.seconds) { logo =>
          lerp >>> SignalFunction { d =>
            logo.map(
              _.moveTo(screenCenter).modifyMaterial(_.withAlpha(1.0 - d))
            )
          }
        },
        pause(0.5.second),
        show(0.5.seconds) { logo =>
          logo
            .map(_.modifyMaterial(_.withAlpha(0.0)))
            .addGlobalEvents(
              if emitEventOnComplete then
                Batch(SceneEvent.JumpTo(LoadingScene.name))
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
        Material
          .Bitmap(GameAssets.PurpleKingdomLogo)
          .stretch
          .toImageEffects
      ).withRef(size.toPoint / 2)

    val indigo: Graphic[Material.ImageEffects] =
      val size = Size(100, 105)

      Graphic(
        size,
        Material
          .Bitmap(GameAssets.IndigoLogo)
          .stretch
          .toImageEffects
      ).withRef(size.toPoint / 2)
