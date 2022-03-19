package roguelike.subsystems

import indigo._
import indigoextras.subsystems._
import io.indigoengine.roguelike.starterkit.terminal.TerminalText
import io.indigoengine.roguelike.starterkit.tiles.RoguelikeTiles
import roguelike.Assets
import roguelike.RogueLikeGame

object FloatingMessage:

  final case class Message(text: String, colour: RGB) extends AutomatonPayload

  private val poolKey = AutomataPoolKey("floating messages")

  def subSystem: Automata =
    Automata(
      poolKey,
      Automaton(
        AutomatonNode.Fixed(
          Text(
            "",
            RoguelikeTiles.Size10x10.Fonts.fontKey,
            TerminalText(Assets.Basic.tileMap, RGB.White, RGBA.Zero)
          ).alignCenter
        ),
        Seconds(1.5d)
      ).withModifier(ModiferFunctions.signal),
      RogueLikeGame.layerKeyUiOverlay
    )

  def spawnEvent(position: Point, message: Message): AutomataEvent =
    AutomataEvent.Spawn(
      poolKey,
      position,
      None,
      Some(message)
    )

  object ModiferFunctions:

    val multiplierS: AutomatonSeedValues => Signal[Double] =
      seed => Signal(seed.progression)

    val spawnPositionS: AutomatonSeedValues => Signal[Point] =
      seed => Signal.fixed(seed.spawnedAt)

    val positionSF: SignalFunction[(Double, Point), Point] =
      SignalFunction { case (multiplier, spawnedAt) =>
        spawnedAt + Point(0, -(30 * multiplier).toInt)
      }

    val newPosition: AutomatonSeedValues => Signal[Point] =
      seed =>
        Signal.product(multiplierS(seed), spawnPositionS(seed)) |> positionSF

    val signal
        : SignalReader[(AutomatonSeedValues, SceneNode), AutomatonUpdate] =
      SignalReader { case (seed, sceneGraphNode) =>
        sceneGraphNode match
          case t: Text[_] =>
            seed.payload match
              case Some(FloatingMessage.Message(text, colour)) =>
                newPosition(seed)
                  .map { case position =>
                    AutomatonUpdate(
                      t.moveTo(position)
                        .withText(text)
                        .modifyMaterial { case m: TerminalText =>
                          m.withForeground(colour)
                        }
                    )
                  }
              case _ =>
                Signal.fixed(AutomatonUpdate(sceneGraphNode))

          case _ =>
            Signal.fixed(AutomatonUpdate.empty)
      }

  end ModiferFunctions
