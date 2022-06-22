package roguelike.viewmodel

import indigo._
import indigoextras.geometry.Vertex
import roguelike.GameEvent

/** ActorPosition is a view model helper to track the position actors should be
  * rendered at on the screen.
  * @param fromPosition
  *   The position in the world we are moving from.
  * @param target
  *   The target position on the grid (world space)
  * @param timeElapsed
  *   Amount of time this move has taken so far
  * @param animationDuration
  *   Total time allowed for animation
  */
final case class ActorPosition(
    fromPosition: Point,
    target: Point,
    timeElapsed: Seconds,
    animationDuration: Seconds
):
  def next(
      timeDelta: Seconds,
      currentTarget: Point,
      onCompleteEvent: GlobalEvent
  ): Outcome[ActorPosition] =
    val elapsed        = timeElapsed + timeDelta
    val moveIsComplete = elapsed >= animationDuration

    Outcome(
      this.copy(
        fromPosition = if moveIsComplete then currentTarget else fromPosition,
        target = currentTarget,
        timeElapsed = if moveIsComplete then Seconds.zero else elapsed
      ),
      if moveIsComplete then Batch(onCompleteEvent) else Batch.empty
    )

  def display(squareSize: Point): Point =
    Signal
      .Lerp(fromPosition * squareSize, target * squareSize, animationDuration)
      .at(timeElapsed)

object ActorPosition:
  val DefaultMoveDuration: Seconds = Seconds(0.2)

  def apply(position: Point): ActorPosition =
    initial(position, DefaultMoveDuration)

  def apply(
      position: Point,
      animationDuration: Seconds
  ): ActorPosition =
    initial(position, animationDuration)

  def initial(
      position: Point,
      animationDuration: Seconds
  ): ActorPosition =
    ActorPosition(
      position,
      position,
      Seconds.zero,
      animationDuration
    )
