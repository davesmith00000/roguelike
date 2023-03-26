package roguelike.viewmodel

import indigo.*
import indigoextras.geometry.Vertex
import roguelike.GameEvent

/** ActorPosition is a view model helper to track the position actors should be rendered at on the
  * screen.
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
    animationDuration: Seconds,
    facing: ActorDirection
):
  val state: ActorMoveState =
    if attacking > 0.0 && target == fromPosition then ActorMoveState.Attacking
    else if target != fromPosition then ActorMoveState.Moving
    else ActorMoveState.Idle

  def next(
      timeDelta: Seconds,
      currentTarget: Point,
      onCompleteEvent: GlobalEvent
  ): Outcome[ActorPosition] =
    val elapsed        = timeElapsed + timeDelta
    val moveIsComplete = elapsed >= animationDuration

    val nextFacing =
      if target.y < fromPosition.y then facing // ActorDirection.Up // This is no up for the knight
      else if target.y > fromPosition.y then
        facing // ActorDirection.Up // This is no down for the knight
      else if target.x < fromPosition.x then ActorDirection.Left
      else if target.x > fromPosition.x then ActorDirection.Right
      else facing

    Outcome(
      this.copy(
        fromPosition = if moveIsComplete then currentTarget else fromPosition,
        target = currentTarget,
        timeElapsed = if moveIsComplete then Seconds.zero else elapsed,
        facing = nextFacing
      ),
      if moveIsComplete then Batch(onCompleteEvent) else Batch.empty
    )

  def moving(squareSize: Point): Point =
    Signal
      .Lerp(fromPosition * squareSize, target * squareSize, animationDuration)
      .at(timeElapsed)

  def attacking: Double =
    Signal.SmoothPulse
      .map { d =>
        if fromPosition == target then d * 0.55
        else d * 0.35
      }
      .at(timeElapsed / animationDuration)

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
      animationDuration,
      ActorDirection.Right
    )

enum ActorMoveState:
  case Idle, Moving, Attacking

enum ActorDirection:
  case Up, Down, Left, Right
