package roguelike.viewmodel

import indigo._
import roguelike.GameEvent
import roguelike.model.GamePhase

final case class ActorPosition(
    current: Point,
    target: Point,
    precise: Vector2,
    travelTime: Seconds
):
  val display: Point =
    if precise ~== target.toVector then target else precise.toPoint

  // TODO: Write tests. Feels odd.
  def next(
      timeDelta: Seconds,
      position: Point,
      squareSize: Point
  ): Outcome[ActorPosition] =
    val modelPosition = (position * squareSize) + (squareSize.x / 2)
    val elapsedTime   = travelTime + timeDelta
    val time =
      if modelPosition == target then
        if elapsedTime > ActorPosition.DefaultMoveDuration then
          ActorPosition.DefaultMoveDuration
        else elapsedTime
      else Seconds.zero

    val next = ActorPosition.moveTowardsTarget(
      this,
      travelTime,
      ActorPosition.DefaultMoveDuration
    )

    val moveIsComplete =
      modelPosition == target && time >= ActorPosition.DefaultMoveDuration && (next ~== target.toVector)

    val events =
      if moveIsComplete then List(GameEvent.PlayerTurnComplete) else Nil

    Outcome(
      this.copy(
        current = if moveIsComplete then target else current,
        target = modelPosition,
        precise = next,
        travelTime = time
      )
    ).addGlobalEvents(events)

object ActorPosition:
  val DefaultMoveDuration: Seconds = Seconds(0.2)

  def initial(mapPosition: Point, squareSize: Point): ActorPosition =
    val modelPosition = (mapPosition * squareSize) + (squareSize.x / 2)
    ActorPosition(
      modelPosition,
      modelPosition,
      modelPosition.toVector,
      DefaultMoveDuration
    )

  def moveTowardsTarget(
      actorPosition: ActorPosition,
      travelTime: Seconds,
      animationDuration: Seconds
  ): Vector2 =
    val currentVec = actorPosition.current.toVector
    val targetVec  = actorPosition.target.toVector
    val distance   = targetVec - currentVec
    val speed      = distance / animationDuration.toDouble
    val moveAmount = speed * travelTime.toDouble
    val diff       = targetVec - actorPosition.precise

    if moveAmount.length >= diff.length then targetVec
    else currentVec + moveAmount
