package roguelike.viewmodel

import indigo._
import roguelike.GameEvent

/** ActorPosition is a view model helper to track the position actors should be
  * rendered at on the screen.
  * @param current
  *   current position in pixels
  * @param target
  *   target position in pixels
  * @param precise
  *   is the precise position in sub-pixels, used to avoid lumpy movement
  * @param travelTimeTaken
  *   Amount of time this move has taken so far
  * @param squareSize
  *   Size of a tile in pixels
  * @param animationDuration
  *   Total time allowed for animation
  */
final case class ActorPosition(
    current: Point,
    target: Point,
    precise: Vector2,
    travelTimeTaken: Seconds,
    squareSize: Point,
    animationDuration: Seconds
):
  lazy val display: Point =
    if precise ~== target.toVector then target else precise.toPoint

  def next(
      timeDelta: Seconds,
      gridDestination: Point,
      onCompleteEvent: GlobalEvent
  ): Outcome[ActorPosition] =
    val destinationPosition =
      ActorPosition.modelToMapSpaceTileCentered(gridDestination, squareSize)

    val elapsedTime = travelTimeTaken + timeDelta
    val time =
      if destinationPosition == target then
        if elapsedTime > animationDuration then animationDuration
        else elapsedTime
      else Seconds.zero

    val next = ActorPosition.moveTowardsTarget(
      this,
      travelTimeTaken,
      animationDuration
    )

    val moveIsComplete =
      destinationPosition == target && time >= animationDuration && (next ~== target.toVector)

    val events =
      if moveIsComplete then Batch(onCompleteEvent) else Batch.empty

    Outcome(
      this.copy(
        current = if moveIsComplete then target else current,
        target = destinationPosition,
        precise = next,
        travelTimeTaken = time
      ),
      events
    )

object ActorPosition:
  val DefaultMoveDuration: Seconds = Seconds(0.2)

  def apply(mapPosition: Point, squareSize: Point): ActorPosition =
    initial(mapPosition, squareSize, DefaultMoveDuration)

  def apply(
      mapPosition: Point,
      squareSize: Point,
      animationDuration: Seconds
  ): ActorPosition =
    initial(mapPosition, squareSize, animationDuration)

  def initial(
      mapPosition: Point,
      squareSize: Point,
      animationDuration: Seconds
  ): ActorPosition =
    val modelPosition = modelToMapSpaceTileCentered(mapPosition, squareSize)
    ActorPosition(
      modelPosition,
      modelPosition,
      modelPosition.toVector,
      Seconds.zero,
      squareSize,
      animationDuration
    )

  def modelToMapSpaceTileCentered(position: Point, squareSize: Point): Point =
    position * squareSize

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
