package roguelike.model

import indigo.shared.Outcome
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Rectangle
import indigo.shared.dice.Dice
import roguelike.GameEvent
import roguelike.model.entity.Hostile

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class HostilesManager(hostiles: List[Hostile]):

  def toJSArray: scalajs.js.Array[Hostile] =
    hostiles.toJSArray

  def findAllInRange(target: Point, range: Int) =
    hostiles
      .filter(
        _.position
          .distanceTo(target)
          .toInt <= range
      )

  def findClosest(position: Point, maxDistance: Int): Option[Hostile] =
    @tailrec
    def rec(
        remaining: List[Hostile],
        closestDistance: Int,
        acc: Option[Hostile]
    ): Option[Hostile] =
      remaining match
        case Nil =>
          acc

        case x :: xs =>
          val dist = x.position.distanceTo(position).toInt
          if dist < closestDistance then rec(xs, dist, Option(x))
          else rec(xs, closestDistance, acc)

    rec(hostiles, maxDistance, None)

  def existsAt(position: Point): Boolean =
    hostiles.exists(_.position == position)

  def findById(id: Int): Option[Hostile] =
    hostiles.toList.collectFirst {
      case e: Hostile if id == e.id => e
    }

  def findByPosition(position: Point): Option[Hostile] =
    hostiles.toList.collectFirst {
      case e: Hostile if e.position == position => e
    }

  def findBlockingByPosition(position: Point): Option[Hostile] =
    hostiles.toList.collectFirst {
      case e: Hostile if e.position == position && e.blocksMovement =>
        e
    }

  def damageHostile(id: Int, damage: Int): Outcome[HostilesManager] =
    Outcome
      .sequence(
        hostiles.map {
          case e if e.id == id && e.isAlive =>
            e.takeDamage(damage)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(hostiles = es))

  def confuseHostile(id: Int, numberOfTurns: Int): Outcome[HostilesManager] =
    Outcome
      .sequence(
        hostiles.map {
          case e if e.id == id && e.isAlive =>
            e.confuseFor(numberOfTurns)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(hostiles = es))

  def updateAllHostiles(
      dice: Dice,
      playerPosition: Point,
      pause: Boolean,
      tileMap: GameMap,
      newVisible: List[Point]
  ): Outcome[HostilesManager] =
    @tailrec
    def rec(
        remaining: List[Hostile],
        events: List[GameEvent],
        acc: List[Hostile]
    ): Outcome[List[Hostile]] =
      remaining match
        case Nil =>
          Outcome(acc).addGlobalEvents(events)

        case x :: xs if !x.isAlive || !newVisible.contains(x.position) =>
          // Filter out the dead and the unseen
          rec(xs, events, x :: acc)

        case x :: xs if playerPosition.distanceTo(x.position) <= 1 =>
          // Close enough to attack!
          val event = GameEvent.HostileMeleeAttack(x.name, x.fighter.power)
          rec(xs, event :: events, x :: acc)

        case x :: xs if x.isConfused =>
          // Is confused!
          val randomMove = getRandomDirection(dice, x.position, tileMap)
          rec(xs, events, x.nextState.moveTo(randomMove) :: acc)

        case x :: xs =>
          // Otherwise, move a little closer...
          val entityPositions =
            (xs ++ acc).flatMap(e =>
              if e.blocksMovement then List(e.position) else Nil
            )
          val path =
            GameMap.getPathTo(
              dice,
              x.position,
              playerPosition,
              entityPositions,
              tileMap
            )

          // First path result is current location, we want the next one if it exists.
          path.drop(1).headOption match
            case Some(nextPosition) =>
              rec(xs, events, x.moveTo(nextPosition) :: acc)

            case None =>
              rec(xs, events, x :: acc)

    if !pause then
      val res = rec(hostiles, Nil, Nil)
      res.map(hs => this.copy(hostiles = hs))
    else Outcome(this)

  def getRandomDirection(
      dice: Dice,
      position: Point,
      tileMap: GameMap
  ): Point =
    val up    = Point(0, -1)
    val down  = Point(0, 1)
    val left  = Point(-1, 0)
    val right = Point(1, 0)

    val positions =
      List(
        position + up,
        position + down,
        position + left,
        position + right
      ).filter { pt =>
        tileMap.lookUp(pt) match
          case None =>
            false

          case Some(t) if t.blocked =>
            false

          case _ =>
            true
      }

    if positions.isEmpty then position
    else positions(dice.rollFromZero(positions.length - 1))