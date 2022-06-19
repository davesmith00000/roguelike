package roguelike.model

import indigo.*
import indigo.syntax.*
import roguelike.GameEvent
import roguelike.HostileEvent
import roguelike.model.entity.Hostile

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class HostilesPool(hostiles: Batch[Hostile]):

  lazy val toJSArray: scalajs.js.Array[Hostile] =
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

    rec(hostiles.toList, maxDistance, None)

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

  def damageHostile(id: Int, damage: Int): Outcome[HostilesPool] =
    Outcome
      .sequence(
        hostiles.map {
          case e if e.id == id && e.isAlive =>
            e.takeDamage(damage)

          case e => Outcome(e)
        }
      )
      .map(es => this.copy(hostiles = es))

  def confuseHostile(id: Int, numberOfTurns: Int): Outcome[HostilesPool] =
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
      gameMap: GameMap
  ): Outcome[HostilesPool] =
    @tailrec
    def rec(
        remaining: List[Hostile],
        events: Batch[GameEvent],
        acc: Batch[Hostile]
    ): Outcome[Batch[Hostile]] =
      remaining match
        case Nil =>
          Outcome(acc).addGlobalEvents(events)

        case x :: xs if !x.isAlive || !gameMap.visible.contains(x.position) =>
          // Filter out the dead and the unseen
          rec(xs, events, x :: acc)

        case x :: xs if playerPosition.distanceTo(x.position) <= 1 =>
          // Close enough to attack!
          val event = GameEvent.Hostile(
            HostileEvent.HostileMeleeAttack(x.name, x.fighter.power)
          )
          rec(xs, event :: events, x :: acc)

        case x :: xs if x.isConfused =>
          // Is confused!
          val randomMove = getRandomDirection(dice, x.position, gameMap)
          rec(xs, events, x.nextState.moveTo(randomMove) :: acc)

        case x :: xs =>
          // Otherwise, move a little closer...
          val entityPositions =
            (xs.toBatch ++ acc).flatMap(e =>
              if e.blocksMovement then Batch(e.position) else Batch.empty
            )
          val path =
            GameMap.getPathTo(
              dice,
              x.position,
              playerPosition,
              entityPositions,
              gameMap
            )

          // First path result is current location, we want the next one if it exists.
          path.drop(1).headOption match
            case Some(nextPosition) =>
              rec(xs, events, x.moveTo(nextPosition) :: acc)

            case None =>
              rec(xs, events, x :: acc)

    val res = rec(hostiles.toList, Batch.empty, Batch.empty)
    res.map(hs => this.copy(hostiles = hs))

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
