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

  def findAliveById(id: Int): Option[Hostile] =
    hostiles.toList.collectFirst {
      case e: Hostile if id == e.id && e.isAlive => e
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

object HostilesPool:

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
