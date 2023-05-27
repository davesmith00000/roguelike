package roguelike.model

import indigo.*
import indigo.syntax.*
import roguelike.GameEvent
import roguelike.HostileEvent
import roguelike.model.entity.Hostile

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

final case class HostilesPool(
    queued: Batch[Hostile],
    inProgress: Option[Hostile],
    done: Batch[Hostile]
):

  lazy val hostiles: Batch[Hostile] =
    queued ++ Batch.fromOption(inProgress) ++ done

  lazy val toJSArray: scalajs.js.Array[Hostile] =
    hostiles.toJSArray

  def queueAll: HostilesPool =
    this.copy(done = Batch.empty, queued = hostiles)

  def update(
      visibleTiles: Batch[Point],
      f: (Hostile, Batch[Hostile]) => Outcome[Hostile]
  ): Outcome[HostilesPool] =
    inProgress match
      case None =>
        updateNextQueued(visibleTiles, f)

      case Some(hostile) =>
        Outcome(this)

  def completeInProgress: Outcome[HostilesPool] =
    inProgress match
      case None =>
        Outcome(this)

      case Some(hostile) =>
        Outcome(
          this.copy(
            inProgress = None,
            done = hostile :: done
          )
        )

  /** Runs through the queued hostiles until it finds one that is alive, processes that one, and
    * drops out. If it reaches the end of the list, it ends the NPC turn.
    */
  def updateNextQueued(
      visibleTiles: Batch[Point],
      f: (Hostile, Batch[Hostile]) => Outcome[Hostile]
  ): Outcome[HostilesPool] =
    @tailrec
    def rec(
        remaining: Batch[Hostile],
        processed: Batch[Hostile]
    ): Outcome[HostilesPool] =
      if remaining.isEmpty then
        Outcome(
          this.copy(
            queued = Batch.empty,
            inProgress = None,
            done = processed ++ done
          ),
          Batch(GameEvent.NPCTurnComplete)
        )
      else
        val h = remaining.head
        val t = remaining.drop(1)

        if h.isAlive && visibleTiles.contains(h.position) then
          f(h, t ++ done).map { d =>
            this.copy(
              queued = t,
              inProgress = Some(d),
              done = processed ++ done
            )
          }
        else rec(t, h :: processed)

    rec(queued, Batch.empty)

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

  def apply(hostiles: Batch[Hostile]): HostilesPool =
    HostilesPool(Batch.empty, None, hostiles)

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
