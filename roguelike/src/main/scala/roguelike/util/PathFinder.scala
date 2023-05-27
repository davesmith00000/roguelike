package roguelike.util

import indigo.shared.collections.Batch
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.Rectangle
import indigo.shared.datatypes.Size
import indigo.shared.dice.Dice

import scala.annotation.tailrec

object PathFinder:
  private def sampleAt(
      coords: Point
  ): List[Point] =
    List(
      coords + Point(0, -1),
      coords + Point(-1, 0),
      coords + Point(1, 0),
      coords + Point(0, 1)
    )

  // A quick breadth first search or walkable positions
  def locatePath(
      start: Point,
      end: Point,
      walkable: Batch[Point]
  ): Batch[Point] =
    @tailrec
    def rec(open: List[Node], seen: List[Point]): Batch[Point] =
      open match
        case Nil =>
          // Ran out of options, no path found
          Batch.empty

        case x :: xs if x.position == end =>
          // We reached the end! Gimme the path!
          x.toPath

        case x :: xs =>
          // sample the node, filter down to valid samples,
          // add them to list of nodes to try.
          val samples = sampleAt(x.position)
          val filtered = samples
            .filter(pt => !seen.contains(pt) && walkable.contains(pt))
          val next = filtered.map { pt =>
            Node.Move(pt, x)
          }

          rec(xs ++ next, seen ++ filtered)

    rec(List(Node.Root(start)), Nil)

enum Node(val position: Point):
  case Root(coords: Point)               extends Node(coords)
  case Move(coords: Point, parent: Node) extends Node(coords)

  def toPath: Batch[Point] =
    @tailrec
    def rec(node: Node, acc: Batch[Point]): Batch[Point] =
      node match
        case Root(p) =>
          p :: acc

        case Move(p, prev) =>
          rec(prev, p :: acc)

    rec(this, Batch.empty)

  def contains(pt: Point): Boolean =
    @tailrec
    def rec(node: Node): Boolean =
      node match
        case Root(p) =>
          p == pt

        case Move(p, _) if p == pt =>
          true

        case Move(_, prev) =>
          rec(prev)

    rec(this)
