package roguelike.util

import indigo.shared.datatypes.Point

object Indices:

  def indexFromPoint(pt: Point, width: Int): Int =
    pt.y * width + pt.x
  def pointToIndex(pt: Point, width: Int): Int =
    indexFromPoint(pt, width)

  def pointFromIndex(index: Int, width: Int): Point =
    Point(
      x = index % width,
      y = index / width
    )
  def indexToPoint(index: Int, width: Int): Point =
    pointFromIndex(index, width)
