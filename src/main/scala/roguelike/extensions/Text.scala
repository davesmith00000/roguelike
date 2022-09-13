package roguelike.extensions

import indigo._
import roguelike.Math

extension [M <: Material](t: Text[M])
  def lerp(start: Point, end: Point, percentage: Double): Text[M] =
    if (percentage <= 1)
      t.moveTo(
        Point(
          Math.lerp(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.lerp(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      t.moveTo(end)

  def lerp(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Text[M] =
    if (time <= duration)
      lerp(start, end, time.toFloat / duration.toFloat)
    else
      t.moveTo(end)

  def easeIn(start: Point, end: Point, percentage: Double): Text[M] =
    if (percentage <= 1)
      t.moveTo(
        Point(
          Math.easeIn(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeIn(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      t.moveTo(end)

  def easeIn(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Text[M] =
    if (time <= duration)
      easeIn(start, end, time.toDouble / duration.toDouble)
    else
      t.moveTo(end)

  def easeOut(start: Point, end: Point, percentage: Double): Text[M] =
    if (percentage <= 1)
      t.moveTo(
        Point(
          Math.easeOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeOut(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      t.moveTo(end)

  def easeOut(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Text[M] =
    if (time <= duration)
      easeOut(start, end, time.toFloat / duration.toFloat)
    else
      t.moveTo(end)

  def easeInOut(start: Point, end: Point, percentage: Double): Text[M] =
    if (percentage <= 1)
      t.moveTo(
        Point(
          Math.easeInOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeInOut(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      t.moveTo(end)

  def easeOInut(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Text[M] =
    if (time <= duration)
      easeInOut(start, end, time.toFloat / duration.toFloat)
    else
      t.moveTo(end)
