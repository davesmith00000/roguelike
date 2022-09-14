package roguelike.extensions

import indigo._
import roguelike.Math

extension (g: Group)
  def lerp(start: Point, end: Point, percentage: Double): Group =
    if (percentage <= 1)
      g.withPosition(
        Point(
          Math.lerp(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.lerp(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      g.withPosition(end)

  def lerp(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Group =
    if (time <= duration)
      lerp(start, end, time.toFloat / duration.toFloat)
    else
      g.withPosition(end)

  def easeIn(start: Point, end: Point, percentage: Double): Group =
    if (percentage <= 1)
      g.withPosition(
        Point(
          Math.easeIn(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeIn(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      g.withPosition(end)

  def easeIn(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Group =
    if (time <= duration)
      easeIn(start, end, time.toDouble / duration.toDouble)
    else
      g.withPosition(end)

  def easeOut(start: Point, end: Point, percentage: Double): Group =
    if (percentage <= 1)
      g.withPosition(
        Point(
          Math.easeOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeOut(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      g.withPosition(end)

  def easeOut(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Group =
    if (time <= duration)
      easeOut(start, end, time.toFloat / duration.toFloat)
    else
      g.withPosition(end)

  def easeInOut(start: Point, end: Point, percentage: Double): Group =
    if (percentage <= 1)
      g.withPosition(
        Point(
          Math.easeInOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          Math.easeInOut(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      g.withPosition(end)

  def easeInOut(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Group =
    if (time <= duration)
      easeInOut(start, end, time.toFloat / duration.toFloat)
    else
      g.withPosition(end)
