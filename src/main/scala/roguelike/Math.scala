package roguelike

import indigo._

import scala.math.sqrt

object Math:
  def lerp(start: Double, end: Double, percentage: Double): Double =
    if (percentage <= 1)
      (start + (end - start)) * percentage
    else
      end

  def lerp(
      start: Double,
      end: Double,
      duration: Seconds,
      time: Seconds
  ): Double =
    if (time <= duration)
      lerp(start, end, time.toDouble / duration.toDouble)
    else
      end

  def easeIn(t: Double): Double =
    t * t

  def easeIn(
      start: Double,
      end: Double,
      percentage: Double
  ): Double =
    lerp(start, end, easeIn(percentage))

  def easeIn(
      start: Double,
      end: Double,
      duration: Seconds,
      time: Seconds
  ): Double =
    if (time <= duration)
      lerp(start, end, easeIn(time.toDouble / duration.toDouble))
    else
      end

  def flip(x: Double): Double = 1 - x

  def easeOut(t: Double): Double =
    flip(sqrt(flip(t)))

  def easeOut(
      start: Double,
      end: Double,
      duration: Seconds,
      time: Seconds
  ): Double =
    if (time <= duration)
      lerp(start, end, easeOut(time.toDouble / duration.toDouble))
    else
      end

  def easeOut(
      start: Double,
      end: Double,
      percentage: Double
  ): Double =
    lerp(start, end, easeOut(percentage))

  def easeInOut(t: Double) =
    lerp(easeIn(t), easeOut(t), t)

  def easeInOut(
      start: Double,
      end: Double,
      duration: Seconds,
      time: Seconds
  ): Double =
    if (time <= duration)
      lerp(start, end, easeInOut(time.toDouble / duration.toDouble))
    else
      end

  def easeInOut(
      start: Double,
      end: Double,
      percentage: Double
  ): Double =
    lerp(start, end, easeInOut(percentage))
