package roguelike.extensions

import indigo._
import roguelike.Math.lerp

extension (m: Material.ImageEffects)
  def fadeIn(duration: Seconds, time: Seconds): Material.ImageEffects =
    if (time >= duration)
      m.withAlpha(1)
    else
      m.withAlpha(lerp(0, 1, duration, time))

  def fadeIn(
      start: Seconds,
      duration: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    if (time < start)
      m
    else
      fadeIn(duration, time - start)

  def fadeOut(duration: Seconds, time: Seconds): Material.ImageEffects =
    if (time >= duration)
      m.withAlpha(0)
    else
      m.withAlpha(lerp(1, 0, duration, time))

  def fadeOut(
      start: Seconds,
      duration: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    if (time < start)
      m
    else
      fadeOut(duration, time - start)

  def fadeInOut(
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    val midPoint = fadeDuration * 0.5
    val fadeOut  = midPoint + pause
    if (time < midPoint) m.fadeIn(midPoint, time)
    else if (time > fadeOut)
      m.fadeOut(midPoint, time - fadeOut)
    else m.withAlpha(1)

  def fadeInOut(
      start: Seconds,
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    if (time < start)
      m
    else
      fadeInOut(fadeDuration, pause, time - start)

  def fadeOutIn(
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    val midPoint = fadeDuration * 0.5
    val fadeIn   = midPoint + pause
    if (time < midPoint)
      m.fadeOut(midPoint, time)
    else if (time > fadeIn)
      m.fadeIn(midPoint, time - fadeIn)
    else
      m.withAlpha(0)

  def fadeOutIn(
      start: Seconds,
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Material.ImageEffects =
    if (time < start)
      m
    else
      fadeOutIn(fadeDuration, pause, time - start)
