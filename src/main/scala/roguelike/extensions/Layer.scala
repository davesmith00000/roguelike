package roguelike.extensions

import indigo._
import roguelike.Math.lerp

extension (l: Layer)
  def fadeIn(duration: Seconds, time: Seconds): Layer =
    val material = l.blending.map(b => b.blendMaterial) match {
      case Some(m) => m
      case None    => BlendMaterial.BlendEffects.None
    }

    material match {
      case m: BlendMaterial.BlendEffects =>
        if (time >= duration)
          l.withBlendMaterial(m.withAlpha(1))
        else
          l.withBlendMaterial(m.withAlpha(lerp(0, 1, duration, time)))
      case _ => l
    }

  def fadeIn(
      start: Seconds,
      duration: Seconds,
      time: Seconds
  ): Layer =
    if (time < start)
      l
    else
      fadeIn(duration, time - start)

  def fadeOut(duration: Seconds, time: Seconds): Layer =
    val material = l.blending.map(b => b.blendMaterial) match {
      case Some(m) => m
      case None    => BlendMaterial.BlendEffects.None
    }

    material match {
      case m: BlendMaterial.BlendEffects =>
        if (time >= duration)
          l.withBlendMaterial(m.withAlpha(0))
        else
          l.withBlendMaterial(m.withAlpha(lerp(1, 0, duration, time)))
      case _ => l
    }

  def fadeOut(
      start: Seconds,
      duration: Seconds,
      time: Seconds
  ): Layer =
    if (time < start)
      l
    else
      fadeOut(duration, time - start)

  def fadeInOut(
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Layer =
    val material = l.blending.map(b => b.blendMaterial) match {
      case Some(m) => m
      case None    => BlendMaterial.BlendEffects.None
    }

    material match {
      case m: BlendMaterial.BlendEffects =>
        val midPoint = fadeDuration * 0.5
        val fadeOut  = midPoint + pause
        if (time < midPoint) l.fadeIn(midPoint, time)
        else if (time > fadeOut)
          l.fadeOut(midPoint, time - fadeOut)
        else l.withBlendMaterial(m.withAlpha(1))
      case _ => l
    }

  def fadeInOut(
      start: Seconds,
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Layer =
    if (time < start)
      l
    else
      fadeInOut(fadeDuration, pause, time - start)

  def fadeOutIn(
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Layer =
    val material = l.blending.map(b => b.blendMaterial) match {
      case Some(m) => m
      case None    => BlendMaterial.BlendEffects.None
    }

    material match {
      case m: BlendMaterial.BlendEffects =>
        val midPoint = fadeDuration * 0.5
        val fadeIn   = midPoint + pause
        if (time < midPoint)
          l.fadeOut(midPoint, time)
        else if (time > fadeIn)
          l.fadeIn(midPoint, time - fadeIn)
        else
          l.withBlendMaterial(m.withAlpha(0))
      case _ => l
    }

  def fadeOutIn(
      start: Seconds,
      fadeDuration: Seconds,
      pause: Seconds,
      time: Seconds
  ): Layer =
    if (time < start)
      l
    else
      l.fadeOutIn(fadeDuration, pause, time - start)
