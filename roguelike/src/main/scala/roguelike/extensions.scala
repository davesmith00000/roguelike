package roguelike.extensions

import indigo.*
import roguelike.RogueMath

extension (m: Material.ImageEffects)
  def fadeIn(duration: Seconds, time: Seconds): Material.ImageEffects =
    if (time >= duration)
      m.withAlpha(1)
    else
      m.withAlpha(RogueMath.lerp(0, 1, duration, time))

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
      m.withAlpha(RogueMath.lerp(1, 0, duration, time))

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

extension (g: Group)
  def lerp(start: Point, end: Point, percentage: Double): Group =
    if (percentage <= 1)
      g.withPosition(
        Point(
          RogueMath.lerp(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.lerp(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeIn(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeIn(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeOut(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeInOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeInOut(start.y.toDouble, end.y.toDouble, percentage).toInt
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

extension [M <: Material](t: Text[M])
  def lerp(start: Point, end: Point, percentage: Double): Text[M] =
    if (percentage <= 1)
      t.moveTo(
        Point(
          RogueMath.lerp(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.lerp(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeIn(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeIn(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeOut(start.y.toDouble, end.y.toDouble, percentage).toInt
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
          RogueMath.easeInOut(start.x.toDouble, end.x.toDouble, percentage).toInt,
          RogueMath.easeInOut(start.y.toDouble, end.y.toDouble, percentage).toInt
        )
      )
    else
      t.moveTo(end)

  def easeInOut(
      start: Point,
      end: Point,
      duration: Seconds,
      time: Seconds
  ): Text[M] =
    if (time <= duration)
      easeInOut(start, end, time.toFloat / duration.toFloat)
    else
      t.moveTo(end)
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
          l.withBlendMaterial(m.withAlpha(RogueMath.lerp(0, 1, duration, time)))
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
          l.withBlendMaterial(m.withAlpha(RogueMath.lerp(1, 0, duration, time)))
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
