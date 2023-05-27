package roguelike.shaders

import indigo.*

object Hover:

  val shader: Shader =
    UltravioletShader.entityFragment(
      ShaderId("shader"),
      EntityShader.fragment[FragmentEnv](HoverShader.fragment, FragmentEnv.reference)
    )

  val view: BlankEntity =
    BlankEntity(
      Rectangle(28, 28),
      ShaderData(shader.id)
    )

  object HoverShader:

    import ultraviolet.syntax.*

    inline def fragment: Shader[FragmentEnv, Unit] =
      Shader[FragmentEnv] { env =>

        @const val baseColour = vec3(0.37f, 0.8f, 0.89f)

        def sdBox(p: vec2, b: vec2): Float =
          val d = abs(p) - b
          length(max(d, 0.0f)) + min(max(d.x, d.y), 0.0f)

        def makeBox(time: Float, minDistance: Float, distMultiplier: Float): Float =
          val distance = vec2(minDistance + abs(sin(time) * distMultiplier))
          sdBox(env.UV - 0.5f, distance)

        def makeFrame(
            time: Float,
            halfStrokeWidth: Float,
            halfGlowWidth: Float,
            minDistance: Float,
            distMultiplier: Float
        ): vec4 =
          val sdf         = makeBox(time, minDistance, distMultiplier)
          val frame       = 1.0f - step(0.0f, abs(-sdf) - halfStrokeWidth)
          val frameColour = baseColour * frame
          val glowAmount  = smoothstep(0.97f, 1.05f, 1.0f - (abs(sdf) - halfGlowWidth))
          val glowColour  = vec3(0.0f, glowAmount * 0.5f, glowAmount * 0.5f)
          val alpha       = glowAmount + frame
          val finalColour = mix(glowColour, frameColour, frame)

          vec4(finalColour, alpha)

        def fragment(color: vec4): vec4 =
          val frame1 = makeFrame(env.TIME * 3f, 0.02f, 0.075f, 0.40f, 0.1f)
          clamp(frame1, vec4(0.0f), vec4(baseColour, 1.0f))
      }
