package roguelike.screeneffects

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import roguelike.assets.GameAssets
import roguelike.assets.ShaderInnerGlow

final case class InnerGlow(
    screenSize: Size,
    colour: RGBA,
    intensity: Double
) extends BlendMaterial:

  def toShaderData: BlendShaderData =
    BlendShaderData(
      InnerGlow.shaderId,
      UniformBlock(
        UniformBlockName("InnerGlowData"),
        Batch(
          Uniform("GLOW_COLOR")  -> vec4.fromRGBA(colour),
          Uniform("SCREEN_SIZE") -> vec2.fromSize(screenSize),
          Uniform("INTENSITY")   -> float(intensity)
        )
      )
    )

object InnerGlow:
  val shaderId: ShaderId = ShaderId("inner glow blend material")

  val shader: BlendShader.Source =
    BlendShader
      .Source(shaderId)
      .withFragmentProgram(ShaderInnerGlow.InnerglowFragment)
