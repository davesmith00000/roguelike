package roguelike.screeneffects

import indigo.*
import indigo.syntax.*
import indigo.syntax.shaders.*
import roguelike.assets.ShaderInnerGlow
import ultraviolet.syntax.*

final case class InnerGlow(
    screenSize: Size,
    colour: RGBA,
    intensity: Double
) extends BlendMaterial:

  def toShaderData: ShaderData =
    ShaderData(
      InnerGlow.shaderId,
      InnerGlowData(
        colour.toUVVec4,
        screenSize.toUVVec2,
        intensity.toFloat
      )
    )

final case class InnerGlowData(GLOW_COLOR: vec4, SCREEN_SIZE: vec2, INTENSITY: Float)
    derives ToUniformBlock

object InnerGlow:
  val shaderId: ShaderId = ShaderId("inner glow blend material")

  val shader: BlendShader.Source =
    BlendShader
      .Source(shaderId)
      .withFragmentProgram(ShaderInnerGlow.InnerglowFragment)
