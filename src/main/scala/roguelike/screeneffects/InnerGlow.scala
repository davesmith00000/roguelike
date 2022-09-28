package roguelike.screeneffects

import indigo.*
import indigo.scenes.*
import indigo.syntax.*
import roguelike.assets.GameAssets

case object InnerGlow extends BlendMaterial:
  val shaderId: ShaderId = ShaderId("inner glow blend material")

  val shader: BlendShader.External =
    BlendShader
      .External(shaderId)
      .withFragmentProgram(GameAssets.InnerGlow)

  def toShaderData: BlendShaderData =
    BlendShaderData(
      shaderId,
      UniformBlock(
        "InnerGlowData",
        Batch(
          Uniform("GLOW_COLOR") -> vec4.fromRGBA(RGBA.Red)
        )
      )
    )
