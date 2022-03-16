package roguelike.game

import indigo._
import indigo.shared.shader.ShaderPrimitive.rawJSArray
import roguelike.Assets

import scalajs.js
import scalajs.js.JSConverters._

final case class MiniMap(
    position: Point,
    size: Size,
    depth: Depth,
    player: Point,
    walls: js.Array[Point]
) extends EntityNode[MiniMap]:
  val flip: Flip                   = Flip.default
  val ref: Point                   = Point.zero
  val rotation: Radians            = Radians.zero
  val scale: Vector2               = Vector2.one
  val eventHandlerEnabled: Boolean = false
  def eventHandler: ((MiniMap, GlobalEvent)) => Option[GlobalEvent] =
    Function.const(None)
  def withDepth(newDepth: Depth): MiniMap = this.copy(depth = newDepth)

  def moveTo(newPosition: Point): MiniMap =
    this.copy(position = newPosition)

  def resize(newSize: Size): MiniMap =
    this.copy(size = newSize)

  def update(newPlayer: Point, newWalls: js.Array[Point]): MiniMap =
    this.copy(player = newPlayer, walls = newWalls)

  def withPlayer(newPlayer: Point): MiniMap =
    this.copy(player = newPlayer)

  def withWalls(newWalls: js.Array[Point]): MiniMap =
    this.copy(walls = newWalls)

  private val wallArray =
    val arr = MiniMap.EmptyArray;

    walls.foreach { pt =>
      val idx = pt.y * size.width + pt.x
      arr(idx * 4) = 1.0f
    }

    rawJSArray(arr)

  lazy val toShaderData: ShaderData =
    ShaderData(
      MiniMap.shaderId,
      UniformBlock(
        "MiniMapData",
        List(
          Uniform("GRID_DIMENSIONS") -> vec2.fromSize(size),
          Uniform("PLAYER_POSITION") -> vec2.fromPoint(player)
        )
      ),
      UniformBlock(
        "MiniMapBlockData",
        List(
          Uniform("BLOCKS") -> wallArray
        )
      )
    )

object MiniMap:

  val EmptyArray: js.Array[Float] =
    Array.fill[Float](4000 * 4)(0.0).toJSArray

  def initial(position: Point, size: Size): MiniMap =
    MiniMap(
      position,
      size,
      Depth.zero,
      Point.zero,
      js.Array[Point]()
    )

  val shaderId: ShaderId =
    ShaderId("minimap")

  val shader: EntityShader.External =
    EntityShader
      .External(shaderId)
      .withFragmentProgram(Assets.miniMapFragShader)
