package roguelike.model

import indigo.*
import io.circe.*
import io.circe.syntax.*

sealed trait GameTile:
  def blocked: Boolean
  def blockSight: Boolean
  def isBlocked: Boolean = blocked
  def isDownStairs: Boolean
  def isGround: Boolean
  def isWall: Boolean

object GameTile:
  final case class Wall(code: Option[String]) extends GameTile:
    val blocked: Boolean      = true
    val blockSight: Boolean   = true
    val isDownStairs: Boolean = false
    val isGround: Boolean     = false
    val isWall: Boolean       = true

  final case class Ground(style: Int) extends GameTile:
    val blocked: Boolean      = false
    val blockSight: Boolean   = false
    val isDownStairs: Boolean = false
    val isGround: Boolean     = true
    val isWall: Boolean       = false

  case object DownStairs extends GameTile:
    val blocked: Boolean      = false
    val blockSight: Boolean   = false
    val isDownStairs: Boolean = true
    val isGround: Boolean     = false
    val isWall: Boolean       = false

  val scoreAs: GameTile => Int = {
    case Ground(_)  => 1
    case DownStairs => 5
    case Wall(_)    => Int.MaxValue
  }

  given Encoder[GameTile] = new Encoder[GameTile] {
    final def apply(data: GameTile): Json =
      data match
        case Wall(code) =>
          Json.obj(
            ("tileType", Json.fromString("w" + code))
          )

        case Ground(style) =>
          Json.obj(
            ("tileType", Json.fromString("g" + style.toString))
          )

        case DownStairs =>
          Json.obj(
            ("tileType", Json.fromString("s"))
          )
  }

  given Decoder[GameTile] = new Decoder[GameTile] {
    final def apply(c: HCursor): Decoder.Result[GameTile] =
      c.downField("tileType").as[String].flatMap {
        case s if s.startsWith("w") =>
          val code = s.drop(1)
          Right(Wall(Option(code)))

        case s if s.startsWith("g") =>
          val style = s.drop(1).toInt
          Right(Ground(style))

        case "s" =>
          Right(DownStairs)
      }
  }
