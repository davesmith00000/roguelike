package roguelike.model

import indigo._
import io.circe._
import io.circe.syntax._

sealed trait GameTile:
  def blocked: Boolean
  def blockSight: Boolean
  def isBlocked: Boolean = blocked

object GameTile:
  case object Wall extends GameTile:
    val blocked: Boolean    = true
    val blockSight: Boolean = true

  case object Ground extends GameTile:
    val blocked: Boolean    = false
    val blockSight: Boolean = false

  case object DownStairs extends GameTile:
    val blocked: Boolean    = false
    val blockSight: Boolean = false

  val scoreAs: GameTile => Int = {
    case Ground     => 1
    case DownStairs => 5
    case Wall       => Int.MaxValue
  }

  given Encoder[GameTile] = new Encoder[GameTile] {
    final def apply(data: GameTile): Json =
      data match
        case Wall =>
          Json.obj(
            ("tileType", Json.fromString("w"))
          )

        case Ground =>
          Json.obj(
            ("tileType", Json.fromString("g"))
          )

        case DownStairs =>
          Json.obj(
            ("tileType", Json.fromString("s"))
          )
  }

  given Decoder[GameTile] = new Decoder[GameTile] {
    final def apply(c: HCursor): Decoder.Result[GameTile] =
      c.downField("tileType").as[String].flatMap {
        case "w" =>
          Right(Wall)

        case "g" =>
          Right(Ground)

        case "s" =>
          Right(DownStairs)
      }
  }
