package roguelike.model

import indigo.shared.IndigoLogger
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGBA
import indigo.shared.datatypes.Size
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*

object SharedCodecs:

  given Encoder[Size] = new Encoder[Size] {
    final def apply(data: Size): Json = Json.obj(
      ("width", Json.fromInt(data.width)),
      ("height", Json.fromInt(data.height))
    )
  }

  given Decoder[Size] = new Decoder[Size] {
    final def apply(c: HCursor): Decoder.Result[Size] =
      for {
        w <- c.downField("width").as[Int]
        h <- c.downField("height").as[Int]
      } yield Size(w, h)
  }

  given Encoder[Point] = new Encoder[Point] {
    final def apply(data: Point): Json = Json.obj(
      ("x", Json.fromInt(data.x)),
      ("y", Json.fromInt(data.y))
    )
  }

  given Decoder[Point] = new Decoder[Point] {
    final def apply(c: HCursor): Decoder.Result[Point] =
      for {
        x <- c.downField("x").as[Int]
        y <- c.downField("y").as[Int]
      } yield new Point(x, y)
  }

  given Encoder[RGBA] = new Encoder[RGBA] {
    final def apply(data: RGBA): Json =
      Json.obj(
        ("r", Json.fromDoubleOrNull(data.r)),
        ("g", Json.fromDoubleOrNull(data.g)),
        ("b", Json.fromDoubleOrNull(data.b)),
        ("a", Json.fromDoubleOrNull(data.a))
      )
  }

  given Decoder[RGBA] = new Decoder[RGBA] {
    final def apply(c: HCursor): Decoder.Result[RGBA] =
      for {
        r <- c.downField("r").as[Double]
        g <- c.downField("g").as[Double]
        b <- c.downField("b").as[Double]
        a <- c.downField("a").as[Double]
      } yield new RGBA(r, g, b, a)
  }
