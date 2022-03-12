package roguelike.model

import indigo.shared.IndigoLogger
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGB
import indigo.shared.datatypes.Size
import io.circe._
import io.circe.parser.decode
import io.circe.syntax._

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

  given Encoder[RGB] = new Encoder[RGB] {
    final def apply(data: RGB): Json =
      Json.obj(
        ("r", Json.fromDoubleOrNull(data.r)),
        ("g", Json.fromDoubleOrNull(data.g)),
        ("b", Json.fromDoubleOrNull(data.b))
      )
  }

  given Decoder[RGB] = new Decoder[RGB] {
    final def apply(c: HCursor): Decoder.Result[RGB] =
      for {
        r <- c.downField("r").as[Double]
        g <- c.downField("g").as[Double]
        b <- c.downField("b").as[Double]
      } yield new RGB(r, g, b)
  }
