package dungeongen.js

import scala.scalajs.js

trait DungeonGenConfig extends js.Object:
  val seed: Double
  val currentLevel: Int
  val width: Int
  val height: Int

object DungeonGenConfig:
  def apply(s: Double, c: Int, w: Int, h: Int): DungeonGenConfig =
    new DungeonGenConfig {
      val seed: Double      = s
      val currentLevel: Int = c
      val width: Int        = w
      val height: Int       = h
    }
