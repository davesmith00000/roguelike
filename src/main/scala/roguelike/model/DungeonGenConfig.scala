package roguelike.model

import scala.scalajs.js

trait DungeonGenConfig extends js.Object:
  val seed: Double
  val currentLevel: Int

object DungeonGenConfig:
  def apply(seed: Double, currentLevel: Int): DungeonGenConfig =
    new DungeonGenConfig {
      val seed: Double      = seed
      val currentLevel: Int = currentLevel
    }
