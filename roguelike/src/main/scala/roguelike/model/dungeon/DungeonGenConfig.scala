package roguelike.model.dungeon

import scala.scalajs.js

trait DungeonGenConfig extends js.Object:
  val seed: Double
  val currentLevel: Int

object DungeonGenConfig:
  def apply(s: Double, c: Int): DungeonGenConfig =
    new DungeonGenConfig {
      val seed: Double      = s
      val currentLevel: Int = c
    }
