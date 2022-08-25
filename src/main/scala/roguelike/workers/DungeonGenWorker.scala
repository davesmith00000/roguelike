package roguelike.workers

import indigo.shared.IndigoLogger
import indigo.shared.datatypes.Size
import indigo.shared.dice.Dice
import org.scalajs.dom
import roguelike.RogueLikeGame
import roguelike.model.DungeonGen
import roguelike.model.DungeonGenConfig
import roguelike.model.GameMap

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("DungeonGenWorker")
object DungeonGenWorker {
  @JSExport
  def main(): Unit =
    WorkerGlobal.addEventListener("message", onMessage _)

  def onMessage(msg: dom.MessageEvent) = {
    val config = msg.data.asInstanceOf[DungeonGenConfig]
    val dice   = Dice.fromSeed(config.seed.toLong)
    val dungeon = DungeonGen.makeMap(
      dice,
      DungeonGen.MaxRooms,
      DungeonGen.RoomMinSize,
      DungeonGen.RoomMaxSize,
      RogueLikeGame.screenSize - Size(0, 5),
      DungeonGen.maxMonstersPerRoom(0),
      DungeonGen.maxCollectablesPerRoom(0),
      config.currentLevel
    )

    WorkerGlobal.postMessage(dungeon)
  }
}
