package dungeongen.workers

import dungeongen.classic.DungeonRules
import dungeongen.js.DungeonGenConfig
import dungeongen.js.JsDungeon
import dungeongen.js.JsDungeonGameMapTuple
import dungeongen.js.JsGameMap
import indigo.shared.IndigoLogger
import indigo.shared.collections.Batch
import indigo.shared.datatypes.Size
import indigo.shared.dice.Dice
import org.scalajs.dom
import org.scalajs.dom.Worker
import roguelike.model.GameMap

import scala.scalajs.js
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

trait IndigoWorker[A <: js.Any, B <: js.Any]:
  val name: WorkerName
  lazy val worker: Worker = new Worker(name.toString + ".js")

  @JSExport
  def doWork(): Unit =
    WorkerGlobal.addEventListener("message", onMessage _)

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def onMessage(msg: dom.MessageEvent): Unit =
    process(msg.data.asInstanceOf[A])
      .map(WorkerGlobal.postMessage(_))

  def process(data: A): Batch[B]

  def send(data: A) =
    worker.postMessage(data)

  @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
  def receive(callback: (data: B) => Unit) =
    worker.addEventListener(
      "message",
      (e: js.Any) =>
        e match {
          case e: dom.MessageEvent =>
            callback(e.data.asInstanceOf[B])

          case _ => ()
        }
    )

@JSExportTopLevel("DungeonGenWorker")
object DungeonGenWorker extends IndigoWorker[DungeonGenConfig, JsDungeonGameMapTuple] {
  val name: WorkerName = WorkerName("assets/dungeon-gen-worker")

  val useDungeonGenV2: Boolean = true

  def process(config: DungeonGenConfig) = {
    val dice = Dice.fromSeed(config.seed.toLong)
    val dungeonModel =
      if useDungeonGenV2 then
        dungeongen.v2.DungeonGen
          .makeMap(
            dice,
            DungeonRules.MaxRooms,
            DungeonRules.RoomMinSize,
            DungeonRules.RoomMaxSize,
            Size(50, 30),
            DungeonRules.maxMonstersPerRoom(0),
            DungeonRules.maxCollectablesPerRoom(0),
            config.currentLevel
          )
      else
        dungeongen.classic.DungeonGen
          .makeMap(
            dice,
            DungeonRules.MaxRooms,
            DungeonRules.RoomMinSize,
            DungeonRules.RoomMaxSize,
            Size(50, 30),
            DungeonRules.maxMonstersPerRoom(0),
            DungeonRules.maxCollectablesPerRoom(0),
            config.currentLevel
          )

    val gameMapModel =
      GameMap
        .gen(Size(config.width, config.height), dungeonModel)
        .update(dungeonModel.playerStart)

    Batch(new JsDungeonGameMapTuple {
      val dungeon: JsDungeon = JsDungeon.fromDungeon(dungeonModel)
      val gameMap: JsGameMap = JsGameMap.fromGameMap(gameMapModel)
    })
  }
}

opaque type WorkerName = String
object WorkerName:
  inline def apply(WorkerName: String): WorkerName       = WorkerName
  extension (sn: WorkerName) inline def toString: String = sn
  given CanEqual[WorkerName, WorkerName]                 = CanEqual.derived
  given CanEqual[Option[WorkerName], Option[WorkerName]] = CanEqual.derived
