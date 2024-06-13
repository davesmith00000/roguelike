package roguelike.subsystems

import dungeongen.workers.IndigoWorker
import indigo.shared.Outcome
import indigo.shared.collections.Batch
import indigo.shared.events.FrameTick
import indigo.shared.events.GlobalEvent
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.subsystems.SubSystem
import indigo.shared.subsystems.SubSystemFrameContext
import indigo.shared.subsystems.SubSystemId
import roguelike.model.Model

import scala.collection.mutable
import scala.scalajs.js

final case class WorkerSubSystem[A <: js.Any, B <: js.Any](
    worker: IndigoWorker[A, B]
) extends SubSystem[Model]:

  type EventType      = GlobalEvent
  type SubSystemModel = Unit
  type ReferenceData  = Unit

  val id: SubSystemId =
    SubSystemId("[WorkerSubSystem] " + worker.name.toString)

  def send(value: A): WorkerEvent.Send =
    WorkerEvent.Send(value)

  private val eventQueue: mutable.Queue[WorkerEvent.Receive] =
    new mutable.Queue[WorkerEvent.Receive]()

  worker.receive((data: B) =>
    eventQueue.enqueue(
      WorkerEvent.Receive(data)
    )
  )

  def eventFilter: GlobalEvent => Option[EventType] =
    case FrameTick      => Some(WorkerSubSystemEnqueue)
    case e: WorkerEvent => Some(e)
    case _              => None

  def reference(model: Model): ReferenceData =
    ()

  def initialModel: Outcome[Unit] =
    Outcome(())

  def update(
      context: SubSystemFrameContext[ReferenceData],
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    case WorkerEvent.Send(value) =>
      worker.send(value)
      Outcome(model)

    case WorkerSubSystemEnqueue =>
      Outcome(model, Batch.fromSeq(eventQueue.dequeueAll(_ => true)))

    case _ =>
      Outcome(model)

  def present(
      context: SubSystemFrameContext[ReferenceData],
      model: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)

  enum WorkerEvent extends GlobalEvent:
    case Send(value: A)    extends WorkerEvent
    case Receive(value: B) extends WorkerEvent

  case object WorkerSubSystemEnqueue extends GlobalEvent
