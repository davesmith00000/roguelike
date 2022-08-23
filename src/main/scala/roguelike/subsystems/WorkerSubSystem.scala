package roguelike.subsystems

import indigo.shared.Outcome
import indigo.shared.collections.Batch
import indigo.shared.events.FrameTick
import indigo.shared.events.GlobalEvent
import indigo.shared.scenegraph.SceneUpdateFragment
import indigo.shared.subsystems.SubSystem
import indigo.shared.subsystems.SubSystemFrameContext
import indigo.shared.subsystems.SubSystemId
import org.scalajs.dom
import org.scalajs.dom.Event
import org.scalajs.dom.Worker

import scala.annotation.nowarn.apply
import scala.collection.mutable
import scala.scalajs.js

final case class WorkerSubSystem[A <: js.Object, B <: js.Object](
    worker: Worker
) extends SubSystem:

  val id: SubSystemId =
    SubSystemId(worker.hashCode.toString)

  type EventType      = GlobalEvent
  type SubSystemModel = Unit

  def send(value: A): WorkerEvent.Send =
    WorkerEvent.Send(value)

  private val eventQueue: mutable.Queue[WorkerEvent.Receive] =
    new mutable.Queue[WorkerEvent.Receive]()

  worker.onmessage = (e: js.Any) =>
    e match {
      case e: dom.MessageEvent =>
        eventQueue.enqueue(WorkerEvent.Receive(e.data.asInstanceOf[B]))

      case _ => eventQueue
    }

  def eventFilter: GlobalEvent => Option[EventType] =
    case FrameTick      => Some(WorkerSubSystemEnqueue)
    case e: WorkerEvent => Some(e)
    case _              => None

  def initialModel: Outcome[Unit] =
    Outcome(())

  def update(
      context: SubSystemFrameContext,
      model: Unit
  ): GlobalEvent => Outcome[Unit] =
    case WorkerEvent.Send(value) =>
      worker.postMessage(value)
      Outcome(model)

    case WorkerSubSystemEnqueue =>
      Outcome(model, Batch.fromSeq(eventQueue.dequeueAll(_ => true)))

    case _ =>
      Outcome(model)

  def present(
      context: SubSystemFrameContext,
      model: Unit
  ): Outcome[SceneUpdateFragment] =
    Outcome(SceneUpdateFragment.empty)

  enum WorkerEvent extends GlobalEvent:
    case Send(value: A)    extends WorkerEvent
    case Receive(value: B) extends WorkerEvent

  case object WorkerSubSystemEnqueue extends GlobalEvent
