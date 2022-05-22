package roguelike.model.entity

import indigo._
import io.circe._
import io.circe.syntax._
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.model.GameMap
import roguelike.model.Message
import roguelike.model.SharedCodecs

sealed trait Hostile extends Entity:
  val name: String
  def id: Int
  def movePath: List[Point]
  def moveTo(newPosition: Point): Hostile
  def withFighter(newFighter: Fighter): Hostile
  def markAsDead(isDead: Boolean): Hostile
  def state: HostileState
  def isConfused: Boolean
  def confuseFor(turns: Int): Outcome[Hostile]
  def nextState: Hostile
  def xpGiven: Int
  def isAlive: Boolean
  def fighter: Fighter

  def takeDamage(amount: Int): Outcome[Hostile] =
    val f = fighter.takeDamage(amount)
    Outcome(
      this
        .withFighter(f)
        .markAsDead(if f.hp > 0 then false else true)
    ).addGlobalEvents(
      if f.hp <= 0 then
        Batch(
          GameEvent.Log(Message(s"You killed a $name", ColorScheme.enemyDie)),
          GameEvent.HostileGiveXP(xpGiven)
        )
      else Batch.empty
    )

object Hostile:

  import SharedCodecs.given

  given Encoder[Hostile] = new Encoder[Hostile] {
    final def apply(data: Hostile): Json = Json.obj(
      ("name", data.name.asJson),
      ("id", data.id.asJson),
      ("position", data.position.asJson),
      ("isAlive", data.isAlive.asJson),
      ("fighter", data.fighter.asJson),
      ("movePath", data.movePath.asJson),
      ("state", data.state.asJson)
    )
  }

  given Decoder[Hostile] = new Decoder[Hostile] {
    final def apply(c: HCursor): Decoder.Result[Hostile] =
      c.downField("name").as[String].flatMap {
        case Orc.name =>
          for {
            id       <- c.downField("id").as[Int]
            position <- c.downField("position").as[Point]
            isAlive  <- c.downField("isAlive").as[Boolean]
            fighter  <- c.downField("fighter").as[Fighter]
            movePath <- c.downField("movePath").as[List[Point]]
            state    <- c.downField("state").as[HostileState]
          } yield Orc(id, position, isAlive, fighter, movePath, state)

        case Troll.name =>
          for {
            id       <- c.downField("id").as[Int]
            position <- c.downField("position").as[Point]
            isAlive  <- c.downField("isAlive").as[Boolean]
            fighter  <- c.downField("fighter").as[Fighter]
            movePath <- c.downField("movePath").as[List[Point]]
            state    <- c.downField("state").as[HostileState]
          } yield Troll(id, position, isAlive, fighter, movePath, state)
      }
  }

final case class Orc(
    id: Int,
    position: Point,
    isAlive: Boolean,
    fighter: Fighter,
    movePath: List[Point],
    state: HostileState
) extends Hostile:
  val blocksMovement: Boolean = isAlive
  val name: String            = Orc.name
  val xpGiven: Int            = 35

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Orc] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Orc =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Orc =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Orc =
    this.copy(isAlive = !isDead)

  def confuseFor(turns: Int): Outcome[Orc] =
    Outcome(this.copy(state = HostileState.Confused(turns)))

  def isConfused: Boolean =
    state.isConfused

  def nextState: Orc =
    this.copy(state = state.next)

object Orc:
  val name: String = "Orc"

  def spawn(id: Int, start: Point): Orc =
    Orc(id, start, true, Fighter(10, 0, 3), Nil, HostileState.Normal)

final case class Troll(
    id: Int,
    position: Point,
    isAlive: Boolean,
    fighter: Fighter,
    movePath: List[Point],
    state: HostileState
) extends Hostile:
  val blocksMovement: Boolean = isAlive
  val name: String            = Troll.name
  val xpGiven: Int            = 100

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Troll] =
    Outcome(this.copy(position = position + amount))

  def moveTo(newPosition: Point): Troll =
    this.copy(position = newPosition)

  def withFighter(newFighter: Fighter): Troll =
    this.copy(fighter = newFighter)

  def markAsDead(isDead: Boolean): Troll =
    this.copy(isAlive = !isDead)

  def confuseFor(turns: Int): Outcome[Troll] =
    Outcome(this.copy(state = HostileState.Confused(turns)))

  def isConfused: Boolean =
    state.isConfused

  def nextState: Troll =
    this.copy(state = state.next)

object Troll:
  val name: String = "Troll"

  def spawn(id: Int, start: Point): Troll =
    Troll(id, start, true, Fighter(16, 1, 4), Nil, HostileState.Normal)
