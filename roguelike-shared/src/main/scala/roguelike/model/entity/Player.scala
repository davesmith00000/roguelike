package roguelike.model.entity

import indigo.*
import io.circe.*
import io.circe.syntax.*
import io.indigoengine.roguelike.starterkit.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.InventoryEvent
import roguelike.model.Equipment
import roguelike.model.GameMap
import roguelike.model.HostilesPool
import roguelike.model.Inventory
import roguelike.model.Message
import roguelike.model.SharedCodecs
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Melee
import roguelike.model.items.Item

final case class Player(
    position: Point,
    isAlive: Boolean,
    fighter: Fighter,
    inventory: Inventory,
    level: Int,
    xp: Int
) extends Entity:
  val blocksMovement: Boolean = false
  val name: String            = "Player"

  def pickUp(
      worldCollectables: Batch[Collectable]
  ): Outcome[(Player, Batch[Collectable])] =
    worldCollectables.find(_.position == position) match
      case None =>
        Outcome(
          (this, worldCollectables),
          Batch(
            GameEvent.Log(Message.thereIsNothingHereToPickUp)
          )
        )

      case Some(collectable) if inventory.backpack.isFull =>
        Outcome(
          (this, worldCollectables),
          Batch(
            GameEvent.Log(
              Message("Your inventory is full.", ColorScheme.impossible)
            )
          )
        )

      case Some(collectable) =>
        Outcome(
          (
            this.copy(inventory = inventory.add(collectable.item)),
            worldCollectables.filterNot(_.position == position)
          ),
          Batch(
            GameEvent.Inventory(InventoryEvent.PickedUp(collectable.item)),
            GameEvent
              .Log(
                Message(
                  s"You picked up the ${collectable.item.name}!",
                  RGBA(0.0, 0.85, 0.0)
                )
              ),
            GameEvent.PlayerTurnEnd
          )
        )

  def bump(
      amount: Point,
      gameMap: GameMap,
      hostiles: HostilesPool
  ): Outcome[Player] =
    hostiles.findBlockingByPosition(position + amount) match
      case None =>
        moveBy(amount, gameMap)

      case Some(target) =>
        Outcome(this)
          .addGlobalEvents(
            GameEvent.PlayerAttack(
              name,
              fighter.power + inventory.equipment.powerBonus,
              target.id
            ),
            GameEvent.PlayerTurnEnd
          )

  def moveBy(amount: Point, gameMap: GameMap): Outcome[Player] =
    gameMap.lookUp(position + amount) match
      case None =>
        Outcome(this).addGlobalEvents(GameEvent.Log(Message.thatWayIsBlocked))

      case Some(tile) if tile.isBlocked =>
        Outcome(this).addGlobalEvents(GameEvent.Log(Message.thatWayIsBlocked))

      case Some(tile) =>
        Outcome(this.copy(position = position + amount))
          .addGlobalEvents(GameEvent.PlayerTurnEnd)

  def takeDamage(amount: Int): Player =
    val f = fighter.takeDamage(amount)
    this.copy(
      fighter = f,
      isAlive = if f.hp > 0 then true else false
    )

  def heal(amount: Int): Player =
    this.copy(
      fighter = fighter.heal(amount)
    )

  val experienceToNextLevel: Int =
    Player.LevelUpBase + level * Player.LevelUpFactor

  def addXp(additionalXp: Int): Outcome[Player] =
    if xp == 0 then Outcome(this)
    else
      val next    = xp + additionalXp
      val levelUp = next > experienceToNextLevel

      Outcome(
        this.copy(
          xp = if levelUp then next - experienceToNextLevel else next,
          level = if levelUp then level + 1 else level
        )
      ).addGlobalEvents(
        GameEvent.Log(
          Message(s"You gain $additionalXp experience points.", RGBA.White)
        )
      ).createGlobalEvents(p =>
        if levelUp then
          Batch(
            GameEvent
              .Log(Message(s"You advance to level ${p.level}!", RGBA.White))
          )
        else Batch.empty
      )

  def increaseMaxHp(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          hp = fighter.hp + amount,
          maxHp = fighter.maxHp + amount
        )
      )
    ).addGlobalEvents(
      GameEvent.Log(Message("Your health improves!", RGBA.White))
    )

  def increasePower(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          power = fighter.power + amount
        )
      )
    ).addGlobalEvents(GameEvent.Log(Message("You feel stronger!", RGBA.White)))

  def increaseDefense(amount: Int): Outcome[Player] =
    Outcome(
      this.copy(
        fighter = fighter.copy(
          defense = fighter.defense + amount
        )
      )
    ).addGlobalEvents(
      GameEvent.Log(Message("Your movements are getting swifter!", RGBA.White))
    )

object Player:
  val LevelUpBase: Int   = 200
  val LevelUpFactor: Int = 150

  def initial(dice: Dice, start: Point): Player =
    Player(
      start,
      true,
      Fighter(30, 1, 2),
      Inventory.initial,
      1,
      LevelUpBase
    )

  import SharedCodecs.given

  given Encoder[Player] = new Encoder[Player] {
    final def apply(data: Player): Json =
      Json.obj(
        ("position", data.position.asJson),
        ("isAlive", Json.fromBoolean(data.isAlive)),
        ("fighter", data.fighter.asJson),
        ("inventory", data.inventory.asJson),
        ("level", data.level.asJson),
        ("xp", data.xp.asJson)
      )
  }

  given Decoder[Player] = new Decoder[Player] {
    final def apply(c: HCursor): Decoder.Result[Player] =
      for {
        position  <- c.downField("position").as[Point]
        isAlive   <- c.downField("isAlive").as[Boolean]
        fighter   <- c.downField("fighter").as[Fighter]
        inventory <- c.downField("inventory").as[Inventory]
        level     <- c.downField("level").as[Int]
        xp        <- c.downField("xp").as[Int]
      } yield Player(
        position,
        isAlive,
        fighter,
        inventory,
        level,
        xp
      )
  }
