package roguelike.model

import indigo.shared.collections.Batch
import indigo.shared.datatypes.Point
import indigo.shared.datatypes.RGBA
import indigo.shared.datatypes.Size
import indigo.shared.dice.Dice
import io.circe.*
import io.circe.parser.decode
import io.circe.syntax.*
import roguelike.model.dungeon.WallCode
import roguelike.model.entity.Collectable
import roguelike.model.entity.Fighter
import roguelike.model.entity.HostileState
import roguelike.model.entity.Orc
import roguelike.model.entity.Player
import roguelike.model.entity.Troll
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items.Item

class ModelSaveDataTests extends munit.FunSuite:

  test("placeholder") {
    assert(1 == 1)
  }

  import SharedCodecs.given

  test("json - all data") {
    val actual =
      ModelSaveData.fromJsonString(ModelSaveDataSamples.saveData.toJsonString)

    val expected =
      Some(ModelSaveDataSamples.saveData)

    assertEquals(actual.toOption, expected)
  }

  test("json - inventory") {
    val actual =
      decode[Inventory](ModelSaveDataSamples.inventory.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.inventory)

    assertEquals(actual, expected)
  }

  test("json - Collectable") {
    val collectable = Collectable(Point(1), Consumables.HealthPotion)

    val actual =
      decode[Collectable](collectable.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(collectable)

    assertEquals(actual, expected)
  }

  test("json - collectables") {
    val collectable = Collectable(Point(1), Consumables.HealthPotion)

    val actual =
      decode[Collectable](collectable.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(collectable)

    assertEquals(actual, expected)
  }

  test("json - gameMap") {
    val actual =
      decode[GameMap](ModelSaveDataSamples.gameMap.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.gameMap)

    assertEquals(actual, expected)
  }

  test("json - player") {
    val actual =
      decode[Player](ModelSaveDataSamples.player.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.player)

    assertEquals(actual, expected)
  }

  test("json - messageLog") {
    val actual =
      decode[MessageLog](ModelSaveDataSamples.messageLog.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.messageLog)

    assertEquals(actual, expected)
  }

  test("json - size") {
    val actual =
      decode[Size](ModelSaveDataSamples.size.asJson.noSpaces) match
        case Right(d) => Some(d)
        case Left(e) =>
          println(e)
          None

    val expected =
      Some(ModelSaveDataSamples.size)

    assertEquals(actual, expected)
  }

object ModelSaveDataSamples:

  val inventory: Inventory =
    Inventory(
      Backpack.initial,
      Equipment.initial
    )

  val gameMap: GameMap =
    GameMap(
      Size(80, 50),
      Batch(Option(GameTile.Wall(WallCode.Wall)), Option(GameTile.Ground(0))),
      Batch(Point(1), Point(2), Point(3)),
      Set(Point(1), Point(2), Point(3), Point(4))
    )

  val size: Size =
    Size(80, 50)

  val player: Player =
    Player(
      Point(5, 6),
      true,
      Fighter(1, 2, 3, 4),
      inventory,
      2,
      350
    )

  val stairsPosition: Point =
    Point(10, 10)

  val messageLog: MessageLog =
    MessageLog(
      List(
        Message("a", RGBA.Red),
        Message("b", RGBA.Green),
        Message("c", RGBA.Blue)
      ),
      None
    )

  val currentLevel: Int =
    1

  val saveData: ModelSaveData =
    ModelSaveData(
      player,
      stairsPosition,
      gameMap,
      messageLog,
      currentLevel,
      List(Collectable(Point(1), Consumables.HealthPotion)),
      List(Orc.spawn(0, Point(0)))
    )
