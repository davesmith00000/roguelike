package roguelike.model.dungeon

import indigo.*
import roguelike.model.entity.*
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged

import scala.annotation.tailrec

object DungeonRules:

  val RoomMaxSize: Int = 12
  val RoomMinSize: Int = 9
  val MaxRooms: Int    = 30

  final case class Limit(floor: Int, amount: Int)
  final case class Chance(entity: String, weight: Int)

  val maxCollectablesByFloor: List[Limit] = List(
    Limit(0, 1),
    Limit(3, 2)
  )

  val maxMonstersByFloor: List[Limit] = List(
    Limit(0, 2),
    Limit(3, 3),
    Limit(5, 5)
  )

  def maxCollectablesPerRoom(floor: Int): Int =
    maxCollectablesByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def maxMonstersPerRoom(floor: Int): Int =
    maxMonstersByFloor.foldLeft(2) { case (num, limit) =>
      if limit.floor <= floor then limit.amount else num
    }

  def collectableChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Consumables.HealthPotion.name, 35)),
    2 -> List(Chance(Ranged.ConfusionScroll.name, 10)),
    4 -> List(
      Chance(Ranged.LightningScroll.name, 25),
      Chance(Melee.Sword.name, 5)
    ),
    6 -> List(
      Chance(Ranged.FireballScroll.name, 25),
      Chance(Armour.ChainMail.name, 15)
    )
  )

  def enemyChances: Map[Int, List[Chance]] = Map(
    0 -> List(Chance(Orc.name, 80)),
    3 -> List(Chance(Troll.name, 15)),
    5 -> List(Chance(Troll.name, 30)),
    7 -> List(Chance(Troll.name, 60))
  )

  def randomChoices(
      dice: Dice,
      count: Int,
      floor: Int,
      chances: Map[Int, List[Chance]]
  ): List[String] =
    @tailrec
    def select(
        remaining: List[(Int, List[Chance])],
        acc: Map[String, Chance]
    ): List[Chance] =
      remaining match
        case Nil =>
          acc.toList.map(_._2)

        case (flr, _) :: xs if flr > floor =>
          select(Nil, acc)

        case (_, cs) :: xs =>
          select(xs, acc ++ cs.map(c => (c.entity, c)).toMap)

    val possibilities: List[Chance] = select(chances.toList, Map())

    val normalised: List[(String, Double)] =
      val total = possibilities.map(_.weight).sum

      val l = possibilities
        .map(p => (p.entity, p.weight.toDouble / total.toDouble))
        .sortBy(_._2)
        .foldLeft((0.0d, List.empty[(String, Double)])) { case ((total, acc), next) =>
          (total + next._2, acc :+ (next._1, total + next._2))
        }
        ._2

      l.dropRight(1) ++ l.reverse.headOption.map(e => (e._1, 1.0)).toList

    @tailrec
    def pick(remaining: List[(String, Double)], roll: Double): String =
      remaining match
        case Nil =>
          "" // shouldn't happen...

        case (name, chance) :: xs if roll <= chance =>
          name

        case _ :: xs =>
          pick(xs, roll)

    (0 until count).toList.map { _ =>
      pick(normalised, dice.rollDouble)
    }
