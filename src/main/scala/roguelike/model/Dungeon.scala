package roguelike.model

import indigo._
import roguelike.model.entity.Collectable
import roguelike.model.entity.Fighter
import roguelike.model.entity.Hostile
import roguelike.model.entity.HostileState
import roguelike.model.entity.Orc
import roguelike.model.entity.Troll
import roguelike.model.gamedata.Armour
import roguelike.model.gamedata.Consumables
import roguelike.model.gamedata.Melee
import roguelike.model.gamedata.Ranged
import roguelike.model.items._

import scala.scalajs.js

import js.JSConverters._

trait Dungeon:
  val playerStart: Point
  val stairsPosition: Point
  val positionedTiles: List[(Point, GameTile)]
  val hostiles: List[Hostile]
  val collectables: List[Collectable]
  val currentFloor: Int

object Dungeon:
  def toJsObj(d: Dungeon) =
    new JsDungeon {
      val playerStart: JsPoint    = JsPoint.fromPoint(d.playerStart)
      val stairsPosition: JsPoint = JsPoint.fromPoint(d.stairsPosition)
      val positionedTiles: js.Array[(JsPoint, JsGameTile)] =
        d.positionedTiles.map {
          _ match {
            case (p, t) => (JsPoint.fromPoint(p), JsGameTile.fromGameTime(t))
          }
        }.toJSArray
      val hostiles: js.Array[JsHostile] =
        d.hostiles.map(JsHostile.fromHostile(_)).toJSArray
      val collectables: js.Array[JsCollectable] = d.collectables.map {
        JsCollectable.fromCollectable(_)
      }.toJSArray
      val currentFloor: Int = d.currentFloor
    }

  def fromJsObj(d: JsDungeon) =
    new Dungeon {
      val playerStart: Point    = JsPoint.toPoint(d.playerStart)
      val stairsPosition: Point = JsPoint.toPoint(d.stairsPosition)
      val positionedTiles: List[(Point, GameTile)] = d.positionedTiles.map {
        _ match {
          case (p, t) =>
            (
              JsPoint.toPoint(p),
              JsGameTile.toGameTile(t)
            )
        }
      }.toList
      val hostiles: List[Hostile] = d.hostiles.map {
        JsHostile.toHostile(_)
      }.toList
      val collectables: List[Collectable] = d.collectables.map {
        JsCollectable.toCollectable(_)
      }.toList
      val currentFloor: Int = d.currentFloor
    }

trait JsDungeon extends js.Object:
  val playerStart: JsPoint
  val stairsPosition: JsPoint
  val positionedTiles: js.Array[(JsPoint, JsGameTile)]
  val hostiles: js.Array[JsHostile]
  val collectables: js.Array[JsCollectable]
  val currentFloor: Int

trait JsPoint extends js.Object:
  val x: Int
  val y: Int

object JsPoint:
  def fromPoint(p: Point) = new JsPoint {
    val x: Int = p.x
    val y: Int = p.y
  }

  def toPoint(p: JsPoint) = Point(p.x, p.y)

trait JsGameTile extends js.Object:
  val name: String

object JsGameTile:
  def fromGameTime(gt: GameTile) =
    new JsGameTile {
      val name: String = gt match {
        case GameTile.Ground     => "g"
        case GameTile.DownStairs => "d"
        case GameTile.Wall       => "w"
      }
    }

  def toGameTile(gt: JsGameTile) = gt.name match {
    case "g" => GameTile.Ground
    case "d" => GameTile.DownStairs
    case _   => GameTile.Wall
  }

trait JsHostile extends js.Object:
  val id: Int
  val name: String
  val position: JsPoint
  val isAlive: Boolean
  val fighter: JsFighter
  val movePath: js.Array[JsPoint]
  val state: JsHostileState

object JsHostile:
  def fromHostile(h: Hostile) =
    new JsHostile {
      val id: Int            = h.id
      val name: String       = h.name
      val position: JsPoint  = JsPoint.fromPoint(h.position)
      val isAlive: Boolean   = h.isAlive
      val fighter: JsFighter = JsFighter.fromFighter(h.fighter)
      val movePath: js.Array[JsPoint] = h.movePath.map {
        JsPoint.fromPoint(_)
      }.toJSArray
      val state: JsHostileState = JsHostileState.fromHostileState(h.state)
    }

  def toHostile(h: JsHostile) =
    val position = JsPoint.toPoint(h.position)
    val fighter  = JsFighter.toFighter(h.fighter)
    val state    = JsHostileState.toHostileState(h.state)
    val movePath = h.movePath.map(JsPoint.toPoint(_)).toList
    h.name match {
      case Orc.name =>
        Orc(h.id, position, h.isAlive, fighter, movePath, state)

      case Troll.name =>
        Troll(h.id, position, h.isAlive, fighter, movePath, state)
    }

trait JsFighter extends js.Object:
  val hp: Int
  val maxHp: Int
  val defense: Int
  val power: Int

object JsFighter:
  def fromFighter(f: Fighter) = new JsFighter {
    val hp: Int      = f.hp
    val maxHp: Int   = f.maxHp
    val defense: Int = f.defense
    val power: Int   = f.power
  }

  def toFighter(f: JsFighter) = Fighter(
    f.hp,
    f.maxHp,
    f.defense,
    f.power
  )

trait JsCollectable extends js.Object:
  val position: JsPoint
  val item: JsItem

object JsCollectable:
  def fromCollectable(c: Collectable) =
    new JsCollectable {
      val position: JsPoint = JsPoint.fromPoint(c.position)
      val item: JsItem      = JsItem.fromItem(c.item)
    }

  def toCollectable(c: JsCollectable) =
    Collectable(JsPoint.toPoint(c.position), JsItem.toItem(c.item))

trait JsHostileState extends js.Object:
  val name: String
  val remaining: Int

object JsHostileState:
  def fromHostileState(hs: HostileState) =
    hs match {
      case HostileState.Normal =>
        new JsHostileState {
          val name: String   = "normal"
          val remaining: Int = 0
        }
      case HostileState.Confused(r) =>
        new JsHostileState {
          val name: String   = "confused"
          val remaining: Int = r
        }
    }
  def toHostileState(hs: JsHostileState) =
    hs.name match {
      case "confused" => HostileState.Confused(hs.remaining)
      case _          => HostileState.Normal
    }

trait JsItem extends js.Object:
  val name: String

object JsItem:
  def fromItem(i: Item) = new JsItem {
    val name: String = i.name
  }

  def toItem(i: JsItem) =
    i.name match {
      case Armour.ChainMail.name         => Armour.ChainMail
      case Armour.LeatherArmor.name      => Armour.LeatherArmor
      case Consumables.HealthPotion.name => Consumables.HealthPotion
      case Melee.Dagger.name             => Melee.Dagger
      case Melee.Sword.name              => Melee.Sword
      case Ranged.ConfusionScroll.name   => Ranged.ConfusionScroll
      case Ranged.FireballScroll.name    => Ranged.FireballScroll
      case Ranged.LightningScroll.name   => Ranged.LightningScroll
      case _ =>
        new Item {
          val name: String = i.name
        }
    }
