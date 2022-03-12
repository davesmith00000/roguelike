package roguelike.model.entity

import indigo.*
import roguelike.ColorScheme
import roguelike.GameEvent
import roguelike.model.Message
import roguelike.model.gamedata.Ranged

import scala.annotation.tailrec
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

object RangedHelper:

  def useLightningScroll(
      player: Player,
      closestHostile: Option[Hostile]
  ): Outcome[Boolean] =
    closestHostile match
      case None =>
        Outcome(false)
          .addGlobalEvents(
            GameEvent.Log(
              Message(
                "No enemy is close enough to strike.",
                ColorScheme.impossible
              )
            )
          )

      case Some(target) =>
        val msg =
          s"A lighting bolt strikes the ${target.name} with a loud thunder, for ${Ranged.LightningScroll.damage} damage!"
        Outcome(true)
          .addGlobalEvents(
            GameEvent.Log(Message(msg, ColorScheme.playerAttack)),
            GameEvent.PlayerAttack(
              player.name,
              Ranged.LightningScroll.damage,
              target.id
            )
          )

  def useConfusionScroll(
      player: Player,
      target: Hostile
  ): Outcome[Boolean] =
    val msg =
      s"The eyes of the ${target.name} look vacant, as it starts to stumble around!"
    Outcome(true)
      .addGlobalEvents(
        GameEvent.Log(Message(msg, ColorScheme.playerAttack)),
        GameEvent.PlayerCastsConfusion(
          player.name,
          Ranged.ConfusionScroll.turnCount,
          target.id
        )
      )

  def useFireballScroll(
      player: Player,
      hostilesInRange: List[Hostile]
  ): Outcome[Boolean] =
    val events =
      hostilesInRange.flatMap { h =>
        List(
          GameEvent.Log(
            Message(
              s"The ${h.name} is engulfed in a fiery explosion, taking ${Ranged.FireballScroll.damage} damage!",
              ColorScheme.playerAttack
            )
          ),
          GameEvent.PlayerCastsFireball(
            player.name,
            Ranged.FireballScroll.damage,
            h.id
          )
        )
      }.toList

    Outcome(true)
      .addGlobalEvents(events)
