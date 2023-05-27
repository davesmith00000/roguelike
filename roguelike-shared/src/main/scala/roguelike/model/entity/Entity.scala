package roguelike.model.entity

import indigo.Point

trait Entity:
  def position: Point
  def blocksMovement: Boolean
  def name: String
