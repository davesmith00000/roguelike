package roguelike

import indigo.shared.datatypes.RGBA

object ColorScheme:

  val white: RGBA = RGBA.White
  val black: RGBA = RGBA.Black
  val red: RGBA   = RGBA.Red

  val playerAttack: RGBA        = RGBA.fromColorInts(0xe0, 0xe0, 0xe0)
  val enemyAttack: RGBA         = RGBA.fromColorInts(0xff, 0xc0, 0xc0)
  val needsTarget: RGBA         = RGBA.fromColorInts(0x3f, 0xff, 0xff)
  val statusEffectApplied: RGBA = RGBA.fromColorInts(0x3f, 0xff, 0x3f)
  val descend: RGBA             = RGBA.fromColorInts(0x9f, 0x3f, 0xff)

  val playerDie: RGBA = RGBA.fromColorInts(0xff, 0x30, 0x30)
  val enemyDie: RGBA  = RGBA.fromColorInts(0xff, 0xa0, 0x30)

  val invalid: RGBA    = RGBA.Yellow
  val impossible: RGBA = RGBA.fromHexString("808080")
  val error: RGBA      = RGBA.fromHexString("FF4040")

  val welcomeText: RGBA     = RGBA.fromHexString("20A0FF")
  val healthRecovered: RGBA = RGBA.Green

  val barText: RGBA   = RGBA.White
  val barFilled: RGBA = RGBA.fromHexString("006000")
  val barEmpty: RGBA  = RGBA.fromHexString("401010")
