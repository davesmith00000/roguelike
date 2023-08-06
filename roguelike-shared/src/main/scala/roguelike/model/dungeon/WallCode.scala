package roguelike.model.dungeon

// import indigo.*
// import roguelike.model.GameTile.DownStairs
// import roguelike.model.GameTile.Ground
// import roguelike.model.GameTile.Wall
// import roguelike.model.entity.*
// import roguelike.model.gamedata.Armour
// import roguelike.model.gamedata.Consumables
// import roguelike.model.gamedata.Melee
// import roguelike.model.gamedata.Ranged
// import roguelike.model.items.Item
// import roguelike.model.GameTile
// import scala.annotation.tailrec

enum WallCode:
  case Wall
  case DropOff
  case DropOffFadeLeft
  case DropOffMiddle
  case DropOffFadeRight
  case Cap
  case VerticalWallTop
  case VerticalWallToCeilingTop

object WallCode:

  def fromCode(code: String): WallCode =
    code match
      case "d"    => DropOff
      case "dfl"  => DropOffFadeLeft
      case "dm"   => DropOffMiddle
      case "dfr"  => DropOffFadeRight
      case "w"    => Wall
      case "c"    => Cap
      case "vwt"  => VerticalWallTop
      case "vwct" => VerticalWallToCeilingTop
      case _      => Wall

  extension (wc: WallCode)
    def toCode: String =
      wc match
        case Wall                     => "w"
        case DropOff                  => "d"
        case DropOffFadeLeft          => "dfl"
        case DropOffMiddle            => "dm"
        case DropOffFadeRight         => "dfr"
        case Cap                      => "c"
        case VerticalWallTop          => "vwt"
        case VerticalWallToCeilingTop => "vwct"
