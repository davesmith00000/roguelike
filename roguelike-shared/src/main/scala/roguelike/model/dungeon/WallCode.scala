package roguelike.model.dungeon

enum WallCode:
  case Wall
  case DropOff
  case DropOffFadeLeft
  case DropOffMiddle
  case DropOffFadeRight

object WallCode:

  def fromCode(code: String): WallCode =
    code match
      case "d"   => DropOff
      case "dfl" => DropOffFadeLeft
      case "dm"  => DropOffMiddle
      case "dfr" => DropOffFadeRight
      case "w"   => Wall
      case _     => Wall

  extension (wc: WallCode)
    def toCode: String =
      wc match
        case Wall             => "w"
        case DropOff          => "d"
        case DropOffFadeLeft  => "dfl"
        case DropOffMiddle    => "dm"
        case DropOffFadeRight => "dfr"
