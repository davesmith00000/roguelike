package roguelike.model.dungeon

enum WallCode:
  case Wall
  case DropOff
  case DropOffFadeLeft
  case DropOffMiddle
  case DropOffFadeRight
  case Cap
  case VerticalWallTop
  case VerticalWallToCeilingTop
  case HorizontalWallMiddle

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
      case "hwm"  => HorizontalWallMiddle
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
        case HorizontalWallMiddle     => "hwm"
