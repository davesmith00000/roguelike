package roguelike.components.windows

import indigo.Key

enum WindowManagerCommand:
  case ShowQuit
  case ShowLevelUp
  case ShowDropMenu
  case ShowEquipMenu
  case ShowInventoryMenu
  // case ShowHistory
  case CloseAll
  case HandleQuitKeyPress
  case DelegateInput(key: Key)
