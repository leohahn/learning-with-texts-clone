import { MENU_SELECT } from 'actions/constants'

export function menuSelect (selection) {
  return {
    type: MENU_SELECT,
    selection
  }
}
