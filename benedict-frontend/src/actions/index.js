import { Store } from 'actions/constants'

export function menuSelect (selection) {
  return {
    type: Store.MENU_SELECT,
    selection
  }
}
