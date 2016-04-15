import R from 'ramda'
import { MENU_SELECT } from 'actions/constants'

const initialState = {
  selected: 'PROFILE',
  username: 'lhahn'
}

export function menu (state = initialState, action) {
  switch (action.type) {
    case MENU_SELECT:
      return R.assoc('selected', action.selection, state)

    default:
      return state
  }
}
