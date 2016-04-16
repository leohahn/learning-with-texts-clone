import R from 'ramda'
import { Store } from 'actions/constants'

const initialState = {
  selected: 'WORKSPACE',
  username: 'lhahn'
}

export function menu (state = initialState, action) {
  switch (action.type) {
    case Store.MENU_SELECT:
      return R.assoc('selected', action.selection, state)

    default:
      return state
  }
}
