import R from 'ramda'

const SELECT = 'benedict/menu/SELECT'

export function menuSelect (selection) {
  return {
    type: SELECT,
    selection
  }
}

const initialState = {
  selected: 'WORKSPACE'
}

export default function menu (state = initialState, action) {
  switch (action.type) {
    case SELECT:
      return R.assoc('selected', action.selection, state)

    default:
      return state
  }
}
