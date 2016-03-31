import { routerReducer } from 'react-router-redux'
import { combineReducers } from 'redux'
import R from 'ramda'
import * as type from '../constants/ActionTypes'
import {MenuSelection} from '../constants/AppConstants'

function menuSelection(state = MenuSelection.WORKSPACE, action) {
  switch (action.type) {
    // Change the selection on the top menubar
    case type.MENU_SELECT:
      return action.selection

    default:
      return state
  }
}

function texts(state = [], action) {
  switch (action.type) {
    case type.TEXT_ADD:
      return R.append({
        text: action.text,
        lang: action.lang,
      }, state)

    default:
      return state
  }
}

const rootReducer = combineReducers({
  menuSelection,
  texts,
  routing: routerReducer,
})

export default rootReducer
