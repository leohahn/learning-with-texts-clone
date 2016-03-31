import * as types from '../constants/ActionTypes'

export function menuSelect(selection) {
  return {
    type: types.MENU_SELECT,
    selection,
  }
}

export function textAdd({text, lang}) {
  return {
    lang,
    text,
    type: types.TEXT_ADD,
  }
}
