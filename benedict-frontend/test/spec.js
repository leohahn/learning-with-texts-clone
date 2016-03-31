/* eslint no-undef: 0 */
import {expect} from 'chai'
import * as actions from '../src/js/actions'
import * as types from '../src/js/constants/ActionTypes'
import {MenuSelection, Language} from '../src/js/constants/AppConstants'

describe('actions', () => {
  it('should create an action to change the menu selection', () => {
    const selection = MenuSelection.WORKSPACE
    const expectedAction = {
      type: types.MENU_SELECT,
      selection,
    }
    expect(actions.menuSelect(selection)).to.eql(expectedAction)
  })

  it('should create an action to add a new text', () => {
    const newText = {
      lang: Language.ENGLISH,
      text: '',
    }
    const expectedAction = {
      type: types.TEXT_ADD,
      lang: newText.lang,
      text: newText.text,
    }
    expect(actions.textAdd(newText)).to.eql(expectedAction)
  })
})
