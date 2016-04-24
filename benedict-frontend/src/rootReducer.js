import { combineReducers } from 'redux'

import account from 'ducks/account'
import menu from 'ducks/menu'
import notifications from 'ducks/notifications/index.js'

export default combineReducers({
  account,
  menu,
  notifications
})
