import { combineReducers } from 'redux'

import { menu } from 'reducers/menu'
import { account } from 'reducers/account'
import { notifications } from 'reducers/notifications'

const rootReducer = combineReducers({
  menu,
  account,
  notifications
})

export default rootReducer
