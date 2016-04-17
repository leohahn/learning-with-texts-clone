import { combineReducers } from 'redux'

import { menu } from 'reducers/menu'
import { user } from 'reducers/user'

const rootReducer = combineReducers({
  menu,
  user
})

export default rootReducer
