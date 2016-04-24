import R from 'ramda'

import { Notify } from './actions'

export default function notifications (state = [], action) {
  switch (action.type) {
    case Notify.COMPLETED:
      return []

    case Notify.LOGIN_SUCCESS:
      return R.append(action, state)

    case Notify.LOGIN_FAIL:
      return R.append(action, state)

    default:
      return state
  }
}
