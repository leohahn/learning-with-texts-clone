import R from 'ramda'

import { Store } from 'actions/constants'

export function notifications (state = [], action) {
  switch (action.type) {
    case Store.NOTIFY_COMPLETED:
      return []

    case Store.NOTIFY_LOGIN_SUCCESS:
      return R.append(action, state)

    case Store.NOTIFY_LOGIN_FAIL:
      return R.append(action, state)

    default:
      return state
  }
}
