import R from 'ramda'
import { Store } from 'actions/constants'

const initialState = {
  token: null,
  href: null,
  data: {
    username: null,
    password: null,
    email: null,
    about: null,
    dicts: null,
    docs: null
  }
}

export function user (state = initialState, action) {
  switch (action.type) {
    case Store.USER_LOGIN:
      const { href, data, token } = action
      return R.merge(state, { href, data, token })

    default:
      return state
  }
}
