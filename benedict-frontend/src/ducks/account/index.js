import R from 'ramda'

import { Account } from './actions'

const initialState = {
  token: null,
  userLink: {
    href: null,
    user: {
      username: null,
      password: null,
      email: null,
      about: null,
      dicts: null,
      docs: null
    }
  },
  status: {
    processing: false,
    loggedIn: false
  }
}

export default function account (state = initialState, action) {
  switch (action.type) {
    case Account.LOGIN_SUCCESS:
      return R.pipe(
        R.assocPath(['status', 'loggedIn'], true),
        R.assocPath(['status', 'processing'], false),
        R.assoc('userLink', action.userLink),
        R.assoc('token', action.token)
      )(state)

    case Account.LOGIN_FAIL:
      return R.pipe(
        R.assocPath(['status', 'loggedIn'], false),
        R.assocPath(['status', 'processing'], false),
        R.assoc('userLink', null),
        R.assoc('token', null)
      )(state)

    case Account.LOGIN_PROCESSING:
      return R.pipe(
        R.assocPath(['status', 'loggedIn'], false),
        R.assocPath(['status', 'processing'], true),
        R.assoc('userLink', null),
        R.assoc('token', null)
      )(state)

    default:
      return state
  }
}
