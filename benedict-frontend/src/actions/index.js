import { Store, Saga } from 'actions/constants'

export function menuSelect (selection) {
  return {
    type: Store.MENU_SELECT,
    selection
  }
}

export function userLoginRequest (username, password) {
  return {
    type: Saga.USER_LOGIN_REQUEST,
    body: {
      username,
      password
    }
  }
}

export function userLoginFail () {
  return {
    type: Store.USER_LOGIN_FAIL
  }
}

export function userLoginSuccess (userLink, token) {
  return {
    type: Store.USER_LOGIN_SUCCESS,
    userLink,
    token
  }
}

export function notifyCompleted () {
  return {
    type: Store.NOTIFY_COMPLETED
  }
}

export function notifyLoginFail (message) {
  return {
    type: Store.NOTIFY_LOGIN_FAIL,
    message
  }
}

export function notifyLoginSuccess () {
  return {
    type: Store.NOTIFY_LOGIN_SUCCESS
  }
}
