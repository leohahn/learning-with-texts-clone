const COMPLETED = 'benedict/notifications/COMPLETED'
const LOGIN_SUCCESS = 'benedict/notifications/LOGIN_SUCCESS'
const LOGIN_FAIL = 'benedict/notifications/LOGIN_FAIL'

export const Notify = {
  COMPLETED,
  LOGIN_SUCCESS,
  LOGIN_FAIL
}

export function notifyCompleted () {
  return { type: COMPLETED }
}

export function notifyLoginSuccess () {
  return {
    type: LOGIN_SUCCESS
  }
}

export function notifyLoginFail (message) {
  return {
    type: LOGIN_FAIL,
    message
  }
}
