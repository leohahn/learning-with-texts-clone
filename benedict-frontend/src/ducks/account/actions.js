const LOGIN_SUCCESS = 'benedict/account/LOGIN_SUCCESS'
const LOGIN_FAIL = 'benedict/account/LOGIN_FAIL'
const LOGIN_PROCESSING = 'benedict/account/LOGIN_PROCESSING'
/* Sagas */
export const LOGIN_REQUEST = 'benedict/account/LOGIN_REQUEST'

export const Account = {
  LOGIN_SUCCESS,
  LOGIN_FAIL,
  LOGIN_PROCESSING,
  LOGIN_REQUEST
}

export function accountLoginRequest (username, password) {
  return {
    type: LOGIN_REQUEST,
    body: {
      username,
      password
    }
  }
}

export function accountLoginFail () {
  return {
    type: LOGIN_FAIL
  }
}

export function accountLoginSuccess (userLink, token) {
  return {
    type: LOGIN_SUCCESS,
    userLink,
    token
  }
}

export function accountLoginProcessing () {
  return { type: LOGIN_PROCESSING }
}
