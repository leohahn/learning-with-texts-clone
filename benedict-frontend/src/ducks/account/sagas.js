import { hashHistory } from 'react-router'
import { takeLatest } from 'redux-saga'
import { call, put, apply } from 'redux-saga/effects'

import { login } from 'api'
import { notifyLoginFail, notifyLoginSuccess } from 'ducks/notifications/actions'
import {
  accountLoginProcessing,
  accountLoginSuccess,
  accountLoginFail,
  Account
} from './actions'

function * accountLogin (action) {
  yield put(accountLoginProcessing())
  try {
    const response = yield call(login, action.body)
    if (response.ok) {
      const data = yield apply(response, response.json)
      yield put(accountLoginSuccess(data, response.headers.getAll('cookie-auth')))
      yield put(notifyLoginSuccess(data))
      yield hashHistory.push('/workspace')
    } else {
      const error = yield apply(response, response.text)
      yield put(accountLoginFail())
      yield put(notifyLoginFail(error))
    }
  } catch (error) {
    console.error(error)
  }
}

export function * watchAccountLogin () {
  yield * takeLatest(Account.LOGIN_REQUEST, accountLogin)
}
