import { hashHistory } from 'react-router'
import { takeLatest } from 'redux-saga'
import { call, put, apply } from 'redux-saga/effects'

import { Saga, Store } from 'actions/constants'
import { login } from 'api'
import * as actions from 'actions'

export function * userLogin (action) {
  yield put({ type: Store.USER_LOGIN_PROCESSING })
  try {
    const response = yield call(login, action.body)
    if (response.ok) {
      const data = yield apply(response, response.json)
      yield put(actions.userLoginSuccess(data, response.headers.getAll('cookie-auth')))
      yield put(actions.notifyLoginSuccess(data))
      //yield apply(hashHistory, hashHistory.push, ['/workspace'])
      yield hashHistory.push('/workspace')
    } else {
      const error = yield apply(response, response.text)
      yield put(actions.userLoginFail())
      yield put(actions.notifyLoginFail(error))
    }
  } catch (error) {
    console.error(error)
  }
}

export function * watchUserLogin () {
  yield * takeLatest(Saga.USER_LOGIN_REQUEST, userLogin)
}
