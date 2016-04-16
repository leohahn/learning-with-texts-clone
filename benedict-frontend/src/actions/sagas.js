import { takeLatest } from 'redux-saga'
import { call, put } from 'redux-saga/effects'

import { Saga, Store } from 'actions/constants'
import { login } from 'api'

export function * userLogin (action) {
  try {
    const data = yield call(login, action.body)
    yield put({ type: Store.USER_LOGIN_SUCCEESS, data })
  } catch (error) {
    yield put({ type: Store.USER_LOGIN_FAIL, error })
  }
}

export function * watchUserLogin () {
  yield * takeLatest(Saga.USER_LOGIN_REQUEST, userLogin)
}
