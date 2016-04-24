import React, { Component } from 'react'
import { connect } from 'react-redux'
import R from 'ramda'

import LoginPanel from 'components/LoginPanel'
import { accountLoginRequest } from 'ducks/account/actions'

@connect(stateToProps, dispatchToProps)
export default class Login extends Component {
  render () {
    const { onLoginClick, processing } = this.props
    return (
      <LoginPanel onLoginClick={onLoginClick} processing={processing} />
    )
  }
}

function stateToProps (state) {
  return {
    processing: R.path(['account', 'status', 'processing'], state)
  }
}

function dispatchToProps (dispatch) {
  return {
    onLoginClick (username, password) {
      dispatch(accountLoginRequest(username, password))
    }
  }
}
