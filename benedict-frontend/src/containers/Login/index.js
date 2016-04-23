import React, { Component } from 'react'
import { connect } from 'react-redux'

import LoginPanel from 'components/LoginPanel'
import { userLoginRequest } from 'actions'

@connect(mapStateToProps, mapDispatchToProps)
export default class Login extends Component {
  render () {
    const { onLoginClick, processing } = this.props
    return (
      <LoginPanel onLoginClick={onLoginClick} processing={processing} />
    )
  }
}

function mapStateToProps (state) {
  return {
    processing: state.account.status.processing
  }
}

function mapDispatchToProps (dispatch) {
  return {
    onLoginClick (username, password) {
      dispatch(userLoginRequest(username, password))
    }
  }
}
