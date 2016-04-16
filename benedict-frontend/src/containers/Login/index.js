import React, { Component } from 'react'

export default class Login extends Component {
  constructor (props) {
    super(props)
    const redirectRoute = this.props.location.query.next || '/login'
    this.state = {
      username: '',
      password: '',
      redirectTo: redirectRoute
    }
  }

  login (e) {
    e.preventDefault()
    this.props.actions.loginUser(this.state.email, this.state.password, this.state.redirectTo)
  }

  render () {
    return (
      42
    )
  }
}
