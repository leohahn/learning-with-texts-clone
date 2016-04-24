import React, { PropTypes, Component } from 'react'

import styles from './styles.css'

export default class LoginPanel extends Component {
  static propTypes = {
    onLoginClick: PropTypes.func.isRequired,
    processing: PropTypes.bool.isRequired
  }

  constructor (props) {
    super(props)
    this.state = {
      username: '',
      password: ''
    }
  }

  render () {
    const { onLoginClick, processing } = this.props
    const { username, password } = this.state
    const buttonText = processing ? 'Logging in...' : 'Login'
    return (
      <div className={'uk-flex uk-flex-middle uk-flex-center'} style={{height: '100%'}}>
        <form className={'uk-form ' + styles.panel}>
          <legend style={{textAlign: 'center'}}>
            Login to Benedict
          </legend>
          <div className='uk-grid'>
            <div className='uk-width-1-1'>
              <input value={username} onChange={(e) => this.onChange('username', e)}
                     type='text' placeholder='Username' className='uk-width-1-1' />
            </div>
            <div className='uk-width-1-1'>
              <input value={password} onChange={(e) => this.onChange('password', e)}
                     type='password' placeholder='password' className='uk-width-1-1' />
            </div>
            <div className='uk-width-1-1'>
              <button onClick={() => onLoginClick(username, password)}
                      className='uk-button uk-button-success uk-width-1-1'>
                { buttonText }
              </button>
            </div>
          </div>
        </form>
      </div>
    )
  }

  onChange (what, e) {
    if (what === 'username') {
      this.setState({ username: e.target.value })
    } else if (what === 'password') {
      this.setState({ password: e.target.value })
    } else {
      throw new Error('Wrong argument to onChange in LoginPanel')
    }
  }
}

export default LoginPanel
