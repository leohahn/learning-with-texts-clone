import React, { Component } from 'react'
import { connect } from 'react-redux'
import R from 'ramda'

import { notifyCompleted } from 'actions'
import { Store } from 'actions/constants'

@connect(stateToProps, dispatchToProps)
class NotificationSystem extends Component {
  render () {
    return <section style={{display: 'none'}}></section>
  }

  componentDidUpdate () {
    const { notifications } = this.props

    if (notifications.length > 0) {
      this.showAllNotifications()
    }
  }

  showAllNotifications () {
    const showNotification = (action) => {
      switch (action.type) {
        case Store.NOTIFY_LOGIN_SUCCESS:
          UIkit.notify({
            message: 'Login Succeeded!',
            status: 'success',
            timeout: 3000,
            pos: 'top-right'
          })
          break

        case Store.NOTIFY_LOGIN_FAIL:
          UIkit.notify({
            message: action.message,
            status: 'danger',
            timeout: 3000,
            pos: 'top-right'
          })
          break

        default:
          break
      }
    }
    R.forEach(showNotification, this.props.notifications)
    this.props.onComplete()
  }
}

function stateToProps (state) {
  return {
    notifications: state.notifications
  }
}

function dispatchToProps (dispatch) {
  return {
    onComplete () {
      dispatch(notifyCompleted())
    }
  }
}

export default NotificationSystem
