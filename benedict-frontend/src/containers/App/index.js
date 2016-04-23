import React, { Component, PropTypes } from 'react'

import TopMenu from 'containers/TopMenu'
import NotificationSystem from 'containers/NotificationSystem'

export default class App extends Component {
  static propTypes = {
    children: PropTypes.any
  }

  render () {
    const maxHeight = { height: '100%' }
    return (
      <div style={maxHeight}>
        <NotificationSystem />
        <TopMenu />
        <div style={maxHeight}>
          {this.props.children}
        </div>
      </div>
    )
  }
}
