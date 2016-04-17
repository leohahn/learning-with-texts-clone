import React, { Component, PropTypes } from 'react'

import TopMenu from 'containers/TopMenu'

export default class App extends Component {
  static propTypes = {
    children: PropTypes.any
  }

  render () {
    return (
      <div>
        <TopMenu />
        <div>
          {this.props.children}
        </div>
      </div>
    )
  }
}
