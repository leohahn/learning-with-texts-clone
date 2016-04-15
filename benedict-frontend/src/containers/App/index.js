import React, { Component } from 'react'

import TopMenu from 'containers/TopMenu'

export class App extends Component {
  static propTypes = {
    children: React.PropTypes.any
  }

  render () {
    return (
      <div>
        <TopMenu />
        <h1>This is App</h1>
        <div>
          {this.props.children}
        </div>
      </div>
    )
  }
}
