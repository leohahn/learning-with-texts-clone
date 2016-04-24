import React, { Component } from 'react'
import { connect } from 'react-redux'

import { menuSelect } from 'ducks/menu'
import Navbar from 'components/Navbar'

const loggedItems = [
  {
    name: 'Workspace',
    selection: 'WORKSPACE',
    to: '/workspace'
  },
  {
    name: 'Profile',
    selection: 'PROFILE',
    to: '/profile'
  }
]

const unloggedItems = [
]

@connect(stateToProps, dispatchToProps)
class TopMenu extends Component {
  render () {
    return (
      <Navbar { ...this.props } />
    )
  }
}

function stateToProps ({ menu, account }) {
  const loggedIn = account.status.loggedIn

  return {
    selected: menu.selected,
    items: loggedIn ? loggedItems : unloggedItems
  }
}

function dispatchToProps (dispatch) {
  return {
    onClick: (selection) => dispatch(menuSelect(selection))
  }
}

export default TopMenu
