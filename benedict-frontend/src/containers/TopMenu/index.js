import React, { Component } from 'react'
import { connect } from 'react-redux'

import { menuSelect } from 'actions'
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

const mapStateToProps = ({ menu, account }) => {
  const loggedIn = account.status.loggedIn

  return {
    selected: menu.selected,
    items: loggedIn ? loggedItems : unloggedItems
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onClick: (selection) => dispatch(menuSelect(selection))
  }
}

@connect(mapStateToProps, mapDispatchToProps)
export default class TopMenu extends Component {
  render () {
    return (
      <Navbar { ...this.props } />
    )
  }
}
