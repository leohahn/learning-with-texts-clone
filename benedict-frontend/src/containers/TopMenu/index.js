import React, { Component } from 'react'
import { connect } from 'react-redux'

import { menuSelect } from 'actions'
import Navbar from 'components/Navbar'

const loggedItems = [
  {
    name: 'Workspace',
    selection: 'WORKSPACE',
    to: '/'
  },
  {
    name: 'Profile',
    selection: 'PROFILE',
    to: '/profile'
  }
]

const unloggedItems = [
]

const mapStateToProps = ({ menu, user }) => {
  const loggedIn = user.token !== null

  return {
    selected: menu.selected,
    items: loggedIn ? loggedItems : unloggedItems
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onClick: (itemName) => dispatch(menuSelect(itemName))
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
