import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import { menuSelect } from 'actions'
import Navbar from 'components/Navbar'

const mapStateToProps = ({ menu }) => {
  return {
    active: menu.selected
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onSelect: (itemName) => dispatch(menuSelect(itemName))
  }
}

@connect(mapStateToProps, mapDispatchToProps)
export default class TopMenu extends Component {
  render () {
    return (
      <Navbar {...this.props} />
    )
  }
}
