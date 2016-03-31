import NavbarList from '../components/NavbarList'
import R from 'ramda'
import {menuSelect} from '../actions'
import {connect} from 'react-redux'

// Items that will be shown on the top navbar
const items = [
  {
    name: 'Workspace',
    to: '/',
    id: 0,
  },
  {
    name: 'Profile',
    to: '/profile',
    id: 1,
  },
]

const logo = {to: '/', name: 'Benedict'}

const mapStateToProps = (state) => {
  return {
    logo,
    items,
    active: R.prop('menuSelection', state),
  }
}

const mapDispatchToProps = (dispatch) => {
  return {
    onClick: (name) => {
      dispatch(menuSelect(R.toUpper(name)))
    },
  }
}

const Navbar = connect(
  mapStateToProps,
  mapDispatchToProps
)(NavbarList)

export default Navbar
