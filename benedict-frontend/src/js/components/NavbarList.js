import 'bootstrap/dist/css/bootstrap.min.css'

import {PropTypes} from 'react'
import {a, nav, h, div, ul, li} from 'react-hyperscript-helpers'
import {Link} from 'react-router'
import R from 'ramda'
import NavbarItem from './NavbarItem'

const navbarItems = (items, active, onClick) =>
  R.map((item) => {
    return h(NavbarItem, {item, active, onClick})
  }, items)

const NavbarList = ({logo, items, active, onClick}) =>
    nav({className: 'navbar navbar-default navbar-fixed-top'}, [
      div({className: 'container'}, [
        div('.navbar-header', [
          h(Link, {to: logo.to, className: 'navbar-brand'}, logo.name),
        ]),
        div({className: 'collapse navbar-collapse'}, [
          ul({className: 'nav navbar-nav'}, [
           navbarItems(items, active, onClick),
          ]),

          ul({className: 'nav navbar-nav navbar-right'}, [
            li([ a({href: '', key: 0}, 'Hey Leonardo!')]),
          ]),
        ]),
      ]),
    ])

// Necessary data types ------------------------------------------------
NavbarList.propTypes = {
  logo: PropTypes.shape({
    name: PropTypes.string.isRequired,
    to: PropTypes.string.isRequired,
  }).isRequired,
  items: PropTypes.arrayOf(PropTypes.shape({
    name: PropTypes.string.isRequired,
    to: PropTypes.string.isRequired,
    id: PropTypes.number.isRequired,
  }).isRequired).isRequired,
  onClick: PropTypes.func.isRequired,
  active: PropTypes.string.isRequired,
}

export default NavbarList

