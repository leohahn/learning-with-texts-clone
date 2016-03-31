import {PropTypes} from 'react'
import {li, h} from 'react-hyperscript-helpers'
import {Link} from 'react-router'
import R from 'ramda'

const NavbarItem = ({item, active, onClick}) => {
  const isActiveItem = R.toUpper(item.name) === active
  const liProps = {
    onClick: () => onClick(item.name),
    key: item.id,
    className: isActiveItem ? 'active' : '',
  }
  return li(liProps, [
    h(Link, {to: item.to}, [item.name]),
  ])
}

NavbarItem.propTypes = {
  item: PropTypes.shape({
    name: PropTypes.string.isRequired,
    to: PropTypes.string.isRequired,
    id: PropTypes.number.isRequired,
  }).isRequired,
  active: PropTypes.string.isRequired,
  onClick: PropTypes.func.isRequired,
}

export default NavbarItem
