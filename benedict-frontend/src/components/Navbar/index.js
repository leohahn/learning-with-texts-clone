import React, { PropTypes } from 'react'
import { Link } from 'react-router'

const NavbarItem = ({ children, active, to }) =>
  <li className={active ? 'uk-active' : ''}>
    <Link to={to}> {children} </Link>
  </li>

const Navbar = ({ onClick, selected, items }) => {
  return (
    <nav className='uk-navbar'>
      <Link to='/' className='uk-navbar-brand'>
        Benedict
      </Link>
      <ul className='uk-navbar-nav'>
        {items.map((item) =>
          <NavbarItem active={item.selection === selected}
                      to={item.to} onClick={onClick}>
            {item.name}
          </NavbarItem>
        )}
      </ul>
    </nav>
  )
}

Navbar.propTypes = {
  onClick: PropTypes.func.isRequired,
  selected: PropTypes.string.isRequired,
  items: PropTypes.arrayOf(PropTypes.shape({
    name: PropTypes.string.isRequired,
    selection: PropTypes.string.isRequired,
    to: PropTypes.string.isRequired
  }).isRequired).isRequired
}

export default Navbar
