import React, { PropTypes } from 'react'
import { Link } from 'react-router'

const Navbar = ({ onSelect, active }) =>
  <nav className='uk-navbar'>
    <Link to='/' className='uk-navbar-brand'>Benedict</Link>
    <ul className='uk-navbar-nav'>
      <li className={active === 'WORKSPACE' ? 'uk-active' : ''}
          onClick={() => onSelect('WORKSPACE')}>
        <Link to='/'>Workspace</Link>
      </li>
      <li className={active === 'PROFILE' ? 'uk-active' : ''}
          onClick={() => onSelect('PROFILE')}>
        <Link to='/profile'>Profile</Link>
      </li>
    </ul>
  </nav>

Navbar.propTypes = {
  onSelect: PropTypes.func.isRequired,
  active: PropTypes.string.isRequired
}

export default Navbar
