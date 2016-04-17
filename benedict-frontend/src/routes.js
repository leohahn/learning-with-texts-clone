import React from 'react'
import R from 'ramda'
import { Route, IndexRoute } from 'react-router'

/* containers */
import App from 'containers/App'
import Workspace from 'containers/Workspace'
import Profile from 'containers/Profile'
import Login from 'containers/Login'

import { store } from 'index'

function authCheck (nextState, replace) {
  // If the token field has a value, it means that the user is already
  // logged in.
  const loggedIn = R.path(['user', 'token'], store.getState())
  const goingToLogin = nextState.location.pathname === '/login'

  if (goingToLogin && loggedIn) {
    replace('/')
  } else if (!goingToLogin && !loggedIn) {
    replace('/login')
  }
}

export default (
  <Route path='/' component={App}>
    <IndexRoute component={Workspace} onEnter={authCheck} />
    <Route path='profile' component={Profile} onEnter={authCheck} />
    <Route path='login' component={Login} onEnter={authCheck} />
    <Route status={404} path='*' component={Workspace} />
  </Route>
)
