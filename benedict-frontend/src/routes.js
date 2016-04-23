import React from 'react'
import R from 'ramda'
import { Route, IndexRedirect } from 'react-router'

/* containers */
import App from 'containers/App'
import Workspace from 'containers/Workspace'
import Profile from 'containers/Profile'
import Login from 'containers/Login'
import NotFound from 'components/NotFound'

import { store } from 'index'

function authCheck (nextState, replace) {
  const loggedIn = R.path(['account', 'status', 'loggedIn'], store.getState())
  const goingToLogin = nextState.location.pathname === '/login'

  if (goingToLogin && loggedIn) {
    replace('/workspace')
  } else if (!goingToLogin && !loggedIn) {
    replace('/login')
  }
}

export default (
  <Route path='/' component={App}>
    <IndexRedirect to='login' />
    <Route path='workspace' component={Workspace} onEnter={authCheck} />
    <Route path='profile' component={Profile} onEnter={authCheck} />
    <Route path='login' component={Login} onEnter={authCheck} />
    <Route status={404} path='*' component={NotFound} />
  </Route>
)
