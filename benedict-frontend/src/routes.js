import React from 'react'
import { Route, IndexRoute } from 'react-router'

/* containers */
import { App } from 'containers/App'
import { Workspace } from 'containers/Workspace'
import { Profile } from 'containers/Profile'

export default (
  <Route path="/" component={App}>
    <IndexRoute component={Workspace} />
    <Route path="profile" component={Profile} />
    <Route status={404} path="*" component={Workspace} />
  </Route>
)
