import { Route, IndexRoute, Redirect } from 'react-router'
import { h } from 'react-hyperscript-helpers'

import App from './components/App'
import NotFoundView from './views/NotFoundView'
import ProfileView from './views/ProfileView'
import Workspace from './components/Workspace'

export default (
  h(Route, {path: '/', component: App}, [
    h(IndexRoute, {component: Workspace}),
    h(Route, {path: 'profile', component: ProfileView}),
    h(Route, {path: '404', component: NotFoundView}),
    h(Redirect, {from: '*', to: '404'}),
  ])
)
