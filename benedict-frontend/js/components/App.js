import {div} from '@cycle/dom'
// Application's Components
import Login from './Login'
import NotFound from './NotFound'
import Profile from './Profile'
import Topbar from './Topbar'
import Workspace from './Workspace'

// Routes of the whole application with their respective components
const routes = {
  '/': Workspace,
  '/profile': Profile,
  '/login': Login,
  '*': NotFound,
}

function view(topbar, children) {
  return div([
    topbar,
    div('#main-content', {className: 'pusher'}, [children]),
  ])
}

function App(sources) {
  const {router} = sources
  const match$ = router.define(routes)

  const topbar = Topbar(sources, match$.pluck('path'))

  const childrenVTree$ = match$.map(({path, value}) =>
    value({...sources, router: router.path(path)}).DOM)

  return {
    DOM: topbar.DOM.combineLatest(childrenVTree$, view),
  }
}

export default App
