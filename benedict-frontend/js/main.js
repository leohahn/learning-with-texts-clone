import Cycle from '@cycle/core'
import {makeDOMDriver} from '@cycle/dom'
import {isolate} from '@cycle/isolate'
import {restart, restartable} from 'cycle-restart'
import {makeRouterDriver} from 'cyclic-router'
import {createHashHistory} from 'history'

import App from './components/App'

const history = createHashHistory({queryKey: false})

const drivers = {
  DOM: restartable(makeDOMDriver(`#root`), {pauseSinksWhileReplaying: false}),
  router: makeRouterDriver(history),
}

const {sinks, sources} = Cycle.run(App, drivers)

if (module.hot) {
  module.hot.accept(`./components/App`, () => {
    const app = require(`./components/App`).default
    restart(app, drivers, {sinks, sources}, isolate)
  })
}
