import { applyMiddleware, createStore } from 'redux'
import sagaMiddleware from 'redux-saga'
import createLogger from 'redux-logger'
import rootReducer from '../reducers'
/* Importing sagas */
import { watchUserLogin } from 'actions/sagas'

export default function configureStore (initialState) {
  const loggerMiddleware = createLogger({
    collapsed: true,
    predicate: () => process.env.NODE_ENV === 'development'
  })

  // Redux uses two middlewares:
  // Logger: Logs the state on the console as it changes.
  // Saga:   Middleware that helps using asyncronous actions.
  const middlewares = applyMiddleware(
    sagaMiddleware(watchUserLogin),
    loggerMiddleware
  )
  // Creates the store with the middlewares defined above.
  const store = createStore(rootReducer, initialState, middlewares)

  if (module.hot) {
    // Enable Webpack hot module replacement for reducers
    module.hot.accept('../reducers', () => {
      const nextRootReducer = require('../reducers').default
      store.replaceReducer(nextRootReducer)
    })
  }

  return store
}
