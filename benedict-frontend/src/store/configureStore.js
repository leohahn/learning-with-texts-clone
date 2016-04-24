import { applyMiddleware, createStore } from 'redux'
import sagaMiddleware from 'redux-saga'
import createLogger from 'redux-logger'
import rootReducer from '../rootReducer'
/* Importing sagas */
import { watchAccountLogin } from 'ducks/account/sagas'

export default function configureStore (initialState) {
  const loggerMiddleware = createLogger({
    collapsed: true,
    predicate: () => process.env.NODE_ENV === 'development'
  })

  // Redux uses two middlewares:
  // Logger: Logs the state on the console as it changes.
  // Saga:   Middleware that helps using asyncronous actions.
  const middlewares = applyMiddleware(
    sagaMiddleware(watchAccountLogin),
    loggerMiddleware
  )
  // Creates the store with the middlewares defined above.
  const store = createStore(rootReducer, initialState, middlewares)

  if (module.hot) {
    // Enable Webpack hot module replacement for reducers
    module.hot.accept('../rootReducer', () => {
      const nextRootReducer = require('../rootReducer').default
      store.replaceReducer(nextRootReducer)
    })
  }

  return store
}
