import ReactDOM from 'react-dom'
import { Provider } from 'react-redux'
import configureStore  from './store/configureStore'
import { Router, browserHistory } from 'react-router'
import { syncHistoryWithStore } from 'react-router-redux'
import { div, h } from 'react-hyperscript-helpers'

import routes from './routes';

const store = configureStore()
const rootElement = document.getElementById('app')
const history = syncHistoryWithStore(browserHistory, store)

let ComponentEl;

if (process.env.NODE_ENV !== 'production') {
  const DevTools = require('./containers/DevTools').default;

  // If using routes
  ComponentEl = (
    div([
      h(Router, {history, routes}),
//      h(DevTools),
    ])
  );
} else {
  ComponentEl = (
    div([
      h(Router, {history, routes}),
    ])
  );
}

// Render the React application to the DOM
ReactDOM.render(
  h(Provider, {store}, [
    ComponentEl,
  ]),
  rootElement
);
