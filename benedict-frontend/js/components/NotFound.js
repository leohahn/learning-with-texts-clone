import {Observable} from 'rx'
import {div, h2} from '@cycle/dom'

function NotFound() {
  return {
    DOM: Observable.of(
      div([
        h2('This page cannot be found.'),
      ])
    ),
  }
}

export default NotFound
