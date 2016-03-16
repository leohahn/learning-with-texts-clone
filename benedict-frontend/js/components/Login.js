import {Observable} from 'rx'
import {div, h2} from '@cycle/dom'

function Login(props$, sources) {
  return {
    DOM: Observable.of(
      div([
        h2('Login bruuuh'),
      ])
    ),
  }
}

export default Login
