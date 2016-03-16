import {Observable} from 'rx'
import {div, h2} from '@cycle/dom'

function Profile(props$, sources) {
  return {
    DOM: Observable.of(
      div([
        h2('Profile bruuuh'),
      ])
    ),
  }
}

export default Profile
