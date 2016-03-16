import {Observable} from 'rx'
import {span} from '@cycle/dom'
import R from 'ramda'

import {knowledgeToColor} from '../logic/knowledge'

function WordView({DOM}, props$) {
  const {name, knowledge} = props$
  const noPointName = R.replace('.', '', name)
  const wordClass = `.word-${R.toLower(noPointName)}`

  const selected$ = DOM
    .select(wordClass)
    .events('click')
    .map(() => R.toLower(noPointName))
    .startWith('')

  const wordStyle = {
    background: knowledgeToColor[knowledge],
    fontSize: '18px',
  }
  const vTree$ = Observable.of(
    span(wordClass, {style: wordStyle}, [
      name,
    ])
  )
  return {
    DOM: vTree$,
    selected$,
  }
}

export default WordView
