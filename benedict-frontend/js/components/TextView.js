import {Observable} from 'rx'
import {div} from '@cycle/dom'
import R from 'ramda'

import WordView from './WordView'
import {KNOWLEDGE, getKnowledge} from '../logic/knowledge'

function TextView(sources) {
  const dict = [
    {name: 'boat', knowledge: KNOWLEDGE.BAD},
    {name: 'car', knowledge: KNOWLEDGE.EXCELENT},
    {name: 'person', knowledge: KNOWLEDGE.GOOD},
  ]
  const text = 'My boat is something very ugly. That is the main' +
               ' reason why I am a sad person. I have a Car CAR and a BOAT'
  const textWords = text.split(' ')

  const wordComponents$ = textWords.map(word => {
    const props$ = {
      name: word,
      knowledge: getKnowledge(word, dict),
    }
    return WordView(sources, props$)
  })
  const wordComponentsDOM$ = R.map(c => c.DOM, wordComponents$)
  const wordComponentsSel$ = R.map(c => c.selected$, wordComponents$)
  // Gets the latest word clicked on the page
  const currSelection$ = Observable.from(wordComponentsSel$)
    .mergeAll()
    .startWith('')

  currSelection$.subscribe(s => console.log(s))

  const sinks = {
    DOM: Observable.of(
      div(R.intersperse(' ', wordComponentsDOM$))
    ),
  }

  return sinks
}

export default TextView
