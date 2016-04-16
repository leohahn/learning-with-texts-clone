import React, { Component } from 'react'
import { connect } from 'react-redux'

import { Saga } from 'actions/constants'
import { Word } from 'components/Word'
import { Knowledge } from 'types'

@connect() // Inject only dispatch
export class Profile extends Component {
  render () {
    return (
      <div>
        <h1>Profile :)</h1>
        <span>
          Fancy <Word onClick={(str) => console.log(str)}
                      knowledge={Knowledge.Excelent}
                      name='Word'
                />
        </span>
        <button onClick={() => this.props.dispatch({
          type: Saga.USER_LOGIN_REQUEST,
          body: { username: 'lhahn', password: 'abcdef' }})}
        >
          LOGIN!
        </button>
      </div>
    )
  }
}
