import React, { Component } from 'react'
import { bindActionCreators } from 'redux'
import { connect } from 'react-redux'

import { Word } from 'components/Word'

export class Profile extends Component {
  constructor(props) {
    super(props)
  }

  render() {
    return (
      <div>
        <h1>Profile :)</h1>
        <span>
          Fancy <Word knowledge="Good">Word</Word>
        </span>
      </div>
    )
  }
}
