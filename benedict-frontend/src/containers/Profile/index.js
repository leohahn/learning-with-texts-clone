import React, { Component } from 'react'

import { Word } from 'components/Word'
import { Knowledge } from 'types'

class Profile extends Component {
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
      </div>
    )
  }
}

export default Profile
