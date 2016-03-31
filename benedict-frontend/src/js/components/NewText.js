import '../../styles/style.css'
import React, {PropTypes} from 'react'
import {div, button, textarea} from 'react-hyperscript-helpers'

const NewText = ({onAddClick}) => {
  return div([
    textarea({rows: 20}),
    button({className: 'btn btn-primary'},
      'Add Text',
    ),
  ])
}

NewText.propTypes = {
  onAddClick: PropTypes.func.isRequired,
}

export default NewText
