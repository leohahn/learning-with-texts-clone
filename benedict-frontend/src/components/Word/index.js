import React, { PropTypes } from 'react'

import { Knowledge } from 'types'
import styles from './styles.css'

const colors = {
  [Knowledge.Unknown]: styles.unknown,
  [Knowledge.None]: styles.none,
  [Knowledge.Poor]: styles.poor,
  [Knowledge.Medium]: styles.medium,
  [Knowledge.Good]: styles.good,
  [Knowledge.Excelent]: styles.excelent
}

// ----------------------------------------------
// View
// ----------------------------------------------
export const Word = ({ knowledge, onClick, name }) =>
  <span className={ colors[knowledge] } onClick={ () => onClick(name) }>
    { name }
  </span>

// ----------------------------------------------
// Public Interface
// ----------------------------------------------
Word.propTypes = {
  name: PropTypes.string.isRequired,
  knowledge: PropTypes.string.isRequired,
  onClick: PropTypes.func.isRequired
}
