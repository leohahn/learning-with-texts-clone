import React, { Component, PropTypes } from 'react'

import { Knowledge } from 'types'
import styles from './styles.css'

const colors = {
  None: styles.none,
  Poor: styles.poor,
  Medium: styles.medium,
  Good: styles.good,
  Excelent: styles.excelent
}

export const Word = ({ children, knowledge }) =>
  <span className={ colors[knowledge] }>
    { children }
  </span>

Word.propTypes = {
  knowledge: PropTypes.string.isRequired
}
