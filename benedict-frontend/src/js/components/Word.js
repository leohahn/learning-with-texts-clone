import {PropTypes} from 'react'
import {span} from 'react-hyperscript-helpers'

const Word = ({name, color, onClick}) => {
  const wordProps = {
    style: `background: ${color}`,
  }
  return span(wordProps, [name])
}

Word.propTypes = {
  name: PropTypes.string.isRequired,
  color: PropTypes.string.isRequired,
}

export default Word
