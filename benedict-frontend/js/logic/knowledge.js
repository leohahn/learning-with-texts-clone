import R from 'ramda'

export const KNOWLEDGE = {
  BAD: 'BAD',
  POOR: 'POOR',
  AVERAGE: 'AVERAGE',
  GOOD: 'GOOD',
  EXCELENT: 'EXCELENT',
  NONE: 'NONE',
}

export const knowledgeToColor = {
  BAD: 'rgb(221, 112, 23)',
  POOR: 'rgb(182, 157, 69)',
  AVERAGE: 'rgba(142, 191, 37, 0.7)',
  GOOD: 'rgb(13, 173, 140)',
  EXCELENT: 'rgb(40, 186, 212)',
  NONE: 'rgba(255, 255, 255, 0)',
}

export function getKnowledge(word, dict) {
  const procWord = word.replace('.', '').toLowerCase()
  const entry =
    R.head(R.filter(e => e.name === procWord, dict))

  return entry !== undefined ? entry.knowledge : KNOWLEDGE.NONE
}
