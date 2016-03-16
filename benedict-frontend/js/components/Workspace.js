import TextView from './TextView'

function Workspace(sources) {
  const textView = TextView(sources)

  return {
    DOM: textView.DOM,
  }
}

export default Workspace
