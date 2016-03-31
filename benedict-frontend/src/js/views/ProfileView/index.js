import { div, h1, h } from 'react-hyperscript-helpers'
import NewText from '../../components/NewText'

const ProfileView = () =>
  div([
    div('.row', [
      div('.col-md-2', {style: {background: 'green'}}, [
        h1('HELLLLO'),
      ]),
      div('.col-md-10', [
        h(NewText, {onAddClick: () => alert('hello')}),
      ]),
    ]),
  ])

export default ProfileView
