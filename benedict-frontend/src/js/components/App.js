import 'bootstrap/dist/css/bootstrap.min.css'
import {h, div} from 'react-hyperscript-helpers'
import Navbar from '../containers/Navbar'

export default ({children}) =>
  div([
    h(Navbar),
    div('.container-fluid', [
      // Main views are drawn here with respect to the path
      children,
    ]),
  ])

