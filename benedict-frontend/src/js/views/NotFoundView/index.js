import { Component } from 'react';
import { Link } from 'react-router';
import { div, h1, hr, h } from 'react-hyperscript-helpers'

export default class NotFoundView extends Component {
  render () {
    return (
      div({className: 'container text-center'}, [
        h1('This is a demo 404 page!'),
        hr(),
        h(Link, {to: '/'}, ['Back To Home View']),
      ])
    );
  }
}
