import React, { Component } from 'react'

export class App extends Component {
  static propTypes = {
    children: React.PropTypes.any,
  };

  render() {
    return (
      <section>
        <h1>This is App</h1>
        <div>
          {this.props.children}
        </div>
      </section>
    )
  }
}
