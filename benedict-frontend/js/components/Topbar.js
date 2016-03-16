import {Observable} from 'rx'
import {a, div} from '@cycle/dom'

function intent(DOMSources) {
  const workspaceClicks$ = DOMSources.select('#workspace-button')
    .events('click')
    .map(() => 'workspace')
  const profileClicks$ = DOMSources.select('#profile-button')
    .events('click')
    .map(() => 'profile')
  return {
    workspaceClicks$,
    profileClicks$,
  }
}

function model(workspaceClicks$, profileClicks$) {
  const combinedClicks$ = Observable.merge(workspaceClicks$, profileClicks$)
  const sidebarClasses =
    'ui top demo horizontal inverted sidebar labeled icon menu visible'
  const workspaceClasses$ = combinedClicks$
    .map(str => str === 'workspace' ? 'item active' : 'item')
    .startWith('item active')
  const profileClasses$ = combinedClicks$
    .map(str => str === 'profile' ? 'item active' : 'item')
    .startWith('item')
  return {
    sidebarClasses,
    workspaceClasses$,
    profileClasses$,
  }
}

function view(classes, createHref) {
  const workspaceHref = createHref('/workspace')
  const profileHref = createHref('/profile')
  const vTree$ = Observable.combineLatest(
    classes.workspaceClasses$,
    classes.profileClasses$,
    (workspaceClasses, profileClasses) =>
      div({className: classes.sidebarClasses}, [
        a('#workspace-button',
          {className: workspaceClasses, href: workspaceHref}, [
            'Workspace',
        ]),
        a('#profile-button',
          {className: profileClasses, href: profileHref}, [
          'Profile',
        ]),
      ])
  )
  return vTree$
}

function Topbar(sources, path$) {
  const {router: {createHref}} = sources

  const {workspaceClicks$, profileClicks$} = intent(sources.DOM)
  const {
    workspaceClasses$,
    profileClasses$,
    sidebarClasses,
  } = model(workspaceClicks$, profileClicks$)
  const classes = {
    workspaceClasses$, profileClasses$, sidebarClasses,
  }
  const vTree$ = path$.map(() => view(classes, createHref))
  const sinks = {
    DOM: vTree$,
  }
  return sinks
}

export default Topbar
