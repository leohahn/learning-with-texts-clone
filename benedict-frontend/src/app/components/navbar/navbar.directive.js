export function NavbarDirective () {
  'ngInject'

  let directive = {
    restrict: 'E',
    templateUrl: 'app/components/navbar/navbar.html',
    scope: {
      creationDate: '='
    },
    controller: NavbarController,
    controllerAs: 'vm',
    bindToController: true
  }

  return directive
}

class NavbarController {
  constructor ($state, moment, loginService) {
    'ngInject'

    // "this.creationDate" is available by directive option "bindToController: true"
    this.$state = $state
    this.relativeDate = moment(this.creationDate).fromNow()
    this.loginService = loginService
  }

  logout () {
    this.loginService.destroyCredentials()
    this.$state.go('login')
  }
}
