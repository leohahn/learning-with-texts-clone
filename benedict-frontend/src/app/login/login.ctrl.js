export class LoginCtrl {
  constructor ($log, $state, loginService) {
    'ngInject'
    this.$log = $log
    this.$state = $state
    this.loginService = loginService
    this.credentials = {
      username: 'lhahn',
      password: 'abcdef'
    }
  }

  submitCredentials () {
    this.loginService.authenticate(this.credentials)
      .then((status) => {
        if (status.ok) {
          this.$log.log(status.message)
          this.redirectTo('home')
        } else {
          this.$log.log(status.message)
        }
      })
  }

  redirectTo (where) {
    this.$log.log('moving')
    this.$state.go(where)
  }
}
