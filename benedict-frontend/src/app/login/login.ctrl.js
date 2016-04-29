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
    this.status = {
      ok: true,
      message: '',
      className: ''
    }
  }

  setErrorStatus (message) {
    this.status = {
      ok: false,
      message,
      className: 'alert alert-danger'
    }
  }

  submitCredentials () {
    this.loginService.authenticate(this.credentials)
      .then((status) => {
        if (status.ok) {
          this.$log.log(status.message)
          this.redirectTo('main.home')
        } else {
          this.$log.log(status.message)
          this.setErrorStatus(status.message)
        }
      })
  }

  redirectTo (where) {
    this.$log.log('moving')
    this.$state.go(where)
  }
}
