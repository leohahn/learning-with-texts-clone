export class LoginService {
  constructor ($log, $http) {
    'ngInject'
    this.$http = $http
    this.$log = $log
    this.apiHost = 'http://localhost:8081/api/login'

    this.userLink = null
    this.userToken = null
    this.errorMessage = null
  }

  isLoggedIn () {
    return Boolean(this.userToken)
  }

  destroyCredentials () {
    this.userLink = null
    this.userToken = null
    this.errorMessage = null
  }

  authenticate (credentials) {
    return this.$http.post(this.apiHost, credentials)
      .then((response) => {
        this.userLink = response.data
        this.userToken = response.headers()['cookie-auth']
        return {
          ok: true,
          message: 'Login succeeded!'
        }
      })
      .catch((error) => {
        this.userLink = null
        this.userToken = null
        return {
          ok: false,
          message: error.data
        }
      })
  }
}
