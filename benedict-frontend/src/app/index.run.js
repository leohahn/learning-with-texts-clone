export function runBlock ($state, $log, $location, $rootScope, loginService) {
  'ngInject'

  $rootScope.$on('$stateChangeStart', (event, toState, toParams, fromState, fromParams) => {
    const userAuthenticated = loginService.isLoggedIn()
    $log.log(`User Authenticated: ${userAuthenticated}`)

    if (!userAuthenticated && !toState.name !== 'login') {
      $rootScope.savedLocation = $location.url()
      $location.path('/login')
    }
  })
  $log.debug('runBlock end')
}
