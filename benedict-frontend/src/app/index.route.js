export function routerConfig ($stateProvider, $urlRouterProvider) {
  'ngInject'
  $stateProvider
    .state('home', {
      url: '/',
      templateUrl: 'app/main/main.html',
      controller: 'MainController',
      controllerAs: 'main'
    })
    .state('login', {
      url: '/login',
      templateUrl: 'app/login/login.html',
      controller: 'LoginCtrl',
      controllerAs: 'login',
      isLogin: true
    })

  $urlRouterProvider.otherwise('/')
}
