export function routerConfig ($stateProvider, $urlRouterProvider) {
  'ngInject'
  $stateProvider
    .state('main', {
      url: '/',
      templateUrl: 'app/main/main.html',
      controller: 'MainCtrl',
      controllerAs: 'main'
    })
    .state('main.home', {
      url: '/',
      templateUrl: 'app/home/home.html',
      controller: 'HomeCtrl',
      controllerAs: 'home'
    })
    .state('main.profile', {
      url: '/profile',
      templateUrl: 'app/profile/profile.html',
      controller: 'ProfileCtrl',
      controllerAs: 'profile'
    })
    .state('login', {
      url: '/login',
      templateUrl: 'app/login/login.html',
      controller: 'LoginCtrl',
      controllerAs: 'login'
    })

  $urlRouterProvider.otherwise('/')
}
