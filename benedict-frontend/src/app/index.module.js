/* global malarkey:false, moment:false */

import { config } from './index.config'
import { routerConfig } from './index.route'
import { runBlock } from './index.run'
import { MainCtrl } from './main/main.ctrl'
import { LoginCtrl } from './login/login.ctrl'
import { LoginService } from './login/login.service'
import { HomeCtrl } from './home/home.ctrl'
import { ProfileCtrl } from './profile/profile.ctrl'
import { GithubContributorService } from '../app/components/githubContributor/githubContributor.service'
import { WebDevTecService } from '../app/components/webDevTec/webDevTec.service'
import { NavbarDirective } from '../app/components/navbar/navbar.directive'
import { MalarkeyDirective } from '../app/components/malarkey/malarkey.directive'

angular.module('benedict', [
  'ngAnimate',
  'ngCookies',
  'ngTouch',
  'ngSanitize',
  'ngMessages',
  'ngAria',
  'ui.router',
  'toastr'
]).constant('malarkey', malarkey)
  .constant('moment', moment)
  .config(config)
  .config(routerConfig)
  .run(runBlock)
  .service('githubContributor', GithubContributorService)
  .service('loginService', LoginService)
  .service('webDevTec', WebDevTecService)
  .controller('MainCtrl', MainCtrl)
  .controller('HomeCtrl', HomeCtrl)
  .controller('ProfileCtrl', ProfileCtrl)
  .controller('LoginCtrl', LoginCtrl)
  .directive('acmeNavbar', NavbarDirective)
  .directive('acmeMalarkey', MalarkeyDirective)
