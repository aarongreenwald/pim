var pim = angular.module('pim', [
	'ui.router'
	])
	 
pim.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/");

  $stateProvider
    .state('home', {
      url: "/",
      templateUrl: "home/home.html"
    })
})


