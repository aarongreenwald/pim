var pim = angular.module('pim', [
	'ui.router'
	])
	 
pim.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/");

  $stateProvider
    .state('home', {
      url: '/',
      templateUrl: 'home/home.html'
    })
    .state('financials', {
      url: 'financials',
      templateUrl: 'financials/financials.html'
    })
   .state('diary', {
      url: 'diary',
      templateUrl: 'diary/diary.html'
    })
  .state('diary.entry', {
      url: '{id}',
      templateUrl: 'diary/entry.html'
    })
})


