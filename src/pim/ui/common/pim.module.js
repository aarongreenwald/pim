var pim = angular.module('pim', [
	'ui.router',
	'pim.diary'
])

pim.diary = angular.module('pim.diary', [])
	 
pim.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise("/")

  $stateProvider
    .state('home', {
      url: '/',
      templateUrl: 'home/home.html'
    })
    .state('financials', {
      url: '/financials',
      templateUrl: 'financials/financials.html'
    })
   .state('diary', {
      url: '/diary',
      templateUrl: 'diary/diary.html'
    })
  .state('diaryEntry', { //putting this route at the root level also to hide the list view when navigating to a diary entry
      url: '/diary/{diaryEntryId}',
      templateUrl: 'diary/entry.html'
    })
})

pim.filter('pmFormatDate', function(){
  return function(date){
    return moment(date).calendar()
  }
})