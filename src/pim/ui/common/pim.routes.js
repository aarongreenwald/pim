
pim.config(function($stateProvider, $urlRouterProvider) {
  $urlRouterProvider.otherwise('/')

  $stateProvider
    .state('home', {
      url: '/',
      templateUrl: 'home/home.html'
    })
    .state('financials', {
      url: '/financials',
      template: '<div ui-view></div>',
      abstract: true
    })
    .state('financials.home', {
      url: '',
      templateUrl: 'financials/financials.html',      
    })    
    .state('financials.spending', {
      url: '/spending',
      templateUrl: ''
    })
    .state('financials.spendingItem', {
      url: '/spending/{spendingId}',
      templateUrl: 'financials/spending_item.html'
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
