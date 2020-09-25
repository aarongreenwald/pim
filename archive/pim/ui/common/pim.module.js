var pim = angular.module('pim', [
	'ui.router',
	'pim.diary',
  'pim.financials'
])

pim.diary = angular.module('pim.diary', [])
pim.financials = angular.module('pim.financials', [])

pim.run(['$rootScope', function($rootScope){
  $rootScope.data = {}
}])
	 
pim.filter('pmFormatDate', function(){
  return function(date){
    return moment(date).calendar()
  }
})