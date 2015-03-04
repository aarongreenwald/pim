'use strict'

Application.Controllers.controller('CashAccountsCtrl', ['$scope', '$http', 
	function($scope, $http){
	
	$http({method: 'GET', url: '/financial/cash-accounts'}).
	  success(function(data, status, headers, config) {
		$scope.data = data;
	  }).
	  error(function(data, status, headers, config) {
		// called asynchronously if an error occurs
		// or server returns response with an error status.
	  });
	
	$scope.pages = [
			{name: 'Cash Accounts',
				href: 'cash-accounts'},
			{name: 'Cash Accounts',
				href: 'cash-accounts'},				
			{name: 'Spending Items',
				href: 'spending-items'},				
			]
	
}]);
