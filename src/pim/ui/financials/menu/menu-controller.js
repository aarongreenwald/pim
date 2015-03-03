'use strict'

Application.Controllers.controller('menu', ['menu', '$scope', function(menu, $scope){
	
	$scope.items = menu.get();
	$scope.pages = [
			{name: 'Cash Accounts',
				href: 'cash-accounts'},
			{name: 'Cash Accounts',
				href: 'cash-accounts'},				
			{name: 'Spending Items',
				href: 'spending-items'},				
			]
				
	
}]);
