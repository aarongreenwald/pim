pim.financials.controller('FinancialsCtrl', ['$scope', '$http', '$state', 
	function($scope, $http, $state){		
		$scope.financials = new function(){
			var internal = {
				init: function(){
					$http({method: 'GET', url: 'api/financials/spending'})
						.success(function(data){
							api.spending = data
						})
				}
			}
				
			var api = {
				newSpending: function(){
					$state.go('financials.spendingItem', { spendingId: 0 })
				},
				openSpending: function(spendingId){
					$state.go('financials.spendingItem', { spendingId: spendingId})
				}				
			}
			
			internal.init()
			return api
		}
	}
])
