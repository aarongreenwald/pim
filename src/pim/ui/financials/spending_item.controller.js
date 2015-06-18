pim.financials.controller('SpendingItemCtrl', ['$scope', '$http', '$state', '$stateParams',
	function ($scope, $http, $state, $stateParams) {
		_.extend(this, new function() {					
			var internal = {
				init: function () {
					if ($stateParams.spendingItemId) {
						$http({ method: 'GET', url: 'api/financials/spending/' + $stateParams.spendingItemId })
							.success(function (data) {
								api.data = data
						})
					}

				}
			}

			var api = {
				save: function () {
					if (api.incurredOnPaidDate) {
						api.data.incurredBeginDate = api.data.paidDate
						api.data.incurredEndDate = api.data.paidDate
					}
					var method = $stateParams.spendingItemId ? 'PUT' : 'POST'
					$http({ method: method, url: 'api/financials/spending', data: api.data })
						.success(function(){
							$state.go('financials.home')		
						})
				},				
				cancel: function(){
					$state.go('financials.home')
				},
				data: {
					paidDate: new Date(),
					categoryId: 1,
					incurredBeginDate: new Date(),
					incurredEndDate: new Date()
				},
				incurredOnPaidDate: true
			}

			internal.init()
			return api
		})
	}
])
