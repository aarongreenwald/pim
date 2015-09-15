pim.financials.controller('SpendingItemCtrl', ['$scope', '$http', '$state', '$stateParams',
	function ($scope, $http, $state, $stateParams) {
		$scope.spendingItem = new function() {					
			var internal = {
				init: function () {
					acas.data.model.require('currencies', $scope.data).then(function(){
						$scope.$apply(function(){
							var defaultCurrency = _.find($scope.data.currencies, {defaultCurrency: true})
							api.data.defaultCurrencyCode = defaultCurrency.currencyCode
							api.data.currencyCode = defaultCurrency.currencyCode 
						})
					})
					if (parseInt($stateParams.spendingId)) {
						$http({ method: 'GET', url: 'api/financials/spending/' + $stateParams.spendingId })
							.success(function (data) {
								_.extend(api.data, data)							
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
					var method = (parseInt($stateParams.spendingId)) ? 'PUT' : 'POST'
					$http({ method: method, url: 'api/financials/spending', data: api.data })
						.success(function(){
							$state.go('financials.home')		
						})
				},				
				cancel: function(){
					$state.go('financials.home')
				},
				incurredOnPaidDate: true,
				data: {
					paidDate: new Date(),
					categoryId: 1,
					incurredBeginDate: new Date(),
					incurredEndDate: new Date(),															
				}
			}

			internal.init()
			return api
		}
	}
])
