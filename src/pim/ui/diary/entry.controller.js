pim.diary.controller('DiaryEntryCtrl', ['$scope', '$http', '$stateParams', '$state',
	function($scope, $http, $stateParams, $state){
		$scope.diaryEntry = new function(){
			var internal = {
				init: function(){
					$http({method: 'GET', url: 'api/diary/entries/' + $stateParams.diaryEntryId})
						.success(function(data){
							api.entry = data
						})
				}
			}
			
			var api = {
				save: function(){
					$http({method: 'PUT', url: 'api/diary/entries/' + $stateParams.diaryEntryId,						
						data: api.entry											
					}).success(function(data){						
						api.entry = data											
					})																			
				},								
				delete: function(){
					$http({method: 'DELETE', url: 'api/diary/entries/' + $stateParams.diaryEntryId})				
						.success(function(data){
							$state.go('diary')
						})
				}

			}
			
			internal.init()
			return api
		}		
	}
])
