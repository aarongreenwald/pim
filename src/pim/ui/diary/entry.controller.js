pim.diary.controller('DiaryEntryCtrl', ['$scope', '$http', '$stateParams',
	function($scope, $http, $stateParams){
		$scope.diary.entry = new function(){
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
					$http({method: 'PUT', url: 'api/diary/entries/' + $stateParams.diaryEntryId
						, data: api.entry
					})
						.success(function(data){
							api.entry = data
						})
				}
			}
			
			internal.init()
			return api
		}		
	}
])
