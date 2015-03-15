pim.diary.controller('DiaryCtrl', ['$scope', '$http', '$state', 
	function($scope, $http, $state){
		$scope.diary = new function(){
			var internal = {
				init: function(){
					$http({method: 'GET', url: 'api/diary/entries'})
						.success(function(data){
							console.log(data)
						})
				}
			}
				
			var api = {
				newDiaryEntry: function(){
					$http({method: 'POST', url: 'api/diary/entries'})
						.success(function(data){
							$state.go('diary.entry', {diaryEntryId: data})
						})
						
				}
			}
			
			internal.init()
			return api
		}
	}
])
