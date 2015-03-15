pim.diary.controller('DiaryCtrl', ['$scope', '$http', '$state', 
	function($scope, $http, $state){
		$scope.diary = new function(){
			var internal = {
				
			}
				
			var api = {
				newDiaryEntry: function(){
					$http({method: 'POST', url: 'api/diary/entries'})
						.success(function(data){
							$state.go('diary.entry', {diaryEntryId: data})
						})
						
				}
			}
			
			return api
		}
	}
])
