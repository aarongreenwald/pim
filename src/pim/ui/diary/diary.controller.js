pim.diary.controller('DiaryCtrl', ['$scope', '$http', '$state', 
	function($scope, $http, $state){
		$scope.diary = new function(){
			var internal = {
				init: function(){
					$http({method: 'GET', url: 'api/diary/entries'})
						.success(function(data){
							api.entries = data
						})
				}
			}
				
			var api = {
				newDiaryEntry: function(){
					$http({method: 'POST', url: 'api/diary/entries'})
						.success(function(data){
							$state.go('diaryEntry', {diaryEntryId: data})
						})
						
				},
				openEntry: function(diaryEntryId){
					$state.go('diaryEntry', {diaryEntryId: diaryEntryId})
				},
				search: function(){
					$http({method: 'GET', url: 'api/diary/entries', params : {search: api.searchText}})
						.success(function(data){
							api.entries = data
						})
				}
			}
			
			internal.init()
			return api
		}
	}
])
