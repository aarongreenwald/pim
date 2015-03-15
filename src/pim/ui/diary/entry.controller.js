pim.diary.controller('DiaryEntryCtrl', ['$scope', '$http', '$stateParams',
	function($scope, $http, $stateParams){
		var internal = {
			init: function(){
				$http({method: 'GET', url: 'api/diary/entries/' + $stateParams.diaryEntryId})
					.success(function(data){
						console.log(data)
					})
			}
		}
		
		var api = {}
		
		internal.init()
		return api
	}
])
