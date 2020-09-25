acas.data.model.define('currencies', {
	load: function(target){
		var deferred = Q.defer()
		target.currencies = [{currencyCode: 'USD', defaultCurrency: true}, {currencyCode: 'EUR'}]
		deferred.resolve()
		return deferred.promise		
	}
})