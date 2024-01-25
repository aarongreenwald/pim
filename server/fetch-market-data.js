const fetch = require('node-fetch')

const apiKey = 'polygonapikey'
const symbols = ['AAPL', 'MSFT', 'SPY', 'C:USDILS']
const DAYS = 30


// Issue with this approach: it requires iterating over days and knowing if they're market days, or eating errors
// Also, loading a gazillion symbols and discarding them all feels silly
// const date = new Date('2024-01-08').toISOString().substring(0, 10)
// console.log({date})
// fetch(`https://api.polygon.io/v2/aggs/grouped/locale/us/market/stocks/${date}?adjusted=false&apiKey=${apiKey}`)
//     .then(res => res.json())
//     .then(data => data.results.filter(d => symbols.includes(d.T)))
//     .then(data => data.map(d => ({ticker: d.T, close: d.c, date: new Date (d.t)})))
//     .then(console.log)


// I can send five symbols at once, then I need to rate limit on the free version of the API,
// but each symbol gets a range days which is nice. 
// The timestamps are messy, though. They are UTC timestamps of the beginning of the grouping period
// for the data - here the grouping is 1 day, and the period evidently starts at midnight in NY so I get
// 5am in London. Functionally I can take a substring and the close date is correct, the timestamp should
// just hardcode to 16:00:00.000 for equities. For FX, it's probably the last price available in the UTC date,
// which means the db has mixed units...leave it empty in that case. (Data already in the db is from Google,
// with questionable meaning), anyway FX data isn't standardized like equities close prices
// It APPEARS that when changing the agg to 'week', the API leaks intraday data in the 'close' value
// when market is open and the week isn't over? Doesn't seem right. 

function getData () {
    const today = new Date().toISOString().substring(0, 10)
    const startDate = new Date(new Date() - 1000 * 60 * 60 * 24 * DAYS).toISOString().substring(0, 10)
    
    const processData = data => {
	const isFx = data.ticker.substr(0,2) == "C:"	
	return data.results.map(d => ({
	    tickerSymbol: isFx ? data.ticker.substr(2) : data.ticker,
	    price: d.c, //close
	    date: new Date(d.t).toISOString().substring(0, 10).split('-').join(''),
	    time: isFx ? null : '16:00:00.000',
//	    originalTime: new Date(d.t)
	}))
}

    return symbols.map(symbol => {
	console.log(`Fetching data for symbol: ${symbol}`)
	return fetch(`https://api.polygon.io/v2/aggs/ticker/${symbol}/range/1/day/${startDate}/${today}?apiKey=${apiKey}`)
	    .then(res => res.json())
            .then(res => {
		if (res.status === "OK") {
		    throw res
		}
		return res
	    })
	    .then(processData)
	    .catch(ex => {
		console.error(`Failed to get data for symbol: ${symbol}`, ex)
		return []
	    })
    })
}

// TODO auth
function loadToDb(data) {
    // console.log('sending data to db: ', data)
    const initializer = symbols.reduce((acc, item) => { acc[item] = 0; return acc}, {}) //so that there will be '0' values for failures
    const summary = data.reduce((acc, item) => {
	const symbol = item.tickerSymbol
	acc[symbol] = acc[symbol] || 0 //just in case there's data that isn't in symbols
	acc[symbol]++
	return acc
    }, initializer)
    console.log('Sending data to db, row count: ', summary)
    return fetch(`http://localhost:4321/api/market-data`, {method: 'PUT', headers: {'Content-Type': 'application/json'}, body: JSON.stringify(data)})
	.then(res => { console.log('PIM response: ', res.status); return res.text() })
	.then(console.log)
}

Promise.all(getData())
    .then(results => results.flat())
    .then(loadToDb)
