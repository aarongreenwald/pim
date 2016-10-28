
const data = require('./data')
const app = require('express')()

app.use((req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*")
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  next()
})

app.get('/entries', (req, res) => {
  res.send(data.getEntries())
})


app.listen(3000, () => console.log('Example app listening on port 3000!'))
