
const data = require('./data')
const app = require('express')()

app.use((req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*")
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")

  console.log(new Date(), req.method, req.url)
  next()
})

app.route('/entries')
    .get((req, res) => {
        res.send(data.getEntries())
    })    
    .post((req, res) => {
        res.send({startTimestamp: new Date(), content: ''})
    })

app.route('/entries/:id')
    .get((req, res) => {
        res.send(data.getEntry(req.params.id))
    })
    .put((req, res) => {
        res.send(200)
    })

app.listen(3000, () => console.log('Example app listening on port 3000!'))
