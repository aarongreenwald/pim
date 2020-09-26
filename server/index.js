const db = require('./db');
const data = require('./data')
const app = require('express')()
const bodyParser = require('body-parser')
const PORT = process.env.PORT;
const jsonParser = bodyParser.json();

app.use((req, res, next) => {
  res.header("Access-Control-Allow-Origin", "*")
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  console.log(new Date(), req.method, req.url)
  next()
})

app.route('/spending')
    .get((req, res) => {
        db.getAllSpending().then(data => res.send(JSON.stringify(data)))
    })
  .post(jsonParser, (req, res) => {
    db.insertSpending(req.body)
      .then(data => res.send(JSON.stringify(data)))
      .catch(ex => {
        console.error(ex)
        res.status(500).send(ex)
      })
  })

app.route('/categories')
  .get((req, res) => {
    db.getAllCategories().then(data => res.send(JSON.stringify(data)))
  })

app.route('/entries')
    .get((req, res) => {
        res.send(data.getEntries())
    })    
    .post((req, res) => {
        const entry = data.newEntry()
        console.log(entry)
        res.send(entry)    
    })

app.route('/entries/:id')
    .get((req, res) => {
        res.send(data.getEntry(req.params.id))
    })
    .put((req, res) => {
        const {id} = req.params
        const {content} = req.body
        data.saveEntry({id, content})
        res.send(200)
    })

app.listen(PORT, () => console.log(`App listening on port: ${PORT}`));
