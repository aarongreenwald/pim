import * as db from './db'
import bodyParser from 'body-parser';
import express from 'express';
import {setupAuth} from './auth';
const app = express()
const PORT = process.env.PORT;
const jsonParser = bodyParser.json();

app.use((req, res, next) => {
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  console.log(new Date(), req.method, req.url)
  next()
})

setupAuth(app);

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

express()
    .use('/api', app)
    .listen(PORT, () => console.log(`App listening on port: ${PORT}`));
