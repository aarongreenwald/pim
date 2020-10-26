if (process.env.DOTENV) {
    require('dotenv').config({path: process.env.DOTENV})
}
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

app.route('/payments')
  .get((req, res) => {
        db.getAllPayments().then(data => res.send(JSON.stringify(data)))
    })
  .post(jsonParser, (req, res) => {
    db.insertPayment(req.body)
      .then(data => res.send(JSON.stringify(data)))
      .catch(ex => {
        console.error(ex)
        res.status(500).send(ex)
      })
  })
  .put(jsonParser, (req, res) => {
    db.updatePayment(req.body)
      .then(() => res.status(200).send())
      .catch(ex => {
        console.error(ex)
        res.status(500).send(ex)
      })
  })

app.get('/payments/:id', (req, res) => {
    db.getPayment((req.params as any).id).then(data => res.send(JSON.stringify(data)));
})

app.route('/categories')
  .get((req, res) => {
    db.getAllCategories().then(data => res.send(JSON.stringify(data)))
  })

app.get('/analysis/spending-by-category', (req, res) => {
    db.getSpendingByCategory((req.query as any).rootCategoryId).then(data => res.send(JSON.stringify(data)))
})

app.route('/car/summary')
    .get((req, res) =>
        db.getCarSummary().then(data => res.send(JSON.stringify(data)))
    )

app.route('/income')
    .get((req, res) =>
        db.getAllIncome().then(data => res.send(JSON.stringify(data)))
    )

express()
    .use('/api', app)
    .listen(PORT, () => console.log(`App listening on port: ${PORT}`));
