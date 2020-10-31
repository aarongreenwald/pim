import * as db from '../data/db';
import bodyParser from 'body-parser';
import {Express} from 'express';
const jsonParser = bodyParser.json();

export const setupRoutes = (app: Express) => {

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

    app.get('/categories', (req, res) => {
            db.getAllCategories().then(data => res.send(JSON.stringify(data)))
        })

    app.get('/car/summary', (req, res) =>
            db.getAllCarSummaries().then(data => res.send(JSON.stringify(data)))
        )

    app.route('/income')
        .get((req, res) =>
            db.getAllIncome().then(data => res.send(JSON.stringify(data)))
        )
        .post(jsonParser, (req, res) => {
            db.insertIncome(req.body)
                .then(data => res.send(JSON.stringify(data)))
                .catch(ex => {
                    console.error(ex)
                    res.status(500).send(ex)
                })
        })
        .put(jsonParser, (req, res) => {
            db.updateIncome(req.body)
                .then(() => res.status(200).send())
                .catch(ex => {
                    console.error(ex)
                    res.status(500).send(ex)
                })
        })

    app.get('/income/:id', (req, res) =>
            db.getIncome((req.params as any).id).then(data => res.send(JSON.stringify(data)))
        )

    app.get('/analysis/spending-by-category', (req, res) => {
        db.getSpendingByCategory((req.query as any).rootCategoryId).then(data => res.send(JSON.stringify(data)))
    })

}