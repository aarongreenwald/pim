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

    app.get('/car/accounts', (req, res) =>
        db.getActiveCashAccounts().then(data => res.send(JSON.stringify(data)))
    )

    app.route('/car/records')
        .get((req, res) => {
            db.getCashAssetRecords((req.query as any).recordDate)
                .then(data => res.send(JSON.stringify(data)))
        })
        .put(jsonParser, (req, res) => {
            const {recordDate, accountBalances} = req.body;
            db.updateCashAssetRecords(recordDate, accountBalances)
                .then(data => res.send(JSON.stringify(data)))
        })

    app.get('/cash-allocations', (req, res) => {
        Promise.all([
            //TODO consider combining this into one view
            db.getUnallocatedCashSnapshot(),
            db.getCashAssetsAllocation()
        ]).then(([unallocatedCashSnapshot, cashAssetsAllocation]) => {
            res.send(JSON.stringify({
                unallocatedCashSnapshot,
                cashAssetsAllocation
            }))
        })
    })

    app.post('/cash-allocations', jsonParser, (req, res) => {
        db.insertCashAssetAllocationRecord(req.body)
            .then(() => res.status(200).send())
            .catch(ex => {
                console.error(ex)
                res.status(500).send(ex)
            })
    })

    app.get('/unreported-spending', (req, res) => {
        db.getUnreportedSpending()
            .then(data => res.send(JSON.stringify(data)))
    })

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

    app.route('/fuel-log')
        .get((req, res) => {
            Promise.all([db.getFuelLog(), db.getFuelLogSummary()])
                .then(([fuelLog, summary]) => res.send(JSON.stringify({fuelLog, summary})))
                .catch(ex => {
                    console.error(ex)
                    res.status(500).send(ex)
                })
        })
        .post(jsonParser, (req, res) => {
            db.insertFuelLog(req.body)
                .then(() => res.send(200))
                .catch(ex => {
                    console.error(ex)
                    res.status(500).send(ex)
                })
        })

}