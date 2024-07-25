import * as db from '../data/financials.db';
import bodyParser from 'body-parser';
import {Express} from 'express';
import {errorHandler, serialize} from '../utils/utils';
const jsonParser = bodyParser.json();

export const setupFinancialsRoutes = (app: Express) => {

    app.route('/payments')
        .get((req, res) => {
            db.getAllPayments().then(data => res.send(serialize(data, req)))
        })
        .post(jsonParser, (req, res) => {
            db.insertPayment(req.body)
                .then(data => res.send(serialize(data, req)))
                .catch(errorHandler(res))
        })
        .put(jsonParser, (req, res) => {
            db.updatePayment(req.body)
                .then(() => res.status(200).send())
                .catch(errorHandler(res))
        })

    app.get('/payments/:id', (req, res) => {
        db.getPayment((req.params as any).id).then(data => res.send(serialize(data, req)));
    })

    app.get('/categories', (req, res) => {
            db.getAllCategories().then(data => res.send(serialize(data, req)))
        })

    app.get('/car/summary', (req, res) =>
            db.getAllCarSummaries().then(data => res.send(serialize(data, req)))
        )

    app.get('/car/accounts', (req, res) =>
        db.getActiveCashAccounts().then(data => res.send(serialize(data, req)))
    )

    app.route('/car/records')
        .get((req, res) => {
            db.getCashAssetRecords((req.query as any).recordDate)
                .then(data => res.send(serialize(data, req)))
        })
        .put(jsonParser, (req, res) => {
            const {recordDate, accountBalances} = req.body;
            db.updateCashAssetRecords(recordDate, accountBalances)
                .then(data => res.send(serialize(data, req)))
        })

    app.get('/cash-allocations', (req, res) => {
        Promise.all([
            //TODO consider combining this into one view
            db.getUnallocatedCashSnapshot(),
            db.getCashAssetsAllocation()
        ]).then(([unallocatedCashSnapshot, cashAssetsAllocation]) => {
          res.send(serialize([
	    ...cashAssetsAllocation,
	    {...unallocatedCashSnapshot, allocationCode: 'Unallocated'}
	  ], req))
        })
    })

    app.get('/cash-allocations/history/:allocationCode', (req, res) => {
	db.getCashAssetsAllocationHistory((req.params as any).allocationCode).then(data => res.send(serialize(data, req)))
    })

    app.post('/cash-allocations', jsonParser, (req, res) => {
        db.insertCashAssetAllocationRecord(req.body)
            .then(() => res.status(200).send())
            .catch(errorHandler(res))
    })

    app.get('/unreported-spending', (req, res) => {
        db.getUnreportedSpending()
            .then(data => res.send(serialize(data, req)))
    })

    app.route('/income')
        .get((req, res) =>
            db.getAllIncome().then(data => res.send(serialize(data, req)))
        )
        .post(jsonParser, (req, res) => {
            db.insertIncome(req.body)
                .then(data => res.send(serialize(data, req)))
                .catch(errorHandler(res))
        })
        .put(jsonParser, (req, res) => {
            db.updateIncome(req.body)
                .then(() => res.status(200).send())
                .catch(errorHandler(res))
        })

    app.get('/income/:id', (req, res) =>
            db.getIncome((req.params as any).id).then(data => res.send(serialize(data, req)))
        )

    app.get('/analysis/spending-by-category', (req, res) => {
        db.getSpendingByCategory((req.query as any).rootCategoryId).then(data => res.send(serialize(data, req)))
    })

    app.route('/fuel-log')
        .get((req, res) => {
            const pageSize = parseInt(req.query.pageSize as string);
            Promise.all([db.getFuelLog(pageSize), db.getFuelLogSummary()])
              .then(([fuelLog, summary]) => res.send(JSON.stringify({fuelLog, summary})))
              .catch(errorHandler(res))
        })
        .post(jsonParser, (req, res) => {
            db.insertFuelLog(req.body)
              .then(() => res.send(200))
              .catch(errorHandler(res))
        })

    app.get('/stock-holdings-summary', (req, res) => {
        db.getStockHoldingsSummary()
          .then(data => res.send(serialize(data, req)))
          .catch(errorHandler(res))
    })

    app.route('/stock-transactions')
        .get((req, res) =>
            db.getStockTransactions().then(data => res.send(serialize(data, req)))
        )
        .post(jsonParser, (req, res) => {
            db.insertStockTransaction(req.body)
              .then(data => res.send(serialize(data, req)))
              .catch(errorHandler(res))
        })
        .put(jsonParser, (req, res) => {
            db.updateStockTransaction(req.body)
              .then(() => res.status(200).send())
              .catch(errorHandler(res))
        })

    app.get('/stock-transactions/:id', (req, res) =>
        db.getStockTransaction((req.params as any).id).then(data => res.send(serialize(data, req)))
    )

    app.get('/stock-accounts', (req, res) =>
        db.getStockAccounts().then(data => res.send(serialize(data, req)))
    )

  app.get('/stock-accounts/cash-balances', (req, res) =>
    db.getStockAccountCashBalances().then(data => res.send(serialize(data, req))))

  app.get('/stock-accounts/:id/cash-flow', (req, res) =>
      db.getStockAccountCashFlow((req.params as any).id).then(data => res.send(serialize(data, req))))
    
  app.route('/stock-accounts/cash-transactions/:id')
    .get((req, res) =>
      db.getStockAccountCashTransaction((req.params as any).id).then(data => res.send(serialize(data, req))))
    .put(jsonParser, (req, res) =>
      db.updateStockAccountCashTransaction(req.body) // use the id from inside the body, instead of the url, they should be the same anyway. TODO assert this. 
	.then(() => res.status(200).send())
	.catch(errorHandler(res)))

      
  app.get('/fx-history', (req, res) =>
    db.getFxHistory().then(data => res.send(serialize(data, req)))
  )

  app.get('/fx-transactions/:id', (req, res) =>
    db.getFxTransaction((req.params as any).id).then(data => res.send(serialize(data, req)))
  )
  
  app.route('/fx-transactions')    
        .post(jsonParser, (req, res) => {
            db.insertFxTransaction(req.body)
              .then(data => res.send(serialize(data, req)))
              .catch(errorHandler(res))
        })
        .put(jsonParser, (req, res) => {
            db.updateFxTransaction(req.body)
              .then(() => res.status(200).send())
              .catch(errorHandler(res))
        })

  app.put('/market-data', jsonParser, (req, res) =>
    db.updateMarketData(req.body)
      .then(() => res.status(200).send())
      .catch(errorHandler(res))
  )

  app.post('/compound-transactions/cash-stock-accounts-fund-transfer', jsonParser, (req, res) => {
    db.transferCashToStockAccount(req.body)
      .then(() => res.status(200).send())
      .catch(errorHandler(res))
  })
}
