import {
    CarSummary,
    CashAccount,
    CashAssetAllocation,
    CashAssetAllocationRecord,
    CashAssetRecord,
    Category,
    CategoryId,
    FuelLog,
    FuelLogSummary,
    NewFuelLogDto,
    Income,
    IncomeId,
    Payment,
    PaymentId,
    SpendingByCategory,
    UnreportedSpending,
    vPayment,
    StockTransactionDto,
    StockHoldingSummaryDto,
    StockTransactionId,
    StockAccountDto
} from '@pim/common';
import {all, beginTransaction, commitTransaction, get, getDb, rollbackTransaction, run} from './db.helpers';

export const getAllPayments: () => Promise<vPayment[]> = async () => {
  const db = await getDb();
  return all(db, `
    select
           payment_id id,
           paid_date paidDate,
           counterparty, 
           incurred_begin_date incurredBeginDate, 
           incurred_end_date incurredEndDate, 
           ils, 
           usd, 
           category_name categoryName, 
           note, 
           category_id categoryId
    from v_payment order by paid_date desc, payment_id desc`)
}

export const getPayment: (id: PaymentId) => Promise<Payment> = async id => {
    const db = await getDb();
    return get<Payment>(db,
        `
            select
               payment_id id, 
               paid_date paidDate, 
               incurred_begin_date incurredBeginDate, 
               incurred_end_date incurredEndDate, 
               counterparty, 
               amount, 
               incurred_amount incurredAmount,
               category_id categoryId, 
               note, 
               currency
            from payment where payment_id = ?`,
        id)
}

export const insertPayment = async (payment: Payment) => {
    const sql = `
    insert into payment
        (paid_date, incurred_begin_date, incurred_end_date, counterparty, amount, incurred_amount, currency, category_id, note)
    values (?,?,?,?,?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
    //unlikely. make sure to enable FK support in sqlite or categoryId won't be checked
    const params = [
        new Date(payment.paidDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
        payment.incurredBeginDate ? new Date(payment.incurredBeginDate) : null,
        payment.incurredEndDate ? new Date(payment.incurredEndDate) : null,
        payment.counterparty || null,
        payment.amount || null, // 0 and null are the functionally same thing here
        payment.incurredAmount, // 0 is a valid amount and not the same as null, which is coalesced to the paid amount
        payment.currency || null,
        payment.categoryId,
        payment.note || null
    ]
    const db = await getDb(false);

    const {lastId} = await run(db, sql, params)
    return lastId;
}

export const updatePayment = async (payment: Payment) => {
    const sql = `
        update payment
        set paid_date = ?,
            incurred_begin_date = ?,
            incurred_end_date = ?,
            counterparty = ?,
            amount = ?,
            incurred_amount = ?,
            currency = ?,
            category_id = ?,
            note = ?
        where payment_id = ?        
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
    //unlikely. make sure to enable FK support in sqlite or categoryId won't be checked
    const params = [
        new Date(payment.paidDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
        payment.incurredBeginDate ? new Date(payment.incurredBeginDate) : null,
        payment.incurredEndDate ? new Date(payment.incurredEndDate) : null,
        payment.counterparty || null,
        payment.amount || null, // 0 and null are the functionally same thing here
        payment.incurredAmount, // 0 is a valid amount and not the same as null, which is coalesced to the paid amount
        payment.currency || null,
        payment.categoryId,
        payment.note || null,
        payment.id
    ]
    const db = await getDb(false);

    await run(db, sql, params)
}

export const getAllIncome = async () => {
  const db = await getDb();
    return all<Income>(db,
        `
        select income_id id, 
               source, 
               paid_date paidDate, 
               amount, 
               currency, 
               note
        from income
        order by paid_date desc
        `
    )
}

export const getIncome = async (id: IncomeId) => {
    const db = await getDb();
    return get<Income>(db,
        `
        select income_id id, 
               source, 
               paid_date paidDate, 
               amount, 
               currency, 
               note
        from income
        where income_id = ?
        `,
        [id]
    )
}


export const insertIncome = async (income: Income) => {
    const sql = `
    insert into main.income
        (paid_date, source, amount, currency, note)
    values (?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
    //unlikely.
    const params = [
        new Date(income.paidDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
        income.source || null,
        income.amount || null,
        income.currency || null,
        income.note || null
    ]
    const db = await getDb(false);

    const {lastId} = await run(db, sql, params)
    //if the get() rejects but the run() resolved, the server will return 500 which is not good
    //client will be tempted to retry, but the post was successful
    return get<Income>(db, `select * from main.income where income_id = ?`, lastId)
}

export const updateIncome = async (income: Income) => {
    const sql = `
        update income
        set paid_date = ?,
            source = ?,
            amount = ?,
            currency = ?,        
            note = ?
        where income_id = ?        
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
    //unlikely.
    const params = [
        new Date(income.paidDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
        income.source || null,
        income.amount || null,
        income.currency || null,
        income.note || null,
        income.id
    ]
    const db = await getDb(false);

    await run(db, sql, params)
}


export const getAllCarSummaries = async () => {
  const db = await getDb();
    return all<CarSummary>(db,
        `
        select record_date recordDate, ils, usd 
        from v_car_summary
        order by record_date desc
        `
    )
}

export const getActiveCashAccounts = async () => {
    const db = await getDb();
    //todo return inactive to client as well, and hide/show them depending on whether
    //values exist
    return all<CashAccount>(db,
        `
            select cash_account_id id,
                   name,
                   currency,
                   active
            from cash_account             
        `
    )
}

export const getCashAssetRecords = async (recordDate: string): Promise<CashAssetRecord[]> => {
    const db = await getDb();
    return all<CashAssetRecord>(db,
        `
            select cash_account_id accountId, 
                   amount 
            from cash_assets_record where record_date = ?`,
        [recordDate]
    )
}

export const updateCashAssetRecords = async (recordDate: string, accountBalances: CashAssetRecord[]) => {
    const db = await getDb(false);
    await beginTransaction(db);
    try {
        await run(db, 'delete from cash_assets_record where record_date = ?', [new Date(recordDate)])
        await Promise.all(
            accountBalances.map(ab =>
                run(db,
                    `                
                insert into cash_assets_record(record_date, cash_account_id, amount)
                values (?, ?, ?)
            `, [
                        new Date(recordDate),
                        ab.accountId,
                        ab.amount
                    ]
                )
            )
        )
        await commitTransaction(db);
    } catch (ex) {
        await rollbackTransaction(db);
        throw ex
    }


}

export const getCashAssetsAllocation = async () => {
    const db = await getDb();
    return all<CashAssetAllocation>(db, 'select allocation_code allocationCode, ils, usd from v_cash_assets_allocation')
}

export const getUnallocatedCashSnapshot = async () => {
    const db = await getDb();
    return get<CashAssetAllocation>(db, 'select ils, usd from v_unallocated_cash_snapshot')
}

export const getUnreportedSpending = async () => {
    const db = await getDb();
    return all<UnreportedSpending>(db, 
        `
            select start_date startDate, end_date endDate, ils, usd 
            from v_unreported_spending
            order by end_date desc
        `
    )
}


export const getAllCategories = async () => {
  const db = await getDb();
  const sql = 'select category_id as id, name, hierarchical_name hierarchicalName, level from v_category';
  return all<Category>(db, sql)
}

export const getSpendingByCategory = async (rootCategoryId: CategoryId) => {
    const db = await getDb();
    return all<SpendingByCategory>(db,
        `
            select rc.group_category_id id, 
                   rc.group_category_name categoryName, 
                   sum(ils) ils, 
                   sum(usd) usd 
            from v_payment p inner join v_rollup_categories rc
                on p.category_id = rc.category_id and rc.root_category_id = ?
            group by rc.group_category_id, rc.group_category_name
        `, [rootCategoryId])
}

export const insertCashAssetAllocationRecord = async (allocationRecord: CashAssetAllocationRecord) => {
    const sql = `
    insert into cash_assets_allocation
        (record_date, allocation_code, amount, currency, note)
    values (?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid allocation, 0 isn't a valid amount.
    const params = [
        new Date(allocationRecord.recordDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
        allocationRecord.allocationCode || null,
        allocationRecord.amount || null,
        allocationRecord.currency || null,
        allocationRecord.note || null
    ]
    const db = await getDb(false);

    const {lastId} = await run(db, sql, params)
    return lastId
}

export const getFuelLog: (pageSize?: number) => Promise<FuelLog[]> = async (pageSize) => {
    const db = await getDb();
    const sql = `
        select fuel_log_id id, timestamp, km_per_liter kilometersPerLiter, odometer, liters, kilometers, note, is_full isFull, payment_id paymentId
            , amount totalCost
            , currency
        from v_fuel_log
        ${pageSize ? `limit ${pageSize}` : ''}
    `;
    return all<FuelLog>(db, sql)
}

export const getFuelLogSummary: () => Promise<FuelLogSummary> = async () => {
    const db = await getDb();
    const sql = `
        select liters, kilometers, kilometers_per_liter kilometersPerLiter, ils
        from v_fuel_log_summary
    `;
    return get<FuelLogSummary>(db, sql)
}

export const insertFuelLog = async (fuelLogDto: NewFuelLogDto) => {
    //TODO do all this in a single transaction

    const paymentId = await insertPayment({
        id: -1, //For typescript, this is ignored
        paidDate: fuelLogDto.timestamp.toString(),
        //TODO counterparty should be settable, the categoryId should be based on the constants table, and
        //the currency shouldn't be hardcoded (in general the fuel log feature is "single currency", fix this.
        counterparty: 'Gas Station',
        categoryId: 14,
        currency: 'ILS',
        amount: Math.round(fuelLogDto.price * fuelLogDto.liters * 100) / 100,
	incurredAmount: null, //TODO this is a temporary condition
        note: fuelLogDto.note
    })

    const sql = `
    insert into fuel_log 
        (timestamp, odometer, liters, is_full, note, payment_id)
    values (?,?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid allocation, 0 isn't a valid amount.
    const params = [
        fuelLogDto.timestamp,
        fuelLogDto.odometer,
        fuelLogDto.liters,
        fuelLogDto.isFull,
        fuelLogDto.note,
        paymentId
    ];
    const db = await getDb(false);
    const {lastId} = await run(db, sql, params)
    return {paymentId, fuelLogId: lastId}
}


export const getStockHoldingsSummary = async () => {
    const db = await getDb();
    const sql = `
        select name accountName, tax_category taxCategory, ticker_symbol tickerSymbol, quantity, cost_basis costBasis
        from v_stock_holdings
    `;
    return all<StockHoldingSummaryDto>(db, sql)
}

export const getStockTransactions = async () => {
    const db = await getDb();
    const sql = `
        select stock_transaction_id id, 
               sa.stock_account_id accountId,
               sa.name accountName,
               ticker_symbol tickerSymbol,
               datetime(transaction_date / 1000, 'unixepoch') transactionDate, 
               quantity, 
               unit_price costBasis
        from v_stock_transactions st 
            inner join stock_account sa on st.account_id = sa.stock_account_id
        order by transaction_date desc
    `;
    return all<StockTransactionDto>(db, sql)
}

export const getStockTransaction = async (transactionId: StockTransactionId) => {
    const db = await getDb();
    const sql = `
        select stock_transaction_id id,
               account_id accountId,
               ticker_symbol tickerSymbol,
               datetime(transaction_date / 1000, 'unixepoch') transactionDate,
               quantity, 
               unit_price costBasis
        from v_stock_transactions
        where stock_transaction_id = ?
    `;
    return get<StockTransactionDto>(db, sql, transactionId)
}

export const getStockAccounts = async () => {
    const db = await getDb();
    const sql = `
        select stock_account_id id, name, tax_category taxCategory 
        from stock_account
    `;
    return all<StockAccountDto>(db, sql)
}

export const insertStockTransaction = async (transaction: StockTransactionDto) => {
    const sql = `
    insert into main.stock_transaction
        (transaction_date, account_id, ticker_symbol, unit_price, quantity)
    values (?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    const params = [
        new Date(transaction.transactionDate).getTime(),
        transaction.accountId || null,
        transaction.tickerSymbol || null,
        transaction.costBasis || null,
        transaction.quantity || null,
    ]
    const db = await getDb(false);

    const {lastId} = await run(db, sql, params)
    //if the get() rejects but the run() resolved, the server will return 500 which is not good
    //client will be tempted to retry, but the post was successful
    return get<StockTransactionDto>(db, `select *from main.stock_transaction where stock_transaction_id = ?`, lastId)
}

export const updateStockTransaction = async (transaction: StockTransactionDto) => {
    const sql = `
        update main.stock_transaction
        set transaction_date = ?,
            account_id = ?,
            ticker_symbol = ?,
            unit_price = ?,
            quantity = ?                    
        where main.stock_transaction.stock_transaction_id = ?        
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    const params = [
        new Date(transaction.transactionDate).getTime(),
        transaction.accountId || null,
        transaction.tickerSymbol || null,
        transaction.costBasis || null,
        transaction.quantity || null,
        transaction.id
    ]
    const db = await getDb(false);

    await run(db, sql, params)
}

export const execReadonlyQuery = async sql => {
  const db = await getDb(true);
  return await all<any>(db, sql);
}
