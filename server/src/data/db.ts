import {
    CarSummary,
    CashAccount,
    CashAssetAllocation,
    CashAssetAllocationRecord,
    CashAssetRecord,
    Category,
    CategoryId,
    Income,
    IncomeId,
    Payment,
    PaymentId,
    SpendingByCategory,
    UnreportedSpending,
    vPayment
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
    from v_payment order by paid_date desc`)
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
               category_id categoryId, 
               note, 
               currency
            from payment where payment_id = ?`,
        id)
}

export const insertPayment = async (payment: Payment) => {
    const sql = `
    insert into payment
        (paid_date, incurred_begin_date, incurred_end_date, counterparty, amount, currency, category_id, note)
    values (?,?,?,?,?,?,?,?)
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
        payment.amount || null,
        payment.currency || null,
        payment.categoryId,
        payment.note || null
    ]
    const db = await getDb(false);

    const {lastId} = await run(db, sql, params)
    //if the get() rejects but the run() resolved, the server will return 500 which is not good
    //client will be tempted to retry, but the post was successful
    return get<Payment>(db, `select * from payment where payment_id = ?`, lastId)
}

export const updatePayment = async (payment: Payment) => {
    const sql = `
        update payment
        set paid_date = ?,
            incurred_begin_date = ?,
            incurred_end_date = ?,
            counterparty = ?,
            amount = ?,
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
        payment.amount || null,
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
    return all<UnreportedSpending>(db, 'select start_date startDate, end_date endDate, ils, usd from v_unreported_spending')
}


export const getAllCategories = async () => {
  const db = await getDb();
  const sql = 'select category_id as id, name from v_category';
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