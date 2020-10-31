import {
    CarSummary,
    Category,
    CategoryId,
    Income,
    IncomeId,
    Payment,
    PaymentId,
    SpendingByCategory,
    vPayment
} from '@pim/common';
import {all, get, getDb, run} from './db.helpers';

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
        (paid_date, counterparty, amount, currency, category_id, note)
    values (?,?,?,?,?,?)
  `
    //TODO: validations - the fallback to null done here forces the db to reject
    //bad data but there should probably be a validation and sanitization step prior to getting here
    //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
    //unlikely. make sure to enable FK support in sqlite or categoryId won't be checked
    const params = [
        new Date(payment.paidDate), //TODO maybe save the data in yyyy-mm-dd so it's easier to use?
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
