import {Category, Payment, PaymentId, vPayment} from '@pim/common';
import sqlite from 'sqlite3';

const DATABASE_PATH = process.env.DATABASE_PATH;
if (!DATABASE_PATH) {
  throw 'Environment variable DATABASE_PATH is not set!'
}


sqlite.verbose();

/**
 * Helper function to promisify Database.all()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<unknown[]>}
 */
function all<T>(db: sqlite.Database, sql: string, params = []): Promise<T[]> {
  return new Promise(((resolve, reject) =>
      db.all(sql, params, (err, rows) => {
        if (err) reject(err)
        else resolve(rows as unknown as T[])
      })
  ))
}

/**
 * Helper function to promisify Database.run()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<{lastId, changes}>}
 */
function run(db: sqlite.Database, sql: string, params: any[] = []): Promise<{lastId: any; changes: any}> {
  return new Promise((resolve, reject) => {
    db.run(sql, params, function (err) {
      if (err) reject(err)
      // @ts-ignore
      resolve({lastId: this.lastID, changes: this.changes})
    })
  })
}

/**
 * Helper function to promisify Database.get()
 * @param db
 * @param sql
 * @param params
 * @returns {Promise<unknown>}
 */
function get<T>(db: sqlite.Database, sql: string, params = [] as any): Promise<T> {
  return new Promise((resolve, reject) => {
    db.get(sql, params, function(err, data) {
      if (err) reject(err)
      resolve(data)
    })
  })
}

/**
 * Setups a db connection
 * @param readonly = true
 * @returns {Promise<sqlite.Database>}
 */
const getDb = async (readonly = true) => {
  const db = await new Promise<sqlite.Database>((resolve, reject) => {
    //sqlite3 creates a DB immediately but could throw in error if it fails
    //to find the file, or permission is denied, etc. So best to promisify
    //the whole thing
    const _db: sqlite.Database = new sqlite.Database(resolvePath(DATABASE_PATH),
        readonly ? sqlite.OPEN_READONLY : sqlite.OPEN_READWRITE,
        err => err ? reject(err): resolve(_db) //resolve the object already created 🤔
    );
  })
  if (!readonly) {
    await run(db, 'PRAGMA foreign_keys = ON;')
  }
  return db;
}


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



export const getAllIncome = async () => {
  const db = await getDb();
  return all(db, 'select * from income order by paid_date desc')
}

export const getCarSummary = async () => {
  const db = await getDb();
  return all(db, 'select * from v_car order by record_date desc')
}

export const getAllCategories = async () => {
  const db = await getDb();
  const sql = 'select category_id as id, name from v_category';
  return all<Category>(db, sql)
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
/*
  Simple utility to expand ~ in paths to the user's home dir. Only handles tilde as
  first character.
 */
const resolvePath = path => path.startsWith('~') ? path.replace('~', require('os').homedir()) : path;