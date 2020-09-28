import {Category, Payment} from '@pim/common';
import sqlite from 'sqlite3';

const DATABASE_PATH = process.env.PIM_DATABASE_PATH;
if (!DATABASE_PATH) {
  throw 'Environment variable PIM_DATABASE_PATH is not set!'
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
function get<T>(db: sqlite.Database, sql: string, params = []): Promise<T> {
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
  const db = new sqlite.Database(DATABASE_PATH, readonly ? sqlite.OPEN_READONLY : sqlite.OPEN_READWRITE)
  if (!readonly) {
    await run(db, 'PRAGMA foreign_keys = ON;')
  }
  return db;
}


export const getAllSpending = async () => {
  const db = await getDb();
  return all(db, 'select * from v_spending')
}

export const getAllCategories = async () => {
  const db = await getDb();
  const sql = 'select category_id as id, name from category';
  return all<Category>(db, sql)
}

export const insertSpending = async (spending: Payment) => {
  const sql = `
    insert into spending
        (paid_date, recipient, amount, category_id, note)
    values (?,?,?,?,?)
  `
  //TODO: validations - the fallback to null done here forces the db to reject
  //bad data but there should probably be a validation and sanitization step prior to getting here
  //empty string isn't a valid counterparty, 0 isn't a valid amount. 0 could theoretically be a note but
  //unlikely. make sure to enable FK support in sqlite or categoryId won't be checked
  const params = [
    new Date(spending.paidDate),
    spending.counterParty || null,
    spending.amount || null,
    spending.categoryId,
    spending.note || null
  ]
  const db = await getDb(false);

  const {lastId} = await run(db, sql, params)
  //if the get() rejects but the run() resolved, the server will return 500 which is not good
  //client will be tempted to retry, but the post was successful
  return get<Payment>(db, `select * from spending where spending_id = ?`, lastId)
}
