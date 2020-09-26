const sqlite = require('sqlite3')

const DATABASE_PATH = process.env.PIM_DATABASE_PATH;
if (!DATABASE_PATH) {
  throw 'Environment variable PIM_DATABASE_PATH is not set!'
}

sqlite.verbose();

const getDb = () => new sqlite.Database(DATABASE_PATH, sqlite.OPEN_READWRITE)

const getAllSpending = () => new Promise((resolve, reject) =>
  getDb().all('select * from v_spending', (err, rows) => {
    if (err) reject(err)
    else resolve(rows)
  })
)

const getAllCategories = () => new Promise((resolve, reject) =>
  getDb().all('select * from category', (err, rows) => {
    if (err) reject(err)
    else resolve(rows)
  })
)

const insertSpending = (spending) => new Promise((resolve, reject) => {
  const sql = `
    insert into spending
        (paid_date, recipient, amount, category_id, note)
    values (?,?,?,?,?)
 `
  const params = [
    new Date(spending.paidDate),
    spending.counterParty || null, //empty string isn't valid, and '0' is highly unlikely - let the db reject it
    spending.amount || null, //0 isn't valid, instead put null and let the db reject it. really the db should have a constraint here
    spending.categoryId, //is the db enforcing FK constraints?
    spending.note || null //here maybe 0 should be allowed
  ]
  const db = getDb();

  db.run(sql, params, function (err) {
    if (err) reject(err)
    else {
      console.log(this.lastID)
      db.get(`select * from v_spending where spending_id = ?`, this.lastID, (err, data) => {
        if (err) reject(err) //this is problematic, server will return 500 and client will be tempted to retry, but the post was successful
        else {
          console.log(data)
          resolve(data)
        }

      })
    }
  })
})

module.exports = {
  getAllSpending,
  getAllCategories,
  insertSpending
}