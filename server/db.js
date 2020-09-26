const sqlite = require('sqlite3')

const DATABASE_PATH = process.env.PIM_DATABASE_PATH;
if (!DATABASE_PATH) {
  throw 'Environment variable PIM_DATABASE_PATH is not set!'
}

sqlite.verbose();

const getDb = (readonly = true) =>
  new Promise((resolve, reject) => {
    const db = new sqlite.Database(DATABASE_PATH, readonly ? sqlite.OPEN_READONLY : sqlite.OPEN_READWRITE)
    if (!readonly) {
      db.run('PRAGMA foreign_keys = ON;', err => {
        if (err) reject(err)
        else resolve(db)
      })
    }
    resolve(db)
  })

const getAllSpending = () => new Promise(async (resolve, reject) =>
  (await getDb()).all('select * from v_spending', (err, rows) => {
    if (err) reject(err)
    else resolve(rows)
  })
)

const getAllCategories = () => new Promise(async (resolve, reject) => {
    (await getDb()).all('select * from category', (err, rows) => {
      if (err) reject(err)
      else resolve(rows)
    })
}

)

const insertSpending = (spending) => new Promise(async (resolve, reject) => {
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