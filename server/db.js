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

module.exports = {
  getAllSpending
}