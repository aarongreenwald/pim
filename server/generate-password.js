const hash = require('pbkdf2-password')()

const createPassword = (password) =>
    hash({ password }, function (err, pass, salt, hash) {
        if (err) throw err;
        console.log(`SALT=${salt}`)
        console.log(`HASH=${hash}`)
    });

if (!process.env.PASSWORD) {
    throw 'Pass a PASSWORD environment variable to this script'
}

createPassword(process.env.PASSWORD)