const hash = require('pbkdf2-password')()

const createPassword = (password) =>
    hash({ password }, function (err, pass, salt, hash) {
        if (err) throw err;
        console.log('salt', salt)
        console.log('hash', hash)
    });

createPassword(process.env.PASSWORD)