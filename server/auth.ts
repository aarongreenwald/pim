import helmet from "helmet";
import session from "express-session";
import pbkdf2_password from "pbkdf2-password";
import bodyParser from "body-parser";
const jsonParser = bodyParser.json();

const hash = pbkdf2_password()
export const setupAuth = (app) => {

    app.use(helmet());

    app.use(session({
        resave: false, // don't save session if unmodified
        saveUninitialized: false, // don't create session until something stored
        secret: 'secret', //TODO,
        cookie: {
            maxAge: 60000,
            //secure: false //TODO true after TLS
        }

    }));

    // // Session-persisted message middleware
    // app.use(function(req, res, next){
    //   var err = req.session.error;
    //   var msg = req.session.success;
    //   delete req.session.error;
    //   delete req.session.success;
    //   res.locals.message = '';
    //   if (err) res.locals.message = '<p class="msg error">' + err + '</p>';
    //   if (msg) res.locals.message = '<p class="msg success">' + msg + '</p>';
    //   next();
    // });

    app.post('/login', jsonParser, function(req, res){
        authenticate(req.body.password, function(err, result){
            console.log('auth', result)
            if (result) {
                // Regenerate session when signing in
                // to prevent fixation
                // req.session.regenerate(function(){
                    req.session.authenticated = true;
                    res.sendStatus('200');
                    console.log('authenticated', req.session)
                // });

            } else {
                req.session.error = 'Authentication failed, please check your password.'
                res.sendStatus('401');
            }
        });
    });

    app.get('/logout', function(req, res){
        // destroy the user's session to log them out
        // will be re-created next request
        req.session.destroy(function() {
            res.sendStatus(200)
        });
    });

    app.use(restrict);
}


const credentials = {
    salt: process.env.SALT,
    hash: process.env.HASH
}

function restrict(req, res, next) {
    if (req.method === 'OPTIONS') {
        next()
    } else if (req.session.authenticated) {
        next();
    } else {
        req.session.error = 'Access denied!';
        res.sendStatus('401');
    }
}

function authenticate(password: string, callback) {
    console.log('authenticating %s:%s', password);

    hash({ password: password, salt: credentials.salt }, function (err, pass, salt, hash) {
        if (err) return callback(err);
        if (hash === credentials.hash) return callback(null, true)
        callback(new Error('invalid password'), false);
    });
}