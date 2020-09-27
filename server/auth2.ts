import helmet from "helmet";
import express from 'express';
import session from "express-session";
import pbkdf2_password from "pbkdf2-password";
import bodyParser from "body-parser";

const urlencoded = bodyParser.urlencoded({extended: true})
const hash = pbkdf2_password()
export const setupAuth = (app) => {
    app.set('view engine', 'ejs');

    app.use(helmet());

    app.use(session({
        resave: false, // don't save session if unmodified
        saveUninitialized: false, // don't create session until something stored
        secret: 'shhhh, very secret'
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


    app.post('/login', urlencoded, function(req, res){
        authenticate(req.body.password, function(err, result){
            if (result) {
                // Regenerate session when signing in
                // to prevent fixation
                req.session.regenerate(function(){
                    req.session.authenticated = true;
                    res.redirect('/');
                });
            } else {
                req.session.error = 'Authentication failed, please check your password.'
                res.redirect('/login');
            }
        });
    });


    app.get('/logout', function(req, res){
        // destroy the user's session to log them out
        // will be re-created next request
        req.session.destroy(function(){
            res.redirect('/');
        });
    });

    app.get('/login', function(req, res){
        res.render('login');
    });

    app.use(restrict);

    app.use(express.static('../statics/build'))

}


const credentials = {
    salt: process.env.SALT,
    hash: process.env.HASH
}

function restrict(req, res, next) {
    if (req.session.authenticated) {
        next();
    } else {
        req.session.error = 'Access denied!';
        res.redirect('/login');
    }
}

function authenticate(pass, callback) {
    if (!module.parent) console.log('authenticating %s:%s', pass);

    hash({ password: pass, salt: credentials.salt }, function (err, pass, salt, hash) {
        if (err) return callback(err);
        if (hash === credentials.hash) return callback(null, true)
        callback(new Error('invalid password'), false);
    });
}