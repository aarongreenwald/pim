import helmet from 'helmet';
import session from 'express-session';
import pbkdf2_password from 'pbkdf2-password';
import bodyParser from 'body-parser';
import {Express, RequestHandler} from 'express';
const jsonParser = bodyParser.json();

const tlsUnavailable = process.env.NO_TLS; //dev environment
const hash = pbkdf2_password()

const verifyAuthentication: RequestHandler = (req, res, next) => {
    if (req.session!.authenticated) {
        next();
    } else {
        console.error(`Unauthenticated attempt to access: ${req.method} ${req.url}`)
        res.sendStatus(401);
    }
};

const authenticateUser: (password: string) => Promise<boolean> = password => {
    console.log('Authenticating user');

    return new Promise((resolve, reject) => {
        hash({ password, salt: credentials.salt }, (err, pass, salt, hash) => {
            if (err) {
                reject(err)
            } else if (hash === credentials.hash) {
                resolve(true)
            } else {
                resolve(false)
            }
        });
    })

};

export const setupAuth = (app: Express) => {

    app.use(helmet());

    app.set('trust proxy', 1) // in order to use cookie.secure: true with a proxy

    app.use(session({
        resave: false, // don't save session if unmodified
        saveUninitialized: false, // don't create session until something stored
        secret: 'secret', //TODO,
        cookie: {
            maxAge: 60000,
            secure: !tlsUnavailable
        }

    }));

    app.post('/login', jsonParser, async (req, res) => {
        try {
            const result = await authenticateUser(req.body.password);
            if (result) {
                // Regenerate session when signing in. Is this necessary?
                req.session!.regenerate(() => {
                    // @ts-ignore
                    req.session.authenticated = true;
                    res.sendStatus(200);
                    console.log('Auth succeeded')
                });

            } else {
                throw 'Authentication failure'
            }
        } catch (ex) {
            console.error(ex)
            console.error('Login failed')
            res.sendStatus(401);
        }


    });

    app.get('/logout', (req, res) => {
        // destroy the user's session to log them out
        // will be re-created next request
        req.session!.destroy(() => {
            res.sendStatus(200)
        });
    });

    app.use(verifyAuthentication);
}


const credentials = {
    salt: process.env.SALT,
    hash: process.env.HASH
}
