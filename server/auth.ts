import helmet from 'helmet';
import session from 'express-session';
import pbkdf2_password from 'pbkdf2-password';
import bodyParser from 'body-parser';
import {Express, RequestHandler} from 'express';
const jsonParser = bodyParser.json();
import crypto from 'crypto';

const credentials = {
    salt: process.env.SALT,
    hash: process.env.HASH
}

const hash = pbkdf2_password()

function isLoggedIn(req) {
    return !!(process.env.SKIP_AUTH || req.session.authenticated);
}

const verifyAuthentication: RequestHandler = (req, res, next) => {
    if (isLoggedIn(req)) {
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
    if (!credentials.salt || !credentials.hash) {
        console.error('SALT and HASH are not available. Auth will fail.')
    }

    app.use(helmet());

    app.set('trust proxy', 1) // in order to use cookie.secure: true with a proxy

    //TODO this should be an ENV variable so that restarts don't invalidate
    //the existing sessions. Anyway I'm using an in memory store so it doesn't
    //matter until I use a persistent store.
    const secret = crypto.randomBytes(32).toString('hex');

    app.use(session({
        resave: false, // don't save session if unmodified
        saveUninitialized: false, // don't create session until something stored
        secret,
        cookie: {
            maxAge: 60000 * 60 * 24,
            secure: 'auto'
        }

    }));

    app.get('/login', (req, res) => {
        const loggedIn = isLoggedIn(req);
        res.send(loggedIn);
    });

    app.post('/login', jsonParser, async (req, res) => {
        try {
            const result = await authenticateUser(req.body.password);
            if (result) {
                // Regenerate session when signing in. Is this necessary?
                //TODO perhaps the goal is rolling sessions, but there's a config for that
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
