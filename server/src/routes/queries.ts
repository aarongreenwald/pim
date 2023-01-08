import * as db from '../data/db';
import bodyParser from 'body-parser';
import {Express} from 'express';
const jsonParser = bodyParser.json();

export const setupQueriesRoutes = (app: Express) =>{
    app.route('/queries/exec')
        .post(jsonParser, (req, res) => {	   
            db.execReadonlyQuery(req.body.sql)
		.then(data => res.send(JSON.stringify(data)))
		.catch(ex => {
			  console.error(ex)
			  res.status(500).send(ex)
		})
        })
}
