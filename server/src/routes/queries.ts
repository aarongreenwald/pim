import * as db from '../data/db';
import bodyParser from 'body-parser';
import { Express } from 'express';
import {errorHandler, asArrays} from '../utils/utils';
const jsonParser = bodyParser.json();

export const setupQueriesRoutes = (app: Express) => {
    app.route('/queries/exec')
      .post(jsonParser, (req, res) => {
	req.body.writeMode ?
	  db.execQueryNoResults(req.body.sql)
	    .then(data => res.send(JSON.stringify(data)))
	    .catch(errorHandler(res)) :
	  db.execReadonlyQuery(req.body.sql)
            .then(data => res.send(
              req.body.format == "list" ?
                asArrays(data) :
                JSON.stringify(data)
            ))
            .catch(errorHandler(res))
        })
}
