import * as db from '../data/drive.db';
import bodyParser from 'body-parser';
import { Express } from 'express';
import {errorHandler, asArrays} from '../utils/utils';
const jsonParser = bodyParser.json();

export const setupDriveRoutes = (app: Express) => {
    app.route('/drive/ls-dir')
        .post(jsonParser, (req, res) => {
            db.lsDir(req.body.path) //TODO switch get and query params, see notes
                .then(data => res.send(
                    req.body.format == "list" ?
                        JSON.stringify(asArrays(data)) :
                        JSON.stringify(data)
                ))
                .catch(errorHandler(res))
        })
}
