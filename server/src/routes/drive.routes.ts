import * as db from '../data/drive.db';
import bodyParser from 'body-parser';
import { Express } from 'express';
const jsonParser = bodyParser.json();

// TODO extract util
const formatCsv = data => {
    if (!data.length) {
        return ""
    }
    const keys = Object.keys(data[0])
    return [
        // TODO escape , and \n
        keys.join(","),
        ...(data.map(row => keys.map(key => row[key]).join(",")))
    ].join("\n")
}

export const setupDriveRoutes = (app: Express) => {
    app.route('/drive/ls-dir')
        .post(jsonParser, (req, res) => {
            db.lsDir(req.body.path) //TODO switch get and query params, see notes
                .then(data => res.send(
                    req.body.format == "csv" ?
                        formatCsv(data) :
                        JSON.stringify(data)
                ))
                .then(data => res.send())
                .catch(ex => {
                    console.error(ex)
                    res.status(500).send(ex)
                })
        })
}
