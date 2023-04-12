import * as db from '../data/db';
import bodyParser from 'body-parser';
import { Express } from 'express';
const jsonParser = bodyParser.json();

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

export const setupQueriesRoutes = (app: Express) => {
    app.route('/queries/exec')
        .post(jsonParser, (req, res) => {
            db.execReadonlyQuery(req.body.sql)
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
