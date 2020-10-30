if (process.env.DOTENV) {
    require('dotenv').config({path: process.env.DOTENV})
}
import {setupRoutes} from './routes/routes';
import express from 'express';
import {setupAuth} from './auth';
const app = express()
const PORT = process.env.PORT;

app.use((req, res, next) => {
  res.header("Access-Control-Allow-Headers", "Origin, X-Requested-With, Content-Type, Accept")
  console.log(new Date(), req.method, req.url)
  next()
})

setupAuth(app);

setupRoutes(app);

express()
    .use('/api', app)
    .listen(PORT, () => console.log(`App listening on port: ${PORT}`));
