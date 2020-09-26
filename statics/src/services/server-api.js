import config from '../config/config';
/* eslint-disable */
export default {
    newEntry: () => fetch(`${config.apiServiceUrl}/entries`, {method: 'POST'})
        .then(res => res.json()) ,
    getEntries: () => fetch(`${config.apiServiceUrl}/entries`)
        .then(res => res.json()),
    getSpending: () => fetch(`${config.apiServiceUrl}/spending`)
        .then(res => res.json())
}