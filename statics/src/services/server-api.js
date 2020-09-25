import config from '../config/config';
export default {
    newEntry: () =>fetch(`${config.apiServiceUrl}/entries`, {method: 'POST'})
        .then(res => res.json()) ,
    getEntries: () => fetch(`${config.apiServiceUrl}/entries`)
        .then(res => res.json())    
}