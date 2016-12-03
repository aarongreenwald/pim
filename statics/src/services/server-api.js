export default {
    newEntry: () =>fetch('http://localhost:3000/entries', {method: 'POST'})
        .then(res => res.json()) ,
    getEntries: () => fetch('http://localhost:3000/entries')
        .then(res => res.json())    
}