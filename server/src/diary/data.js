

const Chance = require('chance')
const chance = new Chance()
const uuid = require('uuid')

const randomContent = () => [].slice.apply(new Uint8Array(Math.floor(Math.random() * 10))).map(() => chance.paragraph()).join('\n\n')


const entriesGenerator = [].slice.apply(new Uint8Array(50)).map(() => ({
  startTimestamp: chance.date(),
  content: randomContent(),
  id: uuid()
}))

const entries = {}
entriesGenerator.forEach(e => {
  entries[e.id] = e
})

const getEntries = () => {
  const result = []
  Object.keys(entries).forEach(id => result.push(entries[id]))
  return result
}

const newEntry = () => {
  const entry = {
    id: uuid(),
    content: '',
    startTimestamp: new Date()
  }
  entries[entry.id] = entry
  return entry
}

const saveEntry = ({id, content}) => entries[id].content = content

module.exports = {
    getEntries,
    newEntry,
    saveEntry
}