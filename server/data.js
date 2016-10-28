

const Chance = require('chance')
const chance = new Chance()

const randomContent = () => [].slice.apply(new Uint8Array(Math.floor(Math.random() * 10))).map(() => chance.paragraph()).join('\n\n')
console.log(randomContent())

const entries = [].slice.apply(new Uint8Array(50)).map(() => ({
  startTimestamp: chance.date(),
  content: randomContent()
}))

const getEntries = () => entries

module.exports = {
    getEntries
}