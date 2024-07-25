/*
  Simple utility to expand ~ in paths to the user's home dir. Only handles tilde as
  first character.
 */
export const resolvePath = path => path.startsWith('~') ? path.replace('~', require('os').homedir()) : path;

export const errorHandler = (res) => (ex) => {
  console.error(ex)
  res.status(500).send({ex: ex, msg: ex.toString()})
}

export const asArrays = data => {
    if (!data.length) {
        return []
    }
    const keys = Object.keys(data[0])
    return [
        keys,
        ...(data.map(row => keys.map(key => row[key])))
    ]
}

export function serialize(data, req) {
    return req.headers['x-pim-aslist'] ?
      JSON.stringify(asArrays(data)) :
      JSON.stringify(data)
  }
