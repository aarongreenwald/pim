/*
  Simple utility to expand ~ in paths to the user's home dir. Only handles tilde as
  first character.
 */
export const resolvePath = path => path.startsWith('~') ? path.replace('~', require('os').homedir()) : path;

export const errorHandler = (res) => (ex) => {
  console.error(ex)
  res.status(500).send({ex: ex, msg: ex.toString()})
}
