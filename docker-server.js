const http = require('http')
const url = require('url')
const qstring = require('querystring')
const { exec } = require('child_process')

const PORT = process.env.PORT || 7000
const COMMAND = 'hexskell'

const VERBOSE = false

const server = http.createServer((req, res) => {
  if (VERBOSE) { console.log('Incoming query...') }
  // Parse query
  let query
  try {
    const queryString = url.parse(req.url).query
    query = qstring.parse(queryString)
  } catch (err) {
    console.error('Error parsing query string', err)
    writeError(res, 500, 'Error parsing query string', err)
    return
  }

  // Pull query params
  if (!(query.redCode && query.blueCode)) {
    console.error('Query must include "redCode" and "blueCode" parameters')
    writeError(res, 400, 'Query must include "redCode" and "blueCode" parameters')
    return
  }
  const {redCode, blueCode} = query

  // Escape quote characters
  const escapedRedCode = redCode.replace(/(["\\])/g, '\\$1')
  const escapedBlueCode = blueCode.replace(/(["\\])/g, '\\$1')

  // Spawn Process
  if (VERBOSE) { console.log(`Spawning process for game between "${redCode.slice(0, 8)}..." and "${blueCode.slice(0, 8)}..."`) }
  exec(`${COMMAND} "${redCode}" "${blueCode}"`, (err, stdout, stderr) => {
    // Was there an error?
    if (err) {
      writeError(res, 500, 'Error spawning hexskell process', err)
      return
    }

    // Parse output
    let results
    try {
      results = JSON.parse(stdout)
    } catch (err) {
      writeError(res, 500, `Error parsing child process JSON: "${stdout}"`, err)
      return
    }

    // Respond with output
    if (VERBOSE) { console.log('Game execution complete') }
    res.writeHead(200, { 'Content-Type': 'text/json' })
    res.write(JSON.stringify(results))
    res.end()
  })
})

// Start server
server.listen(PORT, () => { console.log(`Now listening on ${PORT}!`) })

// Close server gracefully when process ends
process.on('beforeExit', () => {
  console.log('Closing server')
  server.close()
})
process.on('SIGINT', () => {
  console.info('Interrupted')
  server.close()
  process.exit(0)
})

function writeError (res, code, label, error) {
  res.writeHead(code, { 'Content-Type': 'text/plain' })
  res.write(`${label}: ${error || ''}`)
  res.end()
}
