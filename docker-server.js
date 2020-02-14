const http = require('http')
const url = require('url')
const qstring = require('querystring')
const { exec } = require('child_process')

const PORT = process.env.PORT || 7000
const COMMAND = 'hexskell'

const VERBOSE = false

const server = http.createServer((req, res) => {
  if (VERBOSE) { console.log('Incoming query...') }
  
  let body = []
  req.on('error', err => {
    console.errror(err)
  }).on('data', chunk => {
    body.push(chunk)
  }).on('end', () => {
    // Concatenate chunks into a string
    body = Buffer.concat(body).toString()

    // Must have a body provided
    if (!body) {
      console.error('Request must include a body')
      writeError(res, 400, 'Request must include a body')
      return
    }

    // Parse the body
    let data
    try {
      data = JSON.parse(body)
    } catch (error) {
      console.error(error)
      writeError(res, 500, 'Error parsing body JSON')
      return
    }

    // Get code from data
    let { redCode, blueCode } = data

    // Escape the code's quote chars
    const escapedRedCode = redCode.replace(/(["\\])/g, '\\$1')
    const escapedBlueCode = blueCode.replace(/(["\\])/g, '\\$1')

    // Spawn Process
    if (VERBOSE) { console.log(`Spawning process for game between "${redCode.slice(0, 8)}..." and "${blueCode.slice(0, 8)}..."`) }
    exec(`${COMMAND} "${escapedRedCode}" "${escapedBlueCode}"`, (err, stdout, stderr) => {
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
