// Arguments will be [friendly, enemy, turnCount]
// 'friendly' and 'enemy' will be in form x,y|x,y|x,y| etc
const [
  _a,
  _fp,
  friendliesS = '',
  enemiesS = ''
] = process.argv

// Create grid (easier for writing bot given imperative stuff idk) (+1 as haskell coords start at 1,1)
let grid = []
for (let i = 0; i < 11; i++) {
  grid[i] = []
  for (let j = 0; j < 11; j++) {
    grid[i][j] = {x: i + 1, y: j + 1, team: 'neutral'}
  }
}

// Convert friendly and enemy strings to arrays
const friendlies = friendliesS.length !== 0 ? friendliesS.split('|').map(unit => unit.split(',').map(num => Number(num))) : []
const enemies = enemiesS.length !== 0 ? enemiesS.split('|').map(unit => unit.split(',').map(num => Number(num))) : []

// Add units to grid representation
for (let friendly of friendlies) {
  let [x, y] = friendly
  grid[x - 1][y - 1].team = 'friendly'
}

for (let enemy of enemies) {
  let [x, y] = enemy
  grid[x - 1][y - 1].team = 'enemy'
}

// some helpers (prob only temporarily located here)
const getAllCheckers = (grid) => {
  let checkers = []
  for (let row of grid) {
    for (let checker of row) {
      checkers.push(checker)
    }
  }

  return checkers
}

function bot (grid) {
 /* BOT-START */

  const empty = getAllCheckers(grid).filter(checker => checker.team === 'neutral')
  return empty[0]

 /* BOT-END */
}

// Get output from Bot
const {x, y} = bot(grid)

// Output to STDOUT
process.stdout.write(`${x},${y}`)
