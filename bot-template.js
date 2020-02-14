// Arguments will be [friendly, enemy, turnCount]
// 'friendly' and 'enemy' will be in form x,y|x,y|x,y| etc
const [
  _a,
  _fp,
  friendliesS = '',
  enemiesS = ''
] = process.argv

// Convert friendly and enemy strings to arrays
const friendlies = friendliesS.length !== 0 ? friendliesS.split('|').map(unit => unit.split(',').map(num => Number(num))) : []
const enemies = enemiesS.length !== 0 ? enemiesS.split('|').map(unit => unit.split(',').map(num => Number(num))) : []

function bot (friendlies, enemies) {
 /* BOT-START */

 /* BOT-END */
}

// Get output from Bot
const {x, y} = bot(friendlies, enemies)

// Output to STDOUT
process.stdout.write(`${x},${y}`)
