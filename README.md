# Hexskell
Hex core functionality written in Haskell.

### Notes
- Board is 11x11
- A hex's neighbours are the cartesian directions, +x,-y and -x,+y.
- positions are 1-indexed through to 11 i.e x,y ∈ [1..11]
- There are two players, red and blue.
- Bots are written in javascript and passed to program which **(will when finished)** return the winner, all board states and debug information.
- Red player goes first then turns alternate (i.e red on even turn numbers, blue on odd turn numbers)
- winning player is the first to complete a connected path across the board
  from game perspective, red is left->right and blue is top->bottom

#### From the perspective of the bot scripts,
- They are always the red player
- Always creating horizontal path from left <-> right