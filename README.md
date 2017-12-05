# Connect4 game

### How to install:
Download the code and run `stack install` on the directory containing the game files

### How to play:
Just type `connect4` in any directory and the window to play the game will show up. To play you need to use the function keys: f1 will drop a tile on the first column and so on until f7. You are the red X, the computer is the green O.

### About the implementation:
The graphics are built with the Gloss library. The computer strategy is a min-max with depth 2. It has been set to this depth to make it faster, if you want you can dive into the code and change the depth as you please.
