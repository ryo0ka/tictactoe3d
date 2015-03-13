# tictactoe3d
3D Tic-Tac-Toe where the first player never loses.  
Written in a mid-term project in TECH1704 Introduction To Programming 2015 in Woodbury University.

Depends on the [tuples-homogenous-h98][tuple] and [enummapset-th][enum] packages.  
The Arduino part also depends on the [hArduino][arduino] package.

[tuple]:   https://hackage.haskell.org/package/tuples-homogenous-h98-0.1.1.0
[enum]:    https://hackage.haskell.org/package/enummapset-th
[arduino]: http://hackage.haskell.org/package/hArduino

##Usage
Prepare necessary dependencies listed above and execute `src/Main.hs`. The game starts right away. 
Input your coordinates in the format of `(x, y, z)` or the program will crush. 

##History
This program was intended to be used within an Arduino Mega board and a cube of bi-color LED lamps. 
The command-line interface (`Main.hs`) was created just for testing, however it ended up the final version
of this project as the Arduino board was found dead when no time was left to get another one. Liek if you cri evertim ;(
