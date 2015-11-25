### General

This is a simple TicTacToe game with a command line interface and a TCP server that allows for two players to play against each other on their own machines.

#### Usage

##### Cmd Line

Start game with

`> runhaskell TTTCLI.hs`


The two players enter their choices in an alternating way by following the instructions
displayed on the console.


##### Remote game via TCP

1. Start game server in `ghci`:
    
        > ghci TTTServer.hs
        > serveTTT <port>

2. Connect with a client in `ghci`:
          
        > ghci TTTClient.hs
        > playGame <server IP> <port>

As soon as two clients have connected, the server will spawn a new thread in which the game between the two clients is managed. 

Again, each player will receive instructions on the console that clearly explain
when to enter a choice.
