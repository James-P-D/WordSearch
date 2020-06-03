# WordSearch
Wordsearch puzzle solver with [UI](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/Main.java) in Java using Swing and [searcher](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/library.erl) in Erlang.

![Screenshot](https://github.com/James-P-D/WordSearch/blob/master/Screenshot.gif)

## Details

### Erlang Server

The Java application connects to an Erlang server for performing the actual search of words within the grid. To start the server type `erl -sname server -setcookie wordsearchcookie`, or alternatively, just run the `StartErlangServer.bat` [file](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/StartErlangServer.bat).

After starting the server, remember to enter `c("library.erl").` to compile our library:

![Screenshot](https://github.com/James-P-D/WordSearch/blob/master/StartServer.png)

Finally, make a note of our machine name (in this case `DESKTOP-MF9T345`). We will need to enter this information into the Java UI shortly.

### Java Client

The Java application comes with a `20-by-20` wordsearch puzzle which contains the names of 20 items of fruit.
