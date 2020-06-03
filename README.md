# WordSearch
Wordsearch puzzle solver with [UI](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/Main.java) in Java using Swing and [searcher](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/library.erl) in Erlang.

![Screenshot](https://github.com/James-P-D/WordSearch/blob/master/Screenshot.gif)

## Details

### Erlang Server

The Java application connects to an Erlang server for performing the actual search of words within the grid. To start the server type `erl -sname server -setcookie wordsearchcookie`, or alternatively, just run the `StartErlangServer.bat` [file](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/StartErlangServer.bat).

After starting the server, remember to enter `c("library.erl").` to compile our library.

![Screenshot](https://github.com/James-P-D/WordSearch/blob/master/StartServer.png)

Finally, make a note of our machine name (in this case `DESKTOP-MF9T345`). We will need to enter this information into the Java UI shortly.

### Java Client

The Java application comes with a `20x20` wordsearch puzzle which contains the names of 20 items of fruit.

![UI](https://github.com/James-P-D/WordSearch/blob/master/UI.png)

At the top of the UI there is a textbox for entering the path to a word-list file. This should be a plain textfile which contains a list of words, some of which appear in our puzzle.

Currently this points to [words.txt](https://github.com/James-P-D/WordSearch/blob/master/src/WordSearch/src/words.txt) which contains the names of 20 items of fruit that appear in our grid, and 20 names of vegetables which **don't** appear in the puzzle.

At the top of the UI there is also a textbox for entering the name of the machine running our Erlang server. Obviously you will need to change this from `DESKTOP-MF9T345` to whatever appeared onscreen when you started the Erlang server.

Finally, to actually solve the puzzle, click <kbd>Connect</kbd> to connect to your Erlang server, then <kbd>Solve</kbd> to begin solving the puzzle. When the application has exhausted the list of words to search for, a short message will be displayed:

![Complete](https://github.com/James-P-D/WordSearch/blob/master/Complete.png)

You can now click <kbd>Disconnect</kbd> to terminate the connection to the server.

### Misc Notes

Note that the application will only search horizontally from left-to-right, vertically from top-to-bottom, and diagonally from top-left to bottom-right. Words that appear 'backwards' will not be found.
