import javax.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Random;

// All com.ericsson.otp.erlang.* imported from 'C:\Program Files\erl10.5\lib\jinterface-1.10.1\priv\OtpErlang.jar'
// (Which seems to ship as standard with Erlang/Elixir)
import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

// All connection/disconnection/query Erlang from Java using OtpErlang is taken from here:
// https://github.com/nickmcdowall/Erlang-Examples/wiki/Using-Java-to-call-an-Erlang-function

public class Main {
    private final static int FRAME_WIDTH = 500;
    private final static int FRAME_HEIGHT = 600;
    
    private final static int GRID_WIDTH = 20;
    private final static int GRID_HEIGHT = 20;
    
    private final static String BLACK_COLOR = "#000000";
    private final static String GRAY_COLOR = "#A7A7A7";
    private final static String RED_COLOR = "#FF0000";

    private final static String DEFAULT_WORDS_TEXTFILE = ".\\src\\words.txt";
    private final static String DEFAULT_COMPUTER_NAME = "DESKTOP-MF9T345";
    private final static String DEFAULT_PUZZLE = "____________________" +
                                                 "________l___________" +
                                                 "__strawberry_b______" +
                                                 "___a____m_____apple_" +
                                                 "___n____o______n____" +
                                                 "___g__mango_____a___" +
                                                 "___e_______r_____n__" +
                                                 "___r______pear____a_" +
                                                 "__lime____e__n______" +
                                                 "___n______a___g_____" +
                                                 "___e______c__cherry_" +
                                                 "__________h______h__" +
                                                 "____grape_____yuzu__" +
                                                 "_______papaya____b__" +
                                                 "________r________a__" +
                                                 "_p_____k_i_______r__" +
                                                 "_l_____i__c______b__" +
                                                 "_u_____w___o________" +
                                                 "_mandarin___t_______" +
                                                 "____________________";
    private static int[][] grid;
    private static JTextArea wordFileTextArea;
    private static JTextArea computerNameTextArea;
    private static JButton connectButton;
    private static JButton solveButton;
    private static JButton disconnectButton;

    private static JEditorPane[][] cells = new JEditorPane[GRID_WIDTH][GRID_HEIGHT];
    
    private static OtpSelf client;
    private static OtpPeer server;
    private static OtpConnection connection;
    private static ArrayList<String> wordList = null;
    
    public static void main(String[] args) {
        JFrame frame = new JFrame("Java/Erlang WordSearch Solver");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(FRAME_WIDTH, FRAME_HEIGHT);
        frame.setMinimumSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
        frame.setMinimumSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
        
        // Text Area at the top (wordslist and machine-name textboxes
        JPanel topPanel = new JPanel();
        JLabel wordFileLabel = new JLabel("Wordlist file:");
        topPanel.add(wordFileLabel);
        wordFileTextArea = new JTextArea(DEFAULT_WORDS_TEXTFILE);
        topPanel.add(wordFileTextArea);
        JLabel computerNameLabel = new JLabel("Computer name:");
        topPanel.add(computerNameLabel);
        computerNameTextArea = new JTextArea(DEFAULT_COMPUTER_NAME);
        topPanel.add(computerNameTextArea);
        
        // Main grid in centre of UI
        Container newContainer = new Container();
        newContainer.setLayout(new GridBagLayout());
        
        for(int x = 0; x < GRID_WIDTH; x++) {
            for(int y = 0; y < GRID_HEIGHT; y++) {
                GridBagConstraints contraints = new GridBagConstraints();
                cells[x][y] = new JEditorPane("text/html", "");
                cells[x][y].setText("_");
                contraints.weightx = 1;
                contraints.gridx = x;
                contraints.gridy = y;
                newContainer.add(cells[x][y], contraints);
            }
        }
        // Create the 2D array and update the cells in the UI
        grid = Get2DArray(DEFAULT_PUZZLE);
        SetCells(BLACK_COLOR);
        
        //Creating the panel at bottom containing our buttons
        JPanel bottomPanel = new JPanel();
        
        connectButton = new JButton("Connect");
        connectButton.setEnabled(true);        
        bottomPanel.add(connectButton);
        
        solveButton = new JButton("Solve");
        solveButton.setEnabled(false);        
        bottomPanel.add(solveButton);

        disconnectButton = new JButton("Disconnect");
        disconnectButton.setEnabled(false);        
        bottomPanel.add(disconnectButton);

        // Click event for 'Connect' button
        connectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                try {
                    client = new OtpSelf("client", "wordsearchcookie");
                    server = new OtpPeer("server@" + computerNameTextArea.getText());
                    connection = client.connect(server);
                    connectButton.setEnabled(false);
                    solveButton.setEnabled(true);
                    disconnectButton.setEnabled(true);                    
                } catch (IOException | OtpAuthException e) {
                    JOptionPane.showMessageDialog(null, "Unable to connect to Erlang\n" + e.getMessage());
                    System.out.println(e.getMessage());
                    System.out.println(e.getStackTrace());
                    return;
                }                
            }
        });

        // Click event for 'Solve' button
        solveButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                // If this is the first time clicking 'Solve', initialise the wordlist
                if ((wordList == null) || (wordList.size() == 0)) {
                    // Set all cells to gray initially
                    SetCells(GRAY_COLOR);                    
                    String filename = wordFileTextArea.getText();
                    try {
                        File file = new File(filename);
                        FileReader fileReader = new FileReader(file);
                        BufferedReader bufferedReader = new BufferedReader(fileReader);
                        String line;
                        wordList = new ArrayList<String>();
                        while((line = bufferedReader.readLine()) != null)
                        {
                            wordList.add(line);
                        }
                        bufferedReader.close();
                        fileReader.close();
                    } catch (IOException e) {
                        JOptionPane.showMessageDialog(null, "Unable to read from file: " + filename + "\n" + e.getMessage());
                        System.out.println(e.getMessage());
                        System.out.println(e.getStackTrace());
                        return;
                    }
                }
                
                try {                    
                    String currentWord = wordList.get(0);
                    wordList.remove(0);
                    do {
                        System.out.println("Searching for '" + currentWord + "'");
                        OtpErlangObject[] args = GetArguments(currentWord, grid);
                        connection.sendRPC("library", "search", args);
                        OtpErlangObject response = connection.receiveMsg().getMsg();
                        OtpErlangTuple responseTuple = (OtpErlangTuple)response;
                        OtpErlangTuple responseValues = (OtpErlangTuple)responseTuple.elementAt(1);
                        int row1 = ((OtpErlangLong)responseValues.elementAt(0)).intValue();
                        int col1 = ((OtpErlangLong)responseValues.elementAt(1)).intValue();
                        int row2 = ((OtpErlangLong)responseValues.elementAt(2)).intValue();
                        int col2 = ((OtpErlangLong)responseValues.elementAt(3)).intValue();
                        System.out.println("Response (" + col1 + ", " + row1 + ", " + col2 + ", " + row2 + ")");
                        // library:solve(L1, L2) returns a 4-part tuple specifying the start and
                        // end (x, y) positions of the word if it was found. If it wan't found, then
                        // all four elements are set to -1
                        if ((row1 != -1) && (col1 != -1) && (row2 != -1) && (col2 != -1)) {
                            if (row1 == row2) {
                                // If rows are equal, then we have found a horizontal word                                
                                for (int x = col1; x < col2; x++) {
                                    SetCell(x, row1, RED_COLOR, true);
                              }
                            } else if (col1 == col2){
                                // If columns are equal, they we have found a vertical word
                                for (int y = row1; y < row2; y++){
                                    SetCell(col1, y, RED_COLOR, true);
                                }
                            } else {
                                // In all other cases, we must have found a diagonal word
                                int y = row1;
                                for (int x = col1; x < col2; x++) {
                                    SetCell(x, y, RED_COLOR, true);
                                    y++;
                                }                                
                            }
                            if (wordList.size() == 0) {
                                // If there are no more words left, disable the 'Solve' button
                                // and tell the user
                                JOptionPane.showMessageDialog(null, "No more words!");
                                solveButton.setEnabled(false);
                            }
                            // Nothing else to do but exit the loop
                            return;
                        } else {
                            // If we weren't able to find the current word, get the next one
                            // and repeat the loop
                            currentWord = wordList.get(0);
                            wordList.remove(0);
                        }                        
                        // Stop looping if we run out of words
                    } while (wordList.size() != 0);
                    
                    JOptionPane.showMessageDialog(null, "No more words!");
                    solveButton.setEnabled(false);
                } catch (IOException e) {
                    JOptionPane.showMessageDialog(null, "I/O Exception\n" + e.getMessage());
                    System.out.println(e.getMessage());
                    System.out.println(e.getStackTrace());
                    return;
                } catch(OtpErlangDecodeException | OtpErlangExit | OtpAuthException e) {
                    JOptionPane.showMessageDialog(null, "Unable to query Erlang\n" + e.getMessage());
                    System.out.println(e.getMessage());
                    System.out.println(e.getStackTrace());
                    return;
                } catch (OtpErlangRangeException e) {
                    JOptionPane.showMessageDialog(null, "Unexpected response from Erlang query\n" + e.getMessage());
                    System.out.println(e.getMessage());
                    System.out.println(e.getStackTrace());
                }
            }
        });

        // Click event for 'Disconnect' button
        disconnectButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent arg0) {
                connection.close();
                connectButton.setEnabled(true);
                solveButton.setEnabled(false);
                disconnectButton.setEnabled(false);
            }
        });
        
        // Add our UI elements
        frame.getContentPane().add(BorderLayout.NORTH, topPanel);
        frame.getContentPane().add(BorderLayout.CENTER, newContainer);
        frame.getContentPane().add(BorderLayout.SOUTH, bottomPanel);        
        frame.setVisible(true);
    }
    
    private static OtpErlangObject[] GetArguments(String word, int[][] some_2d_array) {
        // Convert the word to uppercase
        word = word.toUpperCase();
        
        // Convert the word to an ASCII array
        OtpErlangObject[] wordCharacters = new OtpErlangObject[word.length()];
        for (int i = 0; i < word.length(); i++){
            wordCharacters[i] = new OtpErlangLong((int)word.charAt(i));
        }
        
        // Convert the grid to a 2D-ASCII array
        OtpErlangObject[] gridRowCharacters = new OtpErlangObject[GRID_HEIGHT];        
        for (int i = 0; i < GRID_HEIGHT; i++){
            OtpErlangObject[] gridColumnCharacters = new OtpErlangObject[GRID_WIDTH];            
            for(int j = 0; j < GRID_WIDTH; j++){
                gridColumnCharacters[j] = new OtpErlangLong(some_2d_array[j][i]);   
            }
            gridRowCharacters[i] = new OtpErlangList(gridColumnCharacters);
        }
        
        // Return a list containing our two parameters to pass to the Erlang server 
        return new OtpErlangObject[] { 
            new OtpErlangList(wordCharacters),
            new OtpErlangList(gridRowCharacters)
        };
    }

    private static int[][] Get2DArray(String str) {
        Random rand = new Random(); 
        String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        int[][] gridArray = new int[GRID_WIDTH][GRID_HEIGHT];
        str = str.toUpperCase();
        
        if (str.length() != (GRID_WIDTH * GRID_HEIGHT)) {
            JOptionPane.showMessageDialog(null, "Incorrect board size!");
            return null;
        }

        for (int x = 0; x < GRID_WIDTH; x++) {
            for (int y = 0; y < GRID_HEIGHT; y++) {
                char ch = str.charAt((y * GRID_HEIGHT) + x);
                if (ch == '_'){
                    ch = chars.charAt(rand.nextInt(26));
                }
                gridArray[x][y] = (int)ch;
            }
        }
        
        return gridArray;
    }
    
    private static void SetCells(String colorCode) {
        for (int x = 0; x < GRID_WIDTH; x++) {
            for (int y = 0; y < GRID_HEIGHT; y++) {
                SetCell(x, y, colorCode, false);
            }            
        }
    }
    
    private static void SetCell(int x, int y, String colorCode, Boolean bold) {
        if (bold) {
            cells[x][y].setText("<FONT COLOR=\"" + colorCode + "\"><B>" + (char)grid[x][y] + "</B></FONT>");
        } else {
            cells[x][y].setText("<FONT COLOR=\"" + colorCode + "\">" + (char)grid[x][y] + "</FONT>");
        }
    }
}