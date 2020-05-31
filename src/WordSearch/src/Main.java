import javax.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;
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
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

// All connection/disconnection/query Erlang from Java using OtpErlang is taken from here:
// https://github.com/nickmcdowall/Erlang-Examples/wiki/Using-Java-to-call-an-Erlang-function

public class Main {
	private final static int FRAME_WIDTH = 400;
	private final static int FRAME_HEIGHT = 600;
	
	private final static int GRID_WIDTH = 20;
	private final static int GRID_HEIGHT = 20;	

	private final static String DEFAULT_WORDS_TEXTFILE = "words.txt";
	private final static String DEFAULT_COMPUTER_NAME = "DESKTOP-MF9T345";
	
	private static JTextArea wordFileTextArea;
	private static JTextArea computerNameTextArea;
	private static JButton connectButton;
	private static JButton solveButton;
	private static JButton disconnectButton;

	private static JEditorPane[][] cells = new JEditorPane[GRID_WIDTH][GRID_HEIGHT];
	
	private static OtpSelf client;
	private static OtpPeer server;
	private static OtpConnection connection;
	
	public static void main(String[] args) {
		JFrame frame = new JFrame("Java/Erlang WordSearch Solver");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        frame.setSize(FRAME_WIDTH, FRAME_HEIGHT);
        frame.setMinimumSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));
        frame.setMinimumSize(new Dimension(FRAME_WIDTH, FRAME_HEIGHT));

        /////////////////////////////////////////////////////////////////////////////////////
        
        // Text Area at the top
        JPanel topPanel = new JPanel();
        JLabel wordFileLabel = new JLabel("Wordlist file:");
        topPanel.add(wordFileLabel);
        wordFileTextArea = new JTextArea(DEFAULT_WORDS_TEXTFILE);
        topPanel.add(wordFileTextArea);
        JLabel computerNameLabel = new JLabel("Computer name:");
        topPanel.add(computerNameLabel);
        computerNameTextArea = new JTextArea(DEFAULT_COMPUTER_NAME);
        topPanel.add(computerNameTextArea);
        
        /////////////////////////////////////////////////////////////////////////////////////
        
        Container newContainer = new Container();
        newContainer.setLayout(new GridBagLayout());
        //newContainer.add(middlePanel);
        
        for(int x = 0; x < GRID_WIDTH; x++) {
        	for(int y = 0; y < GRID_HEIGHT; y++) {
                GridBagConstraints contraints = new GridBagConstraints();
                cells[x][y] = new JEditorPane("text/html", "");
                cells[x][y].setText("_");
               	//textArea.setText("<FONT COLOR=\"#000000\">X</FONT>");
               	//textArea.setText("<FONT COLOR=\"#D7D7D7\">X</FONT>");
                contraints.weightx = 1;
                contraints.gridx = x;
                contraints.gridy = y;
                newContainer.add(cells[x][y], contraints);
        	}
        }

        SetCells("____________________" +
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
                 "____________________");
                
        /////////////////////////////////////////////////////////////////////////////////////
        
        //Creating the panel at bottom and adding components
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

        connectButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {
					client = new OtpSelf("client", "batman");
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

        solveButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {					
					int[] some_array = new int[3];
					some_array[0] = 1;
					some_array[0] = 2;
					some_array[0] = 3;
					//connection.sendRPC("translator", "translate", withArgs("friend", "Spanish"));
					connection.sendRPC("translator", "list_length", GetArguments(some_array));
					OtpErlangObject response = connection.receiveMsg().getMsg();
					JOptionPane.showMessageDialog(null, response.toString());
					solveButton.setEnabled(false);
				} catch (IOException | OtpErlangDecodeException | OtpErlangExit | OtpAuthException e) {
					JOptionPane.showMessageDialog(null, "Unable to query Erlang\n" + e.getMessage());
					System.out.println(e.getMessage());
					System.out.println(e.getStackTrace());
					return;
				}
			}
        });

        disconnectButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				connection.close();
				connectButton.setEnabled(true);
				solveButton.setEnabled(false);
				disconnectButton.setEnabled(false);
			}
        });
        
        /////////////////////////////////////////////////////////////////////////////////////
        
        frame.getContentPane().add(BorderLayout.NORTH, topPanel);
        frame.getContentPane().add(BorderLayout.CENTER, newContainer);
        frame.getContentPane().add(BorderLayout.SOUTH, bottomPanel);
        
        frame.setVisible(true);
	}
	
	private static OtpErlangObject[] GetArguments(int[] some_array) {
		OtpErlangObject[] otpErlangObjects = new OtpErlangObject[some_array.length];
		for (int i = 0; i < some_array.length; i++){
			otpErlangObjects[i] = new OtpErlangLong(some_array[i]);
		}
		
		return new OtpErlangObject[] { 
			new OtpErlangList(otpErlangObjects)
		};
	}
	
	private static void SetCells(String str) {
		Random rand = new Random(); 
		String chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
		str = str.toUpperCase();
		
        if (str.length() != GRID_WIDTH * GRID_HEIGHT) {
        	JOptionPane.showMessageDialog(null, "Incorrect board size!");
        	return;
        }
        	
        for (int x = 0; x < GRID_WIDTH; x++) {
            for (int y = 0; y < GRID_HEIGHT; y++) {
            	char ch = str.charAt((y * GRID_HEIGHT) + x);
            	if (chars.contains(Character.toString(ch))) {
            		cells[x][y].setText(Character.toString(ch));
            	} else {            	
            		char randChar = chars.charAt(rand.nextInt(26));
            		cells[x][y].setText(Character.toString(randChar));
            	}
            }        	
        }
	}
}
