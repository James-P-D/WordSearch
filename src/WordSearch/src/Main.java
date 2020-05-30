import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;

// All com.ericsson.otp.erlang.* imported from 'C:\Program Files\erl10.5\lib\jinterface-1.10.1\priv\OtpErlang.jar'
// (Which seems to ship as standard with Erlang/Elixir)
import com.ericsson.otp.erlang.OtpAuthException;
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

// All connection/disconnection/query Erlang from Java using OtpErlang is taken from here:
// https://github.com/nickmcdowall/Erlang-Examples/wiki/Using-Java-to-call-an-Erlang-function

public class Main {
	private final static int FRAME_WIDTH = 400;
	private final static int FRAME_HEIGHT = 500;
	
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
                GridBagConstraints c = new GridBagConstraints();
                
                JEditorPane textArea = new  JEditorPane("text/html", "");
                if (((x + y) % 3) == 0) {
                	textArea.setText("<FONT COLOR=\"#000000\">X</FONT>");
                } else {
                	textArea.setText("<FONT COLOR=\"#D7D7D7\">X</FONT>");
                }
                c.weightx = 1;
                c.gridx = x;
                c.gridy = y;
                newContainer.add(textArea, c);
        	}
        }
            	
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
					connection.sendRPC("translator", "translate", withArgs("friend", "Spanish"));
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
	
	private static OtpErlangObject[] withArgs(String word, String language) {
		return new OtpErlangObject[] { 
				new OtpErlangAtom(word),
				new OtpErlangAtom(language) 
		};
	}

}
