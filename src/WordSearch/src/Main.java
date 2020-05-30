import javax.swing.*;

import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.IOException;





import com.ericsson.otp.erlang.OtpAuthException;
//C:\Program Files\erl10.5\lib\jinterface-1.10.1\priv
import com.ericsson.otp.erlang.OtpConnection;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpPeer;
import com.ericsson.otp.erlang.OtpSelf;

public class Main {
	private final static int FRAME_WIDTH = 300;
	private final static int FRAME_HEIGHT = 300;
	private final static String DEFAULT_WORDS_TEXTFILE = "words.txt";
	private final static String DEFAULT_COMPUTER_NAME = "DESKTOP-MF9T345";

	private static JTextArea wordFileTextArea;
	private static JTextArea computerNameTextArea;
	private static JButton connectButton;
	private static JButton solveButton;

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
        wordFileTextArea = new JTextArea(DEFAULT_WORDS_TEXTFILE);
        topPanel.add(wordFileTextArea);
        computerNameTextArea = new JTextArea(DEFAULT_COMPUTER_NAME);
        topPanel.add(computerNameTextArea);
        
        /////////////////////////////////////////////////////////////////////////////////////
        
        Container newContainer = new Container();
        newContainer.setLayout(new GridBagLayout());
        //newContainer.add(middlePanel);
        
        /////////////////////////////////////////////////////////////////////////////////////
        
        //Creating the panel at bottom and adding components
        JPanel bottomPanel = new JPanel();
        
        connectButton = new JButton("Connect");
        connectButton.setEnabled(true);        
        bottomPanel.add(connectButton);
        
        solveButton = new JButton("Solve");
        solveButton.setEnabled(false);        
        bottomPanel.add(solveButton);
        
        connectButton.addActionListener(new ActionListener() {
			@Override
			public void actionPerformed(ActionEvent arg0) {
				try {
					client = new OtpSelf("client", "batman");
					server = new OtpPeer("server@" + computerNameTextArea.getText());
					connection = client.connect(server);
					connectButton.setEnabled(false);
					solveButton.setEnabled(true);
				} catch (IOException | OtpAuthException e) {
					JOptionPane.showMessageDialog(null, "Unable to connect to Erlang\n" + e.getMessage());
					System.out.println(e.getMessage());
					System.out.println(e.getStackTrace());
					return;
				}
				
				try {
					connection.sendRPC("translator", "translate", withArgs("friend", "Spanish"));
					OtpErlangObject response = connection.receiveMsg().getMsg();
					JOptionPane.showMessageDialog(null, response.toString());
				} catch (IOException | OtpErlangDecodeException | OtpErlangExit | OtpAuthException e) {
					// TODO Auto-generated catch block
					JOptionPane.showMessageDialog(null, "Unable to query erlang!");
					//e.printStackTrace();
				}				
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
