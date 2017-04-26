package voclient;

import java.awt.*;
import javax.swing.*;

public class VOConsole {
    JTextArea _text = new JTextArea(10, 45);
        
    /**
     *  Constructor
     */
    public VOConsole() 
    { 
	initVOConsole(); 
    }


    /**
     *  initVOConsole -- Initialize the VOConsole.
     **/
    public void initVOConsole() 
    {
        JFrame win = new JFrame();

	try {
	    UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
	} catch (Exception e) {
	    // Silently ignoring any exception.
		;
	}

        //... Set textarea's initial text, scrolling, and border.
        _text.setText("Starting VOClient Console V1.1\n");
        JScrollPane scrollingArea = new JScrollPane(_text);
        
        // Get the content pane, set layout, add to center.
        JPanel content = new JPanel();
        content.setLayout(new BorderLayout());
        content.add(scrollingArea, BorderLayout.CENTER);
        
        // Set window characteristics.
        win.setContentPane (content);
        win.setTitle ("VOClient V1.3.3 -- Daemon Console");
        win.setDefaultCloseOperation (JFrame.EXIT_ON_CLOSE);
        win.pack ();

        win.setVisible (true);
    }

    /**
     *  appendText -- Append a text string to the console dialog box.
     *
     *  @param	str	String to append.
     */
    public void appendText (String str)
    {
 	_text.append (str + "\n");

	// Keep the scroll position at the bottom on the text.
	_text.selectAll();
	_text.setCaretPosition(_text.getDocument().getLength());
    }
    
    //  Test main().
    public static void main(String[] args) 
    {
        VOConsole myconsole = new VOConsole();
 	myconsole.appendText ("test append string");
    }
}
