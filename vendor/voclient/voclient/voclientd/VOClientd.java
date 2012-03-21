package voclient;		// Declare the class package

import java.io.*;		// Stuff we'll need
import java.net.*;
import java.text.*;
import java.util.*;
import dalclient.*;		// Stuff we'll really need

import javax.swing.UIManager;


/**
 *  The VOClient daemon provides a client-server interface, running 
 *  locally on a uniprocessor or multiprocessor computer, to remote 
 *  VO functionality including access to data access services.
 *
 *  VOClient is implemented as a multi-threaded daemon that listens
 *  for client connections and responds to messages that invoke the
 *  functionality provided by the DALClient library to access or query
 *  VO data services.  Clients may be written in any language since
 *  the basic interface is a client-server protocol wherein messages
 *  are passed over a socket.  Multithreading assures that each client
 *  connection maintains its own context.  Objects created by calls are
 *  stored in a HashMap for later retrieval.  What gets passed back
 *  to the client is the (language-neutral) integer hashCode of the
 *  object the client uses as a handle for the object.  Messages are
 *  a simple text string.
 *
 *  Objects persist for the lifetime of the connection since the
 *  expected typical usage is a client that connects, queries and closes
 *  the connection.  By persisting serialized objects in a database
 *  we would be able to allow multiple tasks to access objects created
 *  by an earlier query, i.e. simple tasks encapsulating a single DAL
 *  Client function such as we might do in a data analysis environment
 *  like IRAF, or through stateless web services.  State is currently
 *  maintained entirely in the VOClient daemon; the C API and bindings
 *  provide only the messaging interface.
 *
 *  Original version - Michael Fitzpatrick, NOAO, June 2006
 *  VOClient integration - DCT, 2006-July-10
 */

public class VOClientd {
    private static final boolean DEBUG      = false;
    private static final int DEF_PORT       = 6200;
    private static final int DEF_TIMEOUT    = 3600000;  	// 1 hour
    private static final String VOC_VERSION = "VOClientd v1.5.6 (Mar 12, 2010)";



    /* Main VOClient daemon class to accept client connections.  We open the 
     * public access port and wait for clients, spawning each new connection
     * on a separate thread.
     */
    public static void main (String[] args) throws IOException 
    {
        int argc    = args.length;
        int port    = DEF_PORT;
        int timeout = 0;
        boolean dbg = false;
        VOConsole cons = null;

        /* Parse args.
	 */
        if (argc == 0) {
            /* Built-in defaults, no-args unit test. */

        } else if (argc >= 1) {
            /*  Parse the commandline args.  */
	    for (int i=0; i < args.length; i++) {
                if (args[i].equals("-port"))			// -port <N>
                    port = Integer.parseInt (args[++i]);

                else if (args[i].equals("-dbg"))		// -timeout <N>
                    dbg = true;

                else if (args[i].equals("-timeout"))		// -timeout <N>
                    timeout = Integer.parseInt (args[++i]);

                else if (args[i].equals("-gui"))		// -gui
        	    cons = new VOConsole();

                else if (args[i].equals("-nogui"))		// -nogui
        	    cons = null;
	    }

        } else {
            System.out.println ("Usage: voclient.VOClientd [port] [-[no]gui]");
            System.exit(1);
        }
            

        /* Get the port number from the command line and create 
         * a ServerSocket object to watch this port for clients.
	 */
        if (DEBUG) {
	    vocLOG (cons, "Starting "+VOC_VERSION);
	    vocLOG (cons, "Listening on port: " + port);
	}

        ServerSocket sock = new ServerSocket (port);

	sock.setReuseAddress (true);		// Set the reuse policy
	//sock.setSoTimeout (DEF_TIMEOUT);	// Set the daemon timeout
	if (timeout > 0)
	    sock.setSoTimeout (timeout);	// Set the daemon timeout


        /* Loop forever, waiting for clients to connect.
	 */
        while (true) {
            /* accept() does not return until a client requests a connection.
             * Once a client connects, create an instance of our special 
	     * thread subclass to handle it. 
	     */
	    try {
                new VOClientConnection (sock.accept(),cons,timeout,dbg).start();
	    } catch (SocketTimeoutException e) {
		vocLOG (cons, "inactivity timeout, quitting.");
		break;
	    }

            if (DEBUG)
		vocLOG (cons, "new connection...");
        }

    } // end of main()


    static void vocLOG (VOConsole cons, String str)
    {
        Date d = new Date();
        DateFormat df = DateFormat.getDateTimeInstance();

        //  Just until we do a proper logging interface.
        if (cons == null)
            System.out.println (df.format(d) + ": " + str);
        else
            cons.appendText (df.format(d) + ": " + str);
    }
}
