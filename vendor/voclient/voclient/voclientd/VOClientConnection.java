package voclient;

import java.io.*;		// Stuff we'll need
import java.net.*;
import java.text.*;
import java.util.*;

import java.rmi.RemoteException;
import javax.xml.rpc.ServiceException;

import dalclient.*;		// Stuff we'll really need
import voclient.*;

/**
 *  Manage a connection to the VOClient daemon.
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
public class VOClientConnection extends Thread {
    final static boolean DEBUG 	    = false;
    final static int     SO_TIMEOUT = 3600000;		// 1 hr


    int		       done = 0;
    Socket             client;
    BufferedReader     cin;			// input socket from client
    OutputStream       cout;	        	// raw output stream to client
    PrintWriter        pout;	        	// for println() etc to client
    HashMap            objTab = new HashMap();
    boolean	       user_debug = false;
    VOConsole cons     = null;

    static QueryRecord currec_obj = (QueryRecord) null;
    static int currec_objid = 0;



    // Pass the socket as a argument to the constructor
    VOClientConnection (Socket client) throws SocketException 
    {
        this.client = client;
        this.cons = null;

        // Set the thread priority down so that the ServerSocket
        // will be responsive to new clients.
        setPriority (NORM_PRIORITY - 1);
    }

    // Pass the socket as a argument to the constructor
    VOClientConnection (Socket client, VOConsole cons) throws SocketException 
    {
        this.client = client;
        this.cons = cons;

        // Set the thread priority down so that the ServerSocket
        // will be responsive to new clients.
        setPriority (NORM_PRIORITY - 1);
    }


    // Pass the socket as a argument to the constructor
    VOClientConnection (Socket client, VOConsole cons, int timeout) 
	throws SocketException 
    {
        this.client = client;
	//this.client.setSoTimeout(timeout);
        this.cons = cons;

        // Set the thread priority down so that the ServerSocket
        // will be responsive to new clients.
        setPriority (NORM_PRIORITY - 1);
    }


    // Pass the socket as a argument to the constructor
    VOClientConnection (Socket client, VOConsole cons, int timeout, 
	boolean debug) throws SocketException 
    {
        this.client = client;
	//this.client.setSoTimeout(timeout);
        this.cons = cons;
        this.user_debug = debug;

        // Set the thread priority down so that the ServerSocket
        // will be responsive to new clients.
        setPriority (NORM_PRIORITY - 1);
    }


    public void run () 
    {
        String key; 		// Command keyword
        String request;

        try {
            // Use the client socket to obtain an input stream from it.
            // For text input we wrap an InputStreamReader around 
            // the raw input stream and set ASCII character encoding. 
            // Finally, use a BufferReader wrapper to obtain 
            // buffering and higher order read methods.

            cin = new BufferedReader(new InputStreamReader(
                        client.getInputStream(), "8859_1"));

            // Now get an output stream to the client.
            cout = client.getOutputStream();

            // For text output we wrap an OutputStreamWriter around 
            // the raw output stream and set ASCII character encoding.
            // Finally, we use a PrintWriter wrapper to obtain its
            // higher level output methods.   Open in append mode. 

            pout = new PrintWriter(new OutputStreamWriter(cout,"8859_1"), true);

            // -------------------------------------------------------
            while (done == 0) {
                // First read the request line from the client
		try {
                    request = cin.readLine();
        	} catch (SocketTimeoutException e) {
	            vocLOG ("inactivity timeout: quitting thread...");
		    request = null;
		    done = 1;
	        }
		if (request != null) {
                    if (DEBUG || user_debug) vocLOG ("Request: " + request);

                    // Use a StringTokenizer to examine the request text.
                    StringTokenizer st = new StringTokenizer(request);

                    // Check that the request has a minimun number of words.
                    if (st.countTokens() >= 1) {
                        if (DEBUG || user_debug) vocLOG ("svrRCV: " + request);

		        // Pass off to the message handler
		        handleMsg (st);
                    }
                } else 
		    break;
            }

            if (DEBUG || user_debug) vocLOG ("closing connection...");

            client.close();

        } catch (IOException e) {
            vocLOG ("I/O error " + e);
        }

        // On return from run() the thread process will stop. 
        if (DEBUG || user_debug) 
	    vocLOG ("quitting thread...");
    }

    public void setDebug (boolean value) 
    {
 	user_debug = value;
    }

    void handleMsg (StringTokenizer st ) 
    {
        String key = null, tok = null;

        try {
            // Extract the command keyword from the token stream and interpret
	    // the rest of the message accordingly.

            key = st.nextToken();
	    if (key.equals ("END")) {
                vocLOG ("Got a shutdown, exiting.... ");
	        //returnResult ("OK", 0, 0, "0");
		done = 1;
		System.exit (0);

            } else if (key.equals ("QUIT")) {
                vocLOG ("Got a quit, exiting.... ");
	        returnResult ("OK", 0, 0, "0");
		done = 1;
		return;

            } else if (key.equals ("ACK")) {
		returnResult ("OK", 0, 0, "0");

            } else {
                tok = st.nextToken();		// eat opening brace

                     if (key.equals ("CALL"))   {   handleCALL (st);  }
                else if (key.equals ("RESULT")) { handleRESULT (st);  }
                else if (key.equals ("MSG"))    {    handleMSG (st);  }
                else
                    vocLOG ("Unknown Request Keyword: " + key);
            }

        } catch (NoSuchElementException nse) {
            vocLOG ("Element Handler error on key=:"+key+":");

        } catch (Exception e) {
            vocLOG ("Handler error " + e);
	    e.printStackTrace ();
        }
    }


    void handleCALL ( StringTokenizer st ) 
    {
        String key, tok;

	// CALL { objId method nparams [ type1 par1 .... ] }
	String method;
	int    objID, nparams;
 
	objID   = Integer.parseInt (st.nextToken());
	method  = st.nextToken ();
	nparams = Integer.parseInt (st.nextToken());

        if (DEBUG || user_debug) 
	    vocLOG ("Got a  CALL: objID = " + objID + 
		" method="+method + "  nparam = "+ nparams);
	    


	// Take an action depending on the method name.  We'll recall 
	//  the object from the HashMap with the appropriate type.

	////////////////////////////////////////////////////////////////////////
	//  DAL Connection Commands
	////////////////////////////////////////////////////////////////////////
	if ( method.equals ("newConnection") ) {

	    String  type = nextStringArg (st);

	    int hcode = newConnection (objID, type);

	    // Return an OK message with the new objID value
	    returnResult ("OK", 1, 1, Integer.toString(hcode));

	} else if ( method.equals ("removeConnection") ) {
	    // Recover the DAL Connection object.
	    DALConnection dal = (DALConnection) objTab.get ("dal"+objID);

	    int stat = removeConnection (objID);

	    if (stat == 0)
	        returnResult ("OK", 0, 0, "0");
	    else
	        returnResult ("ERR", 3, 1, "No such object: "+objID);

	} else if ( method.equals ("addServiceURL") ) {
	    String svc  = nextStringArg (st);

	    addServiceURL (objID, svc);

	    returnResult ("OK", 0, 0, "0");

	} else if ( method.equals ("getServiceCount") ) {

	    int count = getServiceCount (objID);

	    returnResult ("OK", 0, 1, Integer.toString(count));


	} else if ( method.equals ("getServiceURL") ) {
	    int   index = nextIntArg (st);

	    String url = getServiceURL (objID, index);	// get service URL

	    returnResult ("OK", 0, 1, url);



	////////////////////////////////////////////////////////////////////////
	//  DAL Query Commands
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("getQuery") ) {
	    String  type = nextStringArg (st);

	    int hcode = getQuery (objID, type);

	    returnResult ("OK", 1, 1, Integer.toString(hcode));

	} else if ( method.equals ("addParameter") ) {
	    String  name = nextStringArg (st);
	    String   val = nextStringArg (st);

	    int stat = addParameter (objID, name, val);

	    if (stat == 0)
	        returnResult ("OK", 0, 0, "0");
	    else
	        returnResult ("ERR", 3, 1, 
		    "Can't add param: '"+name+"' to "+objID);

	} else if ( method.equals ("getQueryString") ) {
	    String qtype = nextStringArg (st);
	    int    index = nextIntArg (st);

	    String qstring = getQueryString (objID, index);

	    returnResult ("OK", 0, 3, qstring );


	////////////////////////////////////////////////////////////////////////
	//  DAL Query Execution Commands
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("execute") ) {

	    int hcode = execute (objID);

	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	} else if ( method.equals ("getQResponse") ) {

	    int hcode = getQResponse (objID);

	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	} else if ( method.equals ("executeCSV") ) {

	    try {
	        String csv = executeDelimited (objID, ",");
		String test = (String) null;
		if (csv != (String) null)
		    test = csv.substring(0,5);
		else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	        if (csv != (String) null && test.compareTo("ERROR") != 0) {
	            returnResult ("OK", 4, 1, "-1");
    	            returnBulkData (csv.getBytes(), csv.length());
	        } else if (test.compareTo("ERROR") == 0) {
	            returnResult ("ERR", 4, 1, "-1");
    	            returnBulkData (csv.getBytes(), csv.length());
 	        } else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);
	    } catch (Exception e) {
	        returnResult ("ERR", 3, 1, e+"\n");
	    }

	} else if ( method.equals ("executeTSV") ) {

	    try {
	        String tsv = executeDelimited (objID, "\t");
		String test = (String) null;
		if (tsv != (String) null)
		    test = tsv.substring(0,5);
		else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	        if (tsv != (String) null && test.compareTo("ERROR") != 0) {
	            returnResult ("OK", 4, 1, "-1");
    	            returnBulkData (tsv.getBytes(), tsv.length());
	        } else if (test.compareTo("ERROR") == 0) {
	            returnResult ("ERR", 4, 1, "-1");
    	            returnBulkData (tsv.getBytes(), tsv.length());
 	        } else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);
	    } catch (Exception e) {
	        returnResult ("ERR", 3, 1, e+"\n");
	    }

	} else if ( method.equals ("executeASCII") ) {

	    try {
	        String ascii = executeDelimited (objID, "  ");
		String test = (String) null;
		if (ascii != (String) null)
		    test = ascii.substring(0,5);
		else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	        if (ascii != (String) null && test.compareTo("ERROR") != 0) {
	            returnResult ("OK", 4, 1, "-1");
    	            returnBulkData (ascii.getBytes(), ascii.length());
	        } else if (test.compareTo("ERROR") == 0) {
	            returnResult ("ERR", 4, 1, "-1");
    	            returnBulkData (ascii.getBytes(), ascii.length());
 	        } else
	            returnResult ("ERR", 3, 1, "Cannot execute query: "+objID);

	    } catch (Exception e) {
	        returnResult ("ERR", 3, 1, e+"\n");
	    }

	} else if ( method.equals ("executeVOTable") ) {

	    InputStream vot = executeVOTable (objID);

	    if (vot == (InputStream) null) {
	        returnResult ("ERR", 3, 1, "Cannot executeVOTable" );

	    } else {
	        returnResult ("OK", 4, 1, "-1" );

	        try {
                    byte[] buf = new byte[4096];
	            int    n, size = 0;

		    for (size=0; (n=vot.read(buf,0,buf.length)) > 0; size += n)
		        cout.write (buf, 0, n);
	    	    cout.flush();
		    cout.write ("EOF".getBytes());
	    	    cout.flush();
		    vocLOG ("Wrote "+size+" bytes to client...");
	        } catch (Exception e) {
	            vocLOG ("Failed i/o request"); // return an ERR 
	        }
	    }


	} else if ( method.equals ("getRawURL") ) {
	    String url = nextStringArg (st);

	    String result = getRawURL (url);

	    if (result != (String) null) {
	        returnResult ("OK", 4, 1, "-1");
    	        returnBulkData (result.getBytes(), result.length());
 	    } else {
	        returnResult ("ERR", 3, 1, "Cannot access URL: "+url);
	    }

	} else if ( method.equals ("executeRAW") ) {
	    ;  // not yet implemented


	////////////////////////////////////////////////////////////////////////
	//  DAL Query Response Commands
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("getRecord") ) {
	    int   index = nextIntArg (st);

	    int hcode = getRecord (objID, index);

	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Cannot get record: "+objID);


	} else if ( method.equals ("getFieldAttr") ) {
	    int   index = nextIntArg (st);
	    String attrname = nextStringArg (st);

	    String val = getFieldAttr (objID, index, attrname);

	    if (val != (String) null)
	        returnResult ("OK", 3, 1, val);
	    else
	        returnResult ("ERR", 3, 1, "No ID found");

	} else if ( method.equals ("getRecordCount") ) {

	    int count = getRecordCount (objID);

	    // Return an OK message with the record count.
	    returnResult ("OK", 1, 1, Integer.toString(count));

	} else if ( method.equals ("getAttrCount") ) {

	    int count = getAttrCount (objID);

	    // Return an OK message with the record count.
	    returnResult ("OK", 1, 1, Integer.toString(count));

	} else if ( method.equals ("getAttrList") ) {

	    String attrList = getAttributeList (objID);

	    // Return an OK message with the attribute list as a 
	    // single space-delimited string.
	    if (attrList != (String) null)
	        returnResult ("OK", 3, 1, attrList);
	    else
	        returnResult ("ERR", 3, 1, "No attributes found");

	} else if ( method.equals ("getAttribute") ) {
	    String attrname = nextStringArg (st);

	    int hcode = getAttribute (objID, attrname);

	    // Return an OK message with the attribute handle.
	    returnResult ("OK", 1, 1, Integer.toString(hcode));

	} else if ( method.equals ("getDataset") ) {
	    String  url = nextStringArg (st);

	    returnResult ("OK", 4, 1, "-1" );

	    try {
                byte[] buf = new byte[8192];
	        long   n, size = 0;

		URL link = new URL (url);
		InputStream in = link.openStream();

		for (size=0; (n=in.read(buf,0,8192)) > 0; size += n)
		    cout.write (buf, 0, (int)n);
	    	cout.flush();
		cout.write ("EOF".getBytes());
	    	cout.flush();
		vocLOG ("Wrote "+size+" bytes to client...");
	    } catch (Exception e) {
	        vocLOG ("Failed i/o request"); // return an ERR 
	    }

	} else if ( method.equals ("intValue") ) {
	    int ival = intValue (objID);
	    returnResult ("OK", 1, 1, Integer.toString(ival));
	} else if ( method.equals ("floatValue") ) {
	    double dval = floatValue (objID);
	    returnResult ("OK", 2, 1, Double.toString(dval));
	} else if ( method.equals ("stringValue") ) {
	    String s = stringValue (objID);
	    returnResult ("OK", 3, 1, s);


	////////////////////////////////////////////////////////////////////////
	//  Sesame Name Resolver Commands
	//
	//	    sr = nameResolver  (target)
	//	       pos = srGetPOS  (sr)
	//	         ra = srGetRA  (sr)
	//	       dec = srGetDEC  (sr)
	//
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("nameResolver") ) {
	    String target = nextStringArg (st);

	    int hcode = nameResolver (target);

	    // Return an OK message with the record count.
	    returnResult ("OK", 1, 1, Integer.toString(hcode));

	} else if ( method.equals ("srGetPOS") ) {
	    returnResult ("OK", 3, 1, srGetPOS (objID) );
	} else if ( method.equals ("srGetRA") ) {
	    returnResult ("OK", 1, 1, Double.toString( srGetRA (objID) ));
	} else if ( method.equals ("srGetDEC") ) {
	    returnResult ("OK", 1, 1, Double.toString( srGetDEC (objID) ));
	} else if ( method.equals ("srGetRAErr") ) {
	    returnResult ("OK", 1, 1, Double.toString( srGetRAErr (objID) ));
	} else if ( method.equals ("srGetDECErr") ) {
	    returnResult ("OK", 1, 1, Double.toString( srGetDECErr (objID) ));
	} else if ( method.equals ("srGetOtype") ) {
	    returnResult ("OK", 3, 1, srGetOtype (objID) );


	////////////////////////////////////////////////////////////////////////
	//  IMCCE SkyBoT Client Commands
	//
	//        sb = VOCSkybot (ra, dec, rsz, dsz, jd_epoch)
 	//
 	//      count = sb.NObjs ()                         // Num objects found
 	//   str = sb.getStrAttr (attrname, index)          // Get string attr
 	//  dval = sb.getDblAttr (attrname, index)          // Get double attr
 	//
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("skybot") ) {
	    double ra    = nextFloatArg (st);
	    double dec   = nextFloatArg (st);
	    double rsz   = nextFloatArg (st);
	    double dsz   = nextFloatArg (st);
	    double epoch = nextFloatArg (st);

	    int hcode = skybot (ra, dec, rsz, dsz, epoch);

	    // Return an OK message with the record count.
	    returnResult ("OK", 1, 1, Integer.toString (hcode) );

	} else if ( method.equals ("sbNObjs") ) {
	    returnResult ("OK", 2, 1, Integer.toString (sbNObjs (objID)) );

	} else if ( method.equals ("sbStrAttr") ) {
	    String attr  = nextStringArg (st);
	    int index    = nextIntArg (st);

	    String value = sbStrAttr (objID, attr, index);

	    returnResult ("OK", 3, 1, value );

	} else if ( method.equals ("sbDblAttr") ) {
	    String attr  = nextStringArg (st);
	    int index    = nextIntArg (st);

	    double value = sbDblAttr (objID, attr, index);

	    returnResult ("OK", 1, 1, Double.toString (value) );



	////////////////////////////////////////////////////////////////////////
	//  Registry Query and Result Commands
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("regSearch") ) {		// obj
	    String term1 = nextStringArg (st);
	    String term2 = nextStringArg (st);
	    boolean orValues = false;

	    if (term2 != null)			// in case no term2 specified
	 	orValues = nextBoolArg (st);

	    int hcode = regSearch (term1, term2, orValues);

	    // Return an OK message with the record count.
	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Error doing Registry Search.");

	} else if ( method.equals ("regConstSvcType") ) {	// void
	    String type = nextStringArg (st);

	    regConstSvcType (objID, type);

	    returnResult ("OK", 1, 1, "");

	} else if ( method.equals ("regConstWaveband") ) {	// void
	    String type = nextStringArg (st);

	    regConstWaveband (objID, type);

	    returnResult ("OK", 1, 1, "");

	} else if ( method.equals ("regDALOnly") ) {		// void
	    int value = nextIntArg (st);

	    regDALOnly (objID, value);

	    returnResult ("OK", 1, 1, "");

	} else if ( method.equals ("regSortRes") ) {		// void
	    int value = nextIntArg (st);

	    regSortRes (objID, value);

	    returnResult ("OK", 1, 1, "");


	} else if ( method.equals ("regSearchBySvc") ) {	// obj
	    String svc = nextStringArg (st);
	    String term = nextStringArg (st);
	    boolean orValues = false;

	    if (term != null)			// in case no term specified
	 	orValues = nextBoolArg (st);

	    int hcode = regSearchBySvc (svc, term, orValues);

	    // Return an OK message with the record count.
	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Error doing Registry Search.");

	} else if ( method.equals ("regQuery") ) {		// obj
	    String term = nextStringArg (st);
	    boolean orValues = nextBoolArg (st);

	    int hcode = regQuery (term, orValues);

	    // Return an OK message with the record count.
	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Cannot get Query object.");

	} else if ( method.equals ("regAddSearchTerm") ) {	// void
	    String term = nextStringArg (st);
	    boolean orValues = nextBoolArg (st);

	    regAddSearchTerm (objID, term, orValues);

	    returnResult ("OK", 1, 1, "");

	} else if ( method.equals ("regRemoveSearchTerm") ) {	// void

	    regRemoveSearchTerm (objID, nextStringArg (st));
	    returnResult ("OK", 1, 1, "");

	} else if ( method.equals ("regGetSTCount") ) {		// int

	    returnResult ("OK", 1, 1, Integer.toString(regGetSTCount(objID)));

	} else if ( method.equals ("regGetQueryString") ) {	// string

	    String qstring = regGetQueryString(objID);
	    if (qstring == null)
	    	returnResult ("ERR", 3, 1, "Error getting query string");
	    else
	    	returnResult ("OK", 3, 1, qstring);

	} else if ( method.equals ("regExecute") ) {		// obj

	    int hcode = regExecute (objID);

	    // Return a statis message with the object hashCode.
	    if (hcode > 0)
	        returnResult ("OK", 0, 1, Integer.toString(hcode));
	    else
	        returnResult ("ERR", 3, 1, "Cannot get Query object.");

	} else if ( method.equals ("regExecuteRaw") ) {		// obj

	    InputStream s = regExecuteRaw (objID);

	    returnResult ("OK", 4, 1, "-1" );

	    try {
                byte[] buf = new byte[4096];
	        int    n, size = 0;

		for (size=0; (n=s.read(buf,0,buf.length)) > 0; size += n)
		    cout.write (buf, 0, n);
		cout.write ("EOF".getBytes());
	    	cout.flush();
		vocLOG ("Wrote "+size+" bytes to client...");
	    } catch (Exception e) {
	        vocLOG ("Failed i/o request"); // return an ERR 
	    }



	} else if ( method.equals ("resGetCount") ) {		// int
            VOCRegistryQueryResult r = 
		(VOCRegistryQueryResult) objTab.get ("reg"+objID);

	    if (r != (VOCRegistryQueryResult) null)
	        returnResult ("OK", 1, 1, Integer.toString(r.getResultCount()));
	    else
	        returnResult ("ERR", 1, 1, "No results");

	} else if ( method.equals ("resGetString") ) {		// string
	    String attr = nextStringArg (st);
	    int   index = nextIntArg (st);

	    returnResult ("OK", 3, 1, resGetStr(objID, attr, index));

	} else if ( method.equals ("resGetFloat") ) {		// double
	    String attr = nextStringArg (st);
	    int   index = nextIntArg (st);

	    returnResult ("OK", 2, 1, 
		Double.toString (resGetFloat (objID, attr, index) ));

	} else if ( method.equals ("resGetInt") ) {		// int
	    String attr = nextStringArg (st);
	    int   index = nextIntArg (st);

	    returnResult ("OK", 1, 1, 
		Integer.toString (resGetInt (objID, attr, index) ));



	////////////////////////////////////////////////////////////////////////
	//  Utility/Messaging Commands
	////////////////////////////////////////////////////////////////////////
	} else if ( method.equals ("validateObject") ) {

	    String type =  getObjType (objID);

	    if (type.compareToIgnoreCase ("unknown") == 0)
	        returnResult ("ERR", 1, 1, "invalid object");
	    else
	        returnResult ("OK", 1, 1, "1");


	////////////////////////////////////////////////////////////////////////
	//  Unknown Method.
	////////////////////////////////////////////////////////////////////////
	} else {
	    vocLOGErr ("Unknown method call '"+method+"'");
	}
    }

	    
    void returnResult (String stat, int type, int npar, String val)
    {
	String s;

	if (DEBUG || user_debug) 
	    vocLOG ("RESULT { "+stat+" "+type+" "+npar+" "+val+" }");

	int status = (stat.equals ("OK") ? 0 : 1);

	/* String semicolons from text results except for URLs.
         * Unfortunately the semicolon is used as a delimiter in the
         * messaging interface and we need this to avoid a conflict.
	 */
	if (val.startsWith ("http"))
	    s = val;
	else
	    s = val.replace (';',' ');

	if (type == 3) 
	   pout.println("RESULT { "+status+" "+type+" "+npar+" \""+s+"\" };");
	else
	   pout.println("RESULT { "+status+" "+type+" "+npar+" "+s+" };");
    }

    void returnBulkData (byte[] data, int size)
    {
	vocLOG ("Writing "+size+" bytes to client....");
	try {
	    //cout.write (data, 0, size);
	    cout.write (data);
	    cout.flush();
	    cout.write ("EOF".getBytes());
	    cout.flush();

	} catch (SocketException e) {
	    vocLOG ("Bad socket write....");
	    ;
	} catch (IOException e) {
	    vocLOG ("Bad data write....");
	    ;
	}
    }

    void handleRESULT (StringTokenizer st ) 
    {
        String key, tok;

	// RESULT { status [ type length value] }
        vocLOG ("Got a  RESULT: ");
    }


    void handleMSG (StringTokenizer st ) 
    {
        String key, tok;

	// MSG { class str }
        vocLOG ("Got a  MSG: ");
    }

    void vocLOG (String str) 
    {
	Date d = new Date();
	DateFormat df = DateFormat.getDateTimeInstance();

	//  Just until we do a proper logging interface.
        if (cons == null)
	    System.out.println (df.format(d) + ": " + str);
        else
	    cons.appendText (df.format(d) + ": " + str);
    }

    void vocLOGErr (String str) 
    {
	Date d = new Date();
	DateFormat df = DateFormat.getDateTimeInstance();

	//  Just until we do a proper logging interface.
        if (cons == null)
	    System.err.println (df.format(d) + ": ERR: " + str);
        else
	    cons.appendText (df.format(d) + ": ERR: " + str);
    }



    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    //+++++++++++++++++					++++++++++++++++++++
    //+++++++++++++++++ 	PRIVATE METHODS		++++++++++++++++++++
    //+++++++++++++++++					++++++++++++++++++++
    //++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

    //======================================================================
    ///	  Connection Methods
    //======================================================================

    private int newConnection (int objID, String type)
    {
	int hcode = -1;

	// Recover the DAL Connection object.
	if (type.equals ("DAL")) {
            DALConnection dal = new DALConnection ();
	    hcode = dal.hashCode();
	    objTab.put ("dal"+hcode, dal);

	} else if (type.equals ("Cone")) {
	    ConeConnection cone = new ConeConnection();
	    hcode = cone.hashCode();
	    objTab.put ("cone"+hcode, cone);

	} else if (type.equals ("Siap")) {
	    SiapConnection siap = new SiapConnection();
	    hcode = siap.hashCode();
	    objTab.put ("siap"+hcode, siap);

	} else if (type.equals ("Ssap")) {
	    SsapConnection ssap = new SsapConnection();
	    hcode = ssap.hashCode();
	    objTab.put ("ssap"+hcode, ssap);
	}

	return (hcode);
    }

    private void addServiceURL (int objID, String service_url)
    {
	// Recover the Connection object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALConnection dal = (DALConnection) objTab.get("dal"+objID);
	    dal.addService (service_url);

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeConnection cone = (ConeConnection) objTab.get("cone"+objID);
	    cone.addService (service_url);

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapConnection siap = (SiapConnection) objTab.get("siap"+objID);
	    siap.addService (service_url);

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapConnection ssap = (SsapConnection) objTab.get("ssap"+objID);
	    ssap.addService (service_url);
	}
    }

    private int removeConnection (int objID)
    {
	String type = "dal";

	// Recover the Connection object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALConnection dal = (DALConnection) objTab.get ("dal"+objID);
	    type = "dal";

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeConnection cone = (ConeConnection) objTab.get ("cone"+objID);
	    type = "cone";

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapConnection siap = (SiapConnection) objTab.get ("siap"+objID);
	    type = "siap";

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapConnection ssap = (SsapConnection) objTab.get ("ssap"+objID);
	    type = "ssap";
	}

	if (objTab.remove (type+objID) != null)
	    return 0;
	else
	    return 1;
    }

    private int getServiceCount (int objID)
    {
	int count = -1;

	// Recover the Connection object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALConnection dal = (DALConnection) objTab.get("dal"+objID);
	    count = dal.getServiceCount ();

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeConnection cone = (ConeConnection) objTab.get("cone"+objID);
	    count = cone.getServiceCount ();

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapConnection siap = (SiapConnection) objTab.get("siap"+objID);
	    count = siap.getServiceCount ();

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapConnection ssap = (SsapConnection) objTab.get("ssap"+objID);
	    count = ssap.getServiceCount ();
	}

	return (count);
    }


    private String getServiceURL (int objID, int index)
    {
	String url = null;

	// Recover the Connection object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALConnection dal = (DALConnection) objTab.get("dal"+objID);
	    url = dal.getServiceURL(index);

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeConnection cone = (ConeConnection) objTab.get("cone"+objID);
	    url = cone.getServiceURL(index);

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapConnection siap = (SiapConnection) objTab.get("siap"+objID);
	    url = siap.getServiceURL(index);

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapConnection ssap = (SsapConnection) objTab.get("ssap"+objID);
	    url = ssap.getServiceURL(index);
	}

	return (url);
    }


    //======================================================================
    ///	  Query Methods
    //======================================================================

    private int getQuery (int objID, String type)
    {
        int hcode = -1;


	// Recover the Connection object.
	if (type.equals ("DAL")) {
	    DALConnection dal = (DALConnection) objTab.get ("dal"+objID);
	    if (dal == (DALConnection) null)
	        return (-1);
	    DALQuery query = dal.getDALQuery();	// get Query context
	    hcode = query.hashCode();
	    objTab.put ("dal"+hcode, query);

	} else if (type.equals ("Cone")) {
	    ConeConnection cone = (ConeConnection)objTab.get ("cone"+objID);
	    if (cone == (ConeConnection) null)
	        return (-1);
	    ConeQuery query = cone.getConeQuery();	// get Query context
	    hcode = query.hashCode();
	    objTab.put ("cone"+hcode, query);

	} else if (type.equals ("Siap")) {
	    SiapConnection siap = (SiapConnection)objTab.get ("siap"+objID);
	    if (siap == (SiapConnection) null)
	        return (-1);
	    SiapQuery query = siap.getSiapQuery();	// get Query context
	    hcode = query.hashCode();
	    objTab.put ("siap"+hcode, query);

	} else if (type.equals ("Ssap")) {
	    SsapConnection ssap = (SsapConnection)objTab.get ("ssap"+objID);
	    if (ssap == (SsapConnection) null)
	        return (-1);
	    SsapQuery query = ssap.getSsapQuery();	// get Query context
	    hcode = query.hashCode();
	    objTab.put ("ssap"+hcode, query);
	}

	return (hcode);
    }

    private int addParameter (int objID, String name, String value)
    {
	// Recover the Query context object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALQuery query = (DALQuery) objTab.get ("dal"+objID);
	    query.addParameter (name, value);

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);
	    query.addParameter (name, value);

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);
	    query.addParameter (name, value);

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);
	    query.addParameter (name, value);
	}

	return (0);
    }

    private String getQueryString (int objID, int index)
    {
	String qstring = null;

	// Recover the Query context object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALQuery query = (DALQuery) objTab.get ("dal"+objID);
	    if (query == (DALQuery) null)
	        return ("");
	    qstring = query.getQueryString (index);

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);
	    if (query == (ConeQuery) null)
	        return ("");
	    qstring = query.getQueryString (index);

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);
	    if (query == (SiapQuery) null)
	        return ("");
	    qstring = query.getQueryString (index);

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);
	    if (query == (SsapQuery) null)
	        return ("");
	    qstring = query.getQueryString (index);
	}

	return (qstring);
    }


    //======================================================================
    ///	  Execute Methods
    //======================================================================

    private int execute (int objID)
    {
	int hcode = -1;

	// Recover the Query context object.
	if (objTab.containsKey ("dal"+objID)) {
	    DALQuery query = (DALQuery) objTab.get ("dal"+objID);

	} else if (objTab.containsKey ("cone"+objID)) {
	    ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);

	    try {
		vocLOG ("Beginning cone query...." + query);
	        QueryResponse   qr = query.execute();

		if (qr != (QueryResponse)null)
	            hcode = qr.hashCode(); 	       	// add to object table
	        objTab.put ("cone"+hcode, qr);

	        return (hcode);				// return the hcode

	    } catch (Exception e) {
		vocLOG ("Execute cone exception: " + e);
	        return (-1); 				// return an ERR 
	    }

	} else if (objTab.containsKey ("siap"+objID)) {
	    SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);

	    try {
		vocLOG ("Beginning siap query...." + query);
	        QueryResponse   qr = query.execute();

		if (qr != (QueryResponse)null)
	            hcode = qr.hashCode(); 	       	// add to object table
	        objTab.put ("siap"+hcode, qr);

	        return (hcode);				// return the hcode

	    } catch (Exception e) {
		vocLOG ("Execute siap exception: " + e);
	        return (-1); 				// return an ERR 
	    }

	} else if (objTab.containsKey ("ssap"+objID)) {
	    SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);

	    try {
		vocLOG ("Beginning ssap query...." + query);
	        QueryResponse   qr = query.execute();

		if (qr != (QueryResponse)null)
	            hcode = qr.hashCode(); 	       	// add to object table
	        objTab.put ("ssap"+hcode, qr);

	        return (hcode);				// return the hcode

	    } catch (Exception e) {
		vocLOG ("Execute ssap exception: " + e);
	        return (-1); 				// return an ERR 
	    }
	}

	return (0);
    }

    private int getQResponse (int objID)
    {
	int hcode = 0;
	QueryResponse qr = (QueryResponse) null;

	try {
	    if (objTab.containsKey ("dal"+objID)) {
	        DALQuery query = (DALQuery) objTab.get ("dal"+objID);
//		if (query != (SiapQuery) null) {
//		    qr = query.getQResponse();
//		    if (qr != (QueryResponse)null) {
//	                hcode = qr.hashCode(); 	       	// add to object table
//	                objTab.put ("dal"+hcode, qr);
//		    }
//		}

	    } else if (objTab.containsKey ("cone"+objID)) {
	        ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);
//		if (query != (SiapQuery) null) {
//		    qr = query.getQResponse();
//		    if (qr != (QueryResponse)null) {
//	                hcode = qr.hashCode(); 	       	// add to object table
//	                objTab.put ("cone"+hcode, qr);
//		    }
//		}

	    } else if (objTab.containsKey ("siap"+objID)) {
	        SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);
		if (query != (SiapQuery) null) {
		    qr = query.getQResponse();
		    if (qr != (QueryResponse)null) {
	                hcode = qr.hashCode(); 	       	// add to object table
	                objTab.put ("siap"+hcode, qr);
		    }
		}

	    } else if (objTab.containsKey ("ssap"+objID)) {
	        SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);
		if (query != (SsapQuery) null) {
		    qr = query.getQResponse();
		    if (qr != (QueryResponse)null) {
	                hcode = qr.hashCode(); 	       	// add to object table
	                objTab.put ("ssap"+hcode, qr);
		    }
		}
	    }
	} catch (Exception e) {
	    // return an ERR 
	    return (-1);
	}

	return (hcode);
    }

    private String executeDelimited (int objID, String delim)
    {
	ByteArrayOutputStream baos = new ByteArrayOutputStream();
	PrintStream ps = new PrintStream(new BufferedOutputStream(baos));

	try {
	    if (objTab.containsKey ("dal"+objID)) {
	        DALQuery query = (DALQuery) objTab.get ("dal"+objID);
		if (query != (DALQuery) null)
		    query.executeDelimited (ps, delim);

	    } else if (objTab.containsKey ("cone"+objID)) {
	        ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);
		if (query != (ConeQuery) null)
		    query.executeDelimited (ps, delim);

	    } else if (objTab.containsKey ("siap"+objID)) {
	        SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);
		if (query != (SiapQuery) null) {
		    query.executeDelimited (ps, delim);

		    QueryResponse qr = query.getQResponse();
		    int  hcode = -1;
		    if (qr != (QueryResponse)null)
	                hcode = qr.hashCode(); 	       	// add to object table
	            objTab.put ("siap"+hcode, qr);
		}

	    } else if (objTab.containsKey ("ssap"+objID)) {
	        SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);
		if (query != (SsapQuery) null) {
		    query.executeDelimited (ps, delim);

		    QueryResponse qr = query.getQResponse();
		    int  hcode = -1;
		    if (qr != (QueryResponse)null)
	                hcode = qr.hashCode(); 	       	// add to object table
	            objTab.put ("ssap"+hcode, qr);
		}
	    }
	} catch (Exception e) {
	    // return an ERR 
	    vocLOGErr (e.toString());
	    return ("ERROR: " + e.toString() + "\n");
	}
	
	ps.flush();
	return ( new String(baos.toByteArray()) );
    }


    private InputStream executeVOTable (int objID)
    {
	InputStream in = (InputStream) null;

	try {
	    if (objTab.containsKey ("dal"+objID)) {
	        DALQuery query = (DALQuery) objTab.get ("dal"+objID);
		in = query.executeRaw ();

	    } else if (objTab.containsKey ("cone"+objID)) {
	        ConeQuery query = (ConeQuery) objTab.get ("cone"+objID);
		in = query.executeRaw ();

	    } else if (objTab.containsKey ("siap"+objID)) {
	        SiapQuery query = (SiapQuery) objTab.get ("siap"+objID);
		in = query.executeRaw ();

	    } else if (objTab.containsKey ("ssap"+objID)) {
	        SsapQuery query = (SsapQuery) objTab.get ("ssap"+objID);
		in = query.executeRaw ();
	    }
	} catch (Exception e) {
	    // return an ERR 
	    vocLOGErr ("executeVOTable: Failed request");	
	    return ( (InputStream) null );
	}

	return (in);
    }


    private String getRawURL (String baseURL)
    {
        String result = null;

        try {
            URL http = new URL(baseURL);
            InputStream in = http.openStream();
            StringBuffer out = new StringBuffer();
            byte[] b = new byte[8192];

            for (int n;  (n = in.read(b,0,8192)) != -1;  )
              out.append(new String(b, 0, n));

            result = out.toString();

        } catch (Exception e ) {
            vocLOG ("getRawURL: " + e);
        }

        return (result);
    }


    private String executeRAW (int objID)
    {
	return (null);
    }


    //======================================================================
    //   Dataset methods
    //======================================================================
    private int getRecord (int objID, int index) 
    {
        String type = getObjType(objID);
        QueryResponse qr = (QueryResponse) objTab.get (type+objID);
	if (qr == (QueryResponse) null)
	    return (-1);

	if (index >= qr.getRecordCount() || index < 0)
	    return (-1);                        // out of range index error

	QueryRecord rec = qr.getRecord (index);

	int hcode = rec.hashCode(); 	       	// add to object table
	objTab.put (type+hcode, rec);

	currec_obj = rec;
	currec_objid = hcode;

	return (hcode);				// return the hcode
    }

    private int getRecordCount (int objID) 
    {
        QueryResponse qr = (QueryResponse) null;
	int count = -1;

	// Recover the QueryResponse object.
        qr = (QueryResponse) objTab.get (getObjType(objID)+objID);
	if (qr == (QueryResponse) null)
	    return (-1);

	count = qr.getRecordCount ();

	return (count);
    }

    private String getFieldAttr (int objID, int index, String what) 
    {
        QueryResponse qr = (QueryResponse) objTab.get(getObjType(objID)+objID);

	if (qr == (QueryResponse) null) {
	    return ("");
	} else {
	    return ( qr.getFieldAttr (index, what) );
	}
    }

    private int getAttrCount (int objID)
    {
	// Recover the QueryRecord object.
        QueryRecord rec = (QueryRecord) objTab.get (getObjType(objID)+objID);
	if (rec == (QueryRecord) null)
	    return (-1);
	int count = rec.getAttributeCount ();

	return (count);
    }

    private String getAttributeList (int objID)
    {
	String attrList = null;
        QueryRecord rec = (QueryRecord) null;


	// Recover the QueryRecord object.
	if (currec_objid == objID)
            rec = (QueryRecord) currec_obj;
	else
            rec = (QueryRecord) objTab.get (getObjType(objID)+objID);
	if (rec == (QueryRecord) null)
	    return ("");

	LinkedHashMap kmap = rec.getMap();
	Set s = kmap.keySet();
	for (Iterator i = s.iterator(); i.hasNext(); ) {
	    String skey = (String) i.next();
	    attrList = ((attrList == null) ? skey : (attrList + " " + skey));
	}

	return (attrList);
    }

    private int getAttribute (int objID, String attrname)
    {
        String type = getObjType(objID);
        QueryRecord rec = (QueryRecord) null;
	int hcode = -1;

	// Recover the QueryRecord object.
//	if (currec_objid == objID)
//            rec = (QueryRecord) currec_obj;
//	else
            rec = (QueryRecord) objTab.get (getObjType(objID)+objID);
	if (rec == (QueryRecord) null)
	    return (-1);

	try {
	    QRAttribute v = rec.getAttribute (attrname);

	    hcode = v.hashCode();       	// add to object table
	    objTab.put (type+hcode, v);

	} catch (NullPointerException e) {
	    ;
	}

	return (hcode);				// return the hcode
    }

    
    //======================================================================

    private int intValue (int objID)
    {
	// Recover the QueryRecord object.
        QRAttribute v = (QRAttribute) objTab.get (getObjType(objID)+objID);
	if (v == (QRAttribute) null)
	    return (-1);
	int ival = v.intValue ();

	return (ival);
    }

    private double floatValue (int objID)
    {
	// Recover the QueryRecord object.
        QRAttribute v = (QRAttribute) objTab.get (getObjType(objID)+objID);
	if (v == (QRAttribute) null)
	    return (-1.0);
	double dval = v.doubleValue ();

	return (dval);
    }

    private String stringValue (int objID)
    {
	// Recover the QueryRecord object.
        QRAttribute v = (QRAttribute) objTab.get (getObjType(objID)+objID);
	if (v == (QRAttribute) null)
	    return ("");
	String s = v.stringValue().trim();

	return ('"'+s+'"');
    }


    //======================================================================
    //   Sesame Resolver methods
    //======================================================================

    private int nameResolver (String target) {
        VOCSesame sr = new VOCSesame (target);
	if (sr == (VOCSesame) null)
	    return (-1);

	int hcode = sr.hashCode(); 	       	// add to object table
	objTab.put ("sesame"+hcode, sr);

	return (hcode);				// return the hcode
    }

    private String srGetPOS (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return ("");
	if (sr.pos == (String) null)
	    return ("null");
	return (sr.pos);
    }

    private String srGetOtype (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return ("");
	if (sr.otype == (String) null)
	    return ("null");
	return (sr.otype);
    }

    private double srGetRA (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return (0.0);
	return (sr.ra);
    }

    private double srGetDEC (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return (0.0);
	return (sr.dec);
    }

    private double srGetRAErr (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return (0.0);
	return (sr.raERR);
    }

    private double srGetDECErr (int objID) {
        VOCSesame sr = (VOCSesame) objTab.get ("sesame"+objID);
	if (sr == (VOCSesame) null)
	    return (0.0);
	return (sr.decERR);
    }


    //======================================================================
    //   SkyBoT methods
    //======================================================================

    private int skybot (double ra, double dec, double rsz, double dsz, 
	double epoch)
    {
 	try {
            VOCSkybot sb = new VOCSkybot (ra, dec, rsz, dsz, epoch);

	    int hcode = sb.hashCode(); 	       	// add to object table
	    objTab.put ("skybot"+hcode, sb);

	    return (hcode);			// return the hcode
	} catch (Exception e) {
	    return (-1);
	}
    }

    private String sbStrAttr (int objID, String attr, int index) {
        VOCSkybot sb = (VOCSkybot) objTab.get ("skybot"+objID);
	return (sb.getStrAttr (attr, index));
    }

    private double sbDblAttr (int objID, String attr, int index) {
        VOCSkybot sb = (VOCSkybot) objTab.get ("skybot"+objID);
	return (sb.getDblAttr (attr, index));
    }

    private int sbNObjs (int objID) {
        VOCSkybot sb = (VOCSkybot) objTab.get ("skybot"+objID);
	return (sb.NObjs());
    }



    //======================================================================
    //   Registry Query methods
    //======================================================================
	    
    private int regSearch (String term1, String term2, boolean orValues)
    {
	int hcode = -1;
	VOCRegistryQuery query = (VOCRegistryQuery) null;

	if (term1 != null || term2 != null)
	    vocLOG ("regSearch: t1 = '"+term1+"'  t2 = '"+term2+"'");

	if (term1 != null) {
	    query = new VOCRegistryQuery (term1, false);
	    if (term2 != null)
        	query.addSearchTerm (term2, orValues);
	} else if (term2 != null) {
	    query = new VOCRegistryQuery (term2, orValues);
	} else {
	    // NULL search terms.
	    return hcode;
	}

	// vocLOGErr ("qstring = '"+query.getQueryString()+"'");

	try {
	    VOCRegistryQueryResult res = query.execute ();

	    hcode = res.hashCode(); 	       		// add to object table
	    objTab.put ("reg"+hcode, res);

	} catch (ServiceException ex) {
	    vocLOGErr ("regSearch: Service Exception: "+ex);
	    hcode = -1;					// need to add errmsg
	} catch (RemoteException ex) {
	    vocLOGErr ("regSearch: Remote Exception: "+ex);
	    hcode = -1;
	} catch (Exception ex) {
	    vocLOGErr ("regSearch: Exception: "+ex);
	    hcode = -1;
	} 

	return (hcode);					// return the hcode
    }


    private int regSearchBySvc (String svc, String term, boolean orValues)
    {
	return ( regSearch ("ResourceType like '%"+svc+"%'", term, orValues) );
    }


    private int regQuery (String term, boolean orValues)
    {
	VOCRegistryQuery query = new VOCRegistryQuery (term, orValues);

	int hcode = query.hashCode(); 	       	// add to object table
	objTab.put ("reg"+hcode, query);

	return (hcode);				// return the hcode
    }


    private int regConstSvcType (int objID, String type)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	if (query != null) {
	    query.setSvcType(type);
	}

	return (0);
    }


    private int regConstWaveband (int objID, String bpass)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	if (query != null) {
	    query.setWaveband(bpass);
	}

	return (0);
    }


    private int regDALOnly (int objID, int value)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	if (query != null) {
	    query.setDALOnly((value == 0) ? false : true);
	}

	return (0);
    }

    private int regSortRes (int objID, int value)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	if (query != null) {
	    query.setSortRes((value == 0) ? false : true);
	}

	return (0);
    }


    private int regAddSearchTerm (int objID, String term, boolean orValue)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	query.addSearchTerm (term, orValue);

	return (0);
    }

    private int regRemoveSearchTerm (int objID, String term)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	query.removeSearchTerm (term);

	return (0);
    }

    private int regGetSTCount (int objID)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	return ( query.getSearchTermCount() );
    }

    private String regGetQueryString (int objID)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);

	return ( query.getQueryString() );
    }

    private int regExecute (int objID)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);
        VOCRegistryQueryResult result =  (VOCRegistryQueryResult) null;
	int hcode = -1;

	try {

            result = query.execute();

	    hcode = result.hashCode(); 	       		// add to object table
	    objTab.put ("reg"+hcode, result);

	} catch (ServiceException ex) {
	    hcode = -1;					// need to add errmsg
	} catch (RemoteException ex) {
	    hcode = -1;
	} catch (Exception ex) {
	    hcode = -1;
	}

	return (hcode);					// return the hcode
    }

    private InputStream regExecuteRaw (int objID)
    {
        VOCRegistryQuery query = (VOCRegistryQuery) objTab.get ("reg"+objID);
        InputStream in = (InputStream) null;

	try {
	    in = query.executeRaw();
	} catch (Exception e) {
	    // return an ERR 
	    vocLOG ("regExecuteRaw: Failed request"); 	
	    return ( (InputStream) null );
	}

	return (in);
    }


    private String resGetStr (int objID, String attr, int index)
    {
        VOCRegistryQueryResult r = 
		(VOCRegistryQueryResult) objTab.get ("reg"+objID);
	String s = r.getString (attr, index);

	return ( (s != null ? s : "") );
    }

    private int resGetInt (int objID, String attr, int index)
    {
        VOCRegistryQueryResult r = 
		(VOCRegistryQueryResult) objTab.get ("reg"+objID);
	return (r.getInt (attr, index));
    }

    private double resGetFloat (int objID, String attr, int index)
    {
        VOCRegistryQueryResult r = 
		(VOCRegistryQueryResult) objTab.get ("reg"+objID);
	return (r.getFloat (attr, index));
    }



    //======================================================================
    //   Utility methods
    //======================================================================
    private int nextIntArg (StringTokenizer st)
    {
	String type = st.nextToken();			// eat the arg type
	if (type.equals ("}")) return (-1);		// end-of-message
	return (Integer.parseInt (st.nextToken()));
    }

    private double nextFloatArg (StringTokenizer st)
    {
	String type = st.nextToken();			// eat the arg type
	if (type.equals ("}")) return (-1.0);		// end-of-message
	return (Double.parseDouble (st.nextToken()));
    }

    private boolean nextBoolArg (StringTokenizer st)
    {
	String type = st.nextToken();			// eat the arg type
	if (type.equals ("}")) return (false);		// end-of-message
	return ((st.nextToken().equals("0") ? false : true));
    }

    private String nextStringArg (StringTokenizer st)
    {
	if (! st.hasMoreTokens())
	    return (null);
	String type = st.nextToken();			// eat the arg type
	if (type.equals ("}")) 
	    return ((String)null);			// end-of-message

	String s = st.nextToken();
	int len = s.length();
	boolean sq  = s.startsWith ("(") || s.startsWith("'(") || 
		s.startsWith("\"(");
	if (len == 1) return ((String)null);		// null string
	if (s.startsWith("\"") || s.startsWith ("'")) {
	    if (s.endsWith("\"") || s.endsWith ("'")) { // single, quoted word
	        return ( s.trim().substring(1, Math.max(0, (s.length()-1))) );
	    }

	    /*  Traverse a quoted string. We only permit 100 tokens 
	     *  in arg to avoid infinite loop.
	     */
	    for (int i=0; i < 100; i++) {		
		s += " " + st.nextToken();
		/* Blechhhh ,,, */
		if (sq && (s.endsWith(")'") || s.endsWith(")\"")))
		    break;
		if (!sq && (s.endsWith("'") || s.endsWith("\"")))
		    break;
	    }
	    return ( s.trim().substring(1, Math.max(0,(s.length()-1))) );
	}

	return (s.trim());				// unquoted token
    }


    private String getObjType (int objID)
    {
	if (objTab.containsKey ("dal"+objID))
	    return ("dal");
	else if (objTab.containsKey ("cone"+objID))
	    return ("cone");
	else if (objTab.containsKey ("siap"+objID))
	    return ("siap");
	else if (objTab.containsKey ("ssap"+objID))
	    return ("ssap");
	else if (objTab.containsKey ("tap"+objID))
	    return ("tap");
	else if (objTab.containsKey ("sesame"+objID))
	    return ("sesame");
	else if (objTab.containsKey ("reg"+objID))
	    return ("reg");
	else if (objTab.containsKey ("skybot"+objID))
	    return ("skybot");
	else
	    return ("Unknown");
    }
}
