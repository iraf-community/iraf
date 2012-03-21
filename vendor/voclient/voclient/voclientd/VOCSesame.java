package voclient;

import org.apache.xerces.parsers.*;
import org.w3c.dom.Attr; 		// classes to walk the XML DOM tree
import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.xml.sax.InputSource;
import java.net.*;
import java.io.*;


/**
 *  VOCSesame.java
 * 
 *  A utility class for accessing the CDS Sesame name resolver service.  
 *  The service is normally called as part of the constructor for the object 
 *  allowing easy access to the result data.  Class methods also exist so
 *  a VO Client interface the same access to the data.
 *
 *  Class Methods:	
 *  --------------
 *
 *		      sr = VOCSesame ()			// Constructors
 *		      sr = VOCSesame (String target)
 *
 *	   xmlStr = sr.sesameClient  (target)		// Internal access
 *		  sr.parseResultXML  (xmlStr)
 *
 *		 str = sr.getTarget  ()			// Data access
 *		str = sr.getService  ()
 *		  str = sr.getOType  ()
 *		  str = sr.getOName  ()
 *		 str = sr.getPosStr  () 
 *	   	    dval = sr.getRa  ()
 *	   	   dval = sr.getDec  ()
 *	   	 dval = sr.getRaERR  ()
 *	        dval = sr.getDecERR  ()
 *
 *  Example Usage:	
 *  ---------------
 *
 *  1) Quickly resolve a target name and use the data:
 *
 *	    VOCSesame sr = new VOCSesame ("m51");
 * 	    System.out.print ("ra = " + sr.ra + "  dec = " + sr.dec):
 *
 *  2) Create the object but call the service to access the raw XML result:
 *
 *	    VOCSesame sr = new VOCSesame ();
 *	    String xmlStr = sr.sesameClient ("m51");
 *	    sr.parseResultXML (xmlStr);
 * 	    System.out.print ("ra = " + sr.ra + "  dec = " + sr.dec):
 *
 *  Initial version - M.Fitzpatrick, NOAO, July 2006
 *  Modified to avoid AXIS/Java 1.5 problems - D.Tody, 15July2006
 */


/**
 * Resolve a target name to coordinates.  Uses the CDS name resolver
 * web service (HTTP GET version) and the Simbad/Ned/VizieR databases.
 * At the moment we rely only on the default Simbad result.
 */
public class VOCSesame {
    public String    target  = null;		// Resolved output state
    public String    service = null;
    public String    otype   = null;
    public String    oname   = null;
    public String    pos     = null;
    public double    ra      = 0.0;
    public double    dec     = 0.0;
    public double    raERR   = 0.0;
    public double    decERR  = 0.0;
    public String[]  alias;

    // The HTTP-GET version of the Sesame name resolver.
    /*  Old URL
    String baseURL = "http://cdsweb.u-strasbg.fr/viz-bin/nph-sesame/-oxp/SNVA?";
    */
    String baseURL = "http://cdsweb.u-strasbg.fr/cgi-bin/nph-sesame/-oxp/SNVA?";

    /**
     * Invoke the Sesame web-service and parse the result so we can return
     * an object containing the resolved data.
     *
     * @param	    obj		Object name to be resolved
     */
    public VOCSesame (String obj) {
        String result = this.sesameClient (obj);
        this.parseResultXML (result);
    }

    /**
     * Empty constructor, we leave it to the caller to invoke the service
     * and optionally parse the result or use the string directly.
     */
    public VOCSesame () {
    }


    public String getTarget () 	{ return this.target; 	}
    public String getService () { return this.service; 	}
    public String getOType () 	{ return this.otype; 	}
    public String getOName () 	{ return this.oname; 	}
    public String getPosStr () 	{ return this.pos; 	}
    public double getRa () 	{ return this.ra; 	}
    public double getDec () 	{ return this.dec; 	}
    public double getRaERR () 	{ return this.raERR; 	}
    public double getDecERR () 	{ return this.decERR; 	}


    /** Client procedure to call the CDS web-service.
     * 
     *  @param	  target	Target name to be resolved.
     *  @returns  		XML description of resolver record
     */
    public String sesameClient (String target) {
	String result = null;

        try {
	    URL http = new URL(baseURL + target);
	    InputStream in = http.openStream();
	    StringBuffer out = new StringBuffer();
	    byte[] b = new byte[4096];

	    for (int n;  (n = in.read(b)) != -1;  )
	      out.append(new String(b, 0, n));

	    result = out.toString();

        } catch (Exception e ) {
	    System.out.println("Sesame client : " + e);
	}

	return (result);
    }


    /** Parse the XML returned by the web service.
     * 
     *  @param	res		result XML string from Sesame query
     */
    public void parseResultXML (String res) {

        try {
    	    DOMParser dparser = new DOMParser ();

            //  Construct an InputSource from the XML String we're given.
	    //  Xerces is usually used to handling file objects so we need
	    //  to hack a bit here ....
            dparser.parse (new InputSource (
			 new ByteArrayInputStream ( res.getBytes() ) ));

            walk ( dparser.getDocument () );

        } catch (Exception e) { System.out.println("Errors " + e); }
    }


    ///////////////////////////////////////////////////////
    //  Private Class Procedures
    ///////////////////////////////////////////////////////

    // Walk the DOM tree and print as you go.
    //
    private void walk (Node node) {
        int type = node.getNodeType();
	String svc = null;

        if (type == Node.ELEMENT_NODE) {
            String name = node.getNodeName ();

	    // For the target element, look for the Resolver name attribute.
	    if (name.equals ("target") ) {
                NamedNodeMap nnm = node.getAttributes();
                if (nnm != null) {

                    for (int i = 0; i < nnm.getLength(); i++) {
                        Attr attr = (Attr) nnm.item(i);
			String attrName = attr.getNodeName();

			if (attrName.equals ("name") ) {
			    svc = attr.getNodeValue ();
			    break;
			}
                    }
                }
	    }

	    // Get the value of the element as the content text.
	    String text = node.getFirstChild().getNodeValue().trim();

	    // Save the content to the object data.
	    if (name.equals ("target") ) {
		target = text;
	    } else if (name.equals ("Resolver") ) {
		service = svc;
	    } else if (name.equals ("otype") ) {
		otype = text;
	    } else if (name.equals ("jpos") ) {
		pos = text;
	    } else if (name.equals ("jradeg") ) {
		ra = Double.parseDouble (text);
	    } else if (name.equals ("jdedeg") ) {
		dec = Double.parseDouble (text);
	    } else if (name.equals ("errRAmas") ) {
		raERR = Double.parseDouble (text);
	    } else if (name.equals ("errDEmas") ) {
		decERR = Double.parseDouble (text);
	    } else if (name.equals ("oname") ) {
		oname = text;
	    } else if (name.equals ("alias") ) {
		; // alias lists not implemented
	    }
        } 

        // Recurse to finish reading the branch.
        for (Node child = node.getFirstChild(); child != null;
            child = child.getNextSibling()) {
		walk(child);
	}

    } //end of walk


    //  Test main() for the class.
    public static void main(String[] args) {

        if (args.length == 0) {
            //  Require a target name
            System.out.println("Usage::java VOCSesame <target_name>");
            System.exit(0);

        } else {
	    // Call the service, we resolve the name and parse the result in
	    // the constructor.
            VOCSesame sr = new VOCSesame (args[0]);


	    // The following two lines should produce the same results. The 
	    // only difference is in using the method to access the object 
	    // data versus referencing it directly.  We implement both as a
	    // convenience for Java clients that can use the object directly,
	    // and through methods that can be called by the VO Client API.

	    System.out.println ( "\nUsing object data:");
	    System.out.println ( "    "+ sr.target +
	        "   ra = "+ sr.ra + 
		"  dec = "+ sr.dec + 
		"  (" + sr.pos + ")");

	    System.out.println ( "\nUsing object methods:");
	    System.out.println ( "    "+ sr.getTarget() +
	        "   ra = "+ sr.getRa() + 
		"  dec = "+ sr.getDec() +
		"  (" + sr.pos + ")" );
        }
    }
}
