
//  Test program for the VOCSesame class.
//
//  Assuming the NVOSS classpath and working from this directory:
//
//	% javac -classpath ../../lib/voclient.jar:$CLASSPATH resolver.java
//	% java -classpath ../../lib/voclient.jar:$CLASSPATH resolver m51
//

import voclient.*;

public class resolver {

    public static void main (String[] args) {

        //  Require a target name
        if (args.length == 0) {
            System.out.println ("Usage: resolve <target>");
            System.exit (0);
        }

	// Invoke the service in the constructor so we can just use the result.
        VOCSesame sr = new VOCSesame (args[0]);

	// The following two lines should produce the same results.
	System.out.println ( "target = "+ sr.target +
	    "   ra = "+ sr.ra + "  dec = "+ sr.dec + "  (" + sr.pos + ")");
	System.out.println ( "target = "+ sr.getTarget() +
	    "   ra = "+ sr.getRa() + "  dec = "+ sr.getDec() +
	    "  (" + sr.pos + ")" );
    }
}
