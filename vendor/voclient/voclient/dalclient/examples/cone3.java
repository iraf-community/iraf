package dalclient;

import java.io.*;


/**
 * Call a cone search service and return the results as a VOTable.
 *
 * Usage:        cone3 ra dec sr [fname [serviceURL]]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */

public class cone3 {

    // Default query params if none specified on cmdline (for unit test).
    private static final String DEF_SERVICE = 
	    "http://gsss.stsci.edu/webservices/vo/ConeSearch.aspx?CAT=GSC23";
    private static final double DEF_RA = 12.0;
    private static final double DEF_DEC = 12.0;
    private static final double DEF_SR = 0.1;
    private static final String FNAME = "cone3.xml";


    public static void main(String[] args) throws Exception {

        String service = DEF_SERVICE;
        String fname = FNAME;
        double ra = DEF_RA, dec = DEF_DEC, sr = DEF_SR;
        int arg = 0;


        if (args.length == 0) {
            // Built-in no-args unit test.

        } else if (args.length >= 3) {
            ra = Double.parseDouble(args[arg++]);
            dec = Double.parseDouble(args[arg++]);
            sr = Double.parseDouble(args[arg++]);
            fname = (arg < args.length) ? args[arg++] : "cone3.out";

            if (arg < args.length)
                service = args[arg++];

        } else {
            System.out.println("Usage: cone3 ra dec sr [fname [coneURL]]");
            System.exit(1);
        }

        callConeService (service, ra, dec, sr, fname);
    }
 

    /* Simple test routine to call a cone search service and summarize results.
     */
    static void callConeService(String service, double ra, double dec,
        double sr, String fname) throws Exception {


        // Get a new connection to the service and form the query.
        ConeConnection cone = new ConeConnection(service);
        ConeQuery query = cone.getConeQuery(ra, dec, sr);

        // Execute the query.
        System.out.println("# Query: " + query.getQueryString(0));

        InputStream in = query.executeRaw();

        // Copy the query result VOTable to the name output file, overwriting
        // it if the file already exists.
        System.out.println("# copying query result VOTable to file " + fname);

        File file = new File(fname);
        file.delete();

        FileOutputStream out = new FileOutputStream(file);

        byte[] buf = new byte[4096];
        int n;

        while ((n = in.read(buf, 0, buf.length)) > 0)
            out.write(buf, 0, n);
    }
}
