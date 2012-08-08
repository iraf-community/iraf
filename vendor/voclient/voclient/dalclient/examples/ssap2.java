package dalclient;


/**
 * Call a Simple Image Access (SSA) service and print the results in CSV format.
 *
 * Usage:        ssap2 ra dec size [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - M. Fitzpatrick  (Based on like siap2.java by D. Tody)
 */

public class ssap2 {

    // Default query params if none specified on cmdline (for unit test).
    private static final double DEF_RA      = 350.25;
    private static final double DEF_DEC     = -16.4;
    private static final double DEF_SIZE    =   0.5;
    private static final String DEF_SERVICE = 
			"http://galex.stsci.edu/gxWS/SSAP/gxSSAP.aspx?";


    public static void main (String[] args) throws Exception {

        double ra = DEF_RA,  dec = DEF_DEC,  size = DEF_SIZE;
        String service = DEF_SERVICE;
        int arg = 0;


        if (args.length == 0) {
            // Built-in no-args unit test.

        } else if (args.length >= 3) {
            ra   = Double.parseDouble (args[arg++]);
            dec  = Double.parseDouble (args[arg++]);
            size = Double.parseDouble (args[arg++]);

            if (arg < args.length) service = args[arg++];

        } else {
            System.out.println("Usage: ssap2 ra dec size [ssapURL]");
            System.exit(1);
        }

        callSsapService(service, ra, dec, size);
    }


    /* Simple test routine to call a SSA service and summarize results.
     */
    static void callSsapService (String service, double ra, double dec,
        double size) throws Exception {

        // Get a new connection to the service, and form the query.
        SsapConnection ssap = new SsapConnection (service);
        SsapQuery query = ssap.getSsapQuery (ra, dec, size);

        // Execute the query and fetch results.
        System.out.println ("# Query: " + query.getQueryString(0));
        query.executeCSV();
    }
}
