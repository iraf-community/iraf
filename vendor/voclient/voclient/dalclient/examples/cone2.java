package dalclient;


/**
 * Call a cone search service and print the results of the query in CSV format.
 *
 * Usage:        cone2 ra dec sr [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */

public class cone2 {

    // Default query params if none specified on cmdline (for unit test).
    private static final String DEF_SERVICE = 
	    "http://gsss.stsci.edu/webservices/vo/ConeSearch.aspx?CAT=GSC23";
    private static final double DEF_RA = 12.0;
    private static final double DEF_DEC = 12.0;
    private static final double DEF_SR = 0.1;


    public static void main(String[] args) throws Exception {

        String service = DEF_SERVICE;
        double ra = DEF_RA, dec = DEF_DEC, sr = DEF_SR;
        int arg = 0;

        if (args.length == 0) {
            // Built-in no-args unit test.

        } else if (args.length >= 3) {
            ra = Double.parseDouble(args[arg++]);
            dec = Double.parseDouble(args[arg++]);
            sr = Double.parseDouble(args[arg++]);

            if (arg < args.length)
                service = args[arg++];

        } else {
            System.out.println("Usage: cone1 ra dec sr [coneURL]");
            System.exit(1);
        }

        callConeService (service, ra, dec, sr);
    }


    /* Simple test routine to call a cone search service and summarize results.
     */
    static void callConeService(String service, double ra, double dec, 
	double sr) throws Exception {


        // Get a new connection to the service and form the query.
        ConeConnection cone = new ConeConnection(service);
        ConeQuery query = cone.getConeQuery(ra, dec, sr);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));
        query.executeCSV();
    }
}
