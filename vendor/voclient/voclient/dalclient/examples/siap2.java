package dalclient;


/**
 * Call a Simple Image Access (SIA) service and print the results in CSV format.
 *
 * Usage:        siap2 ra dec size [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */
public class siap2 {
    // Default query params if none specified on cmdline (for unit test).
    private static final String DEF_SERVICE = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";
    private static final double DEF_RA = 12.0;
    private static final double DEF_DEC = 0.0;
    private static final double DEF_SIZE = 0.5;

    public static void main(String[] args) throws Exception {
        String service = DEF_SERVICE;
        double ra = DEF_RA;
        double dec = DEF_DEC;
        double size = DEF_SIZE;
        int arg = 0;

        if (args.length == 0) {
            // Built-in no-args unit test.
        } else if (args.length >= 3) {
            ra = Double.parseDouble(args[arg++]);
            dec = Double.parseDouble(args[arg++]);
            size = Double.parseDouble(args[arg++]);

            if (arg < args.length) {
                service = args[arg++];
            }
        } else {
            System.out.println("Usage: siap2 ra dec size [siapURL]");
            System.exit(1);
        }

        callSiapService(service, ra, dec, size);
    }

    /* Simple test routine to call a SIA service and summarize results.
     */
    static void callSiapService(String service, double ra, double dec,
        double size) throws Exception {
        // Get a new connection to the service.
        SiapConnection siap = new SiapConnection(service);

        // Form the query.
        SiapQuery query = siap.getSiapQuery(ra, dec, size);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));
        query.executeCSV();
    }
}
