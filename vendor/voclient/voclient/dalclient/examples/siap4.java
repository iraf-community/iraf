package dalclient;


/**
 * Call a Simple Image Access (SIA) service and download a number of the
 * images referenced in the query result table.
 *
 * Usage:        siap4 ra dec size [format [maximages [serviceURL]]]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */
public class siap4 {
    // Default query params if none specified on cmdline (for unit test).
    private static final String SERVICE1 = "http://www-gsss.stsci.edu/gscvo/DSS2.jsp";
    private static final String SERVICE2 = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";
    private static final double DEF_RA = 12.0;
    private static final double DEF_DEC = 10.0;
    private static final double DEF_SIZE = 0.2;
    private static final String FORMAT = "ALL";
    private static final int MAXIMAGES = 5;

    public static void main(String[] args) throws Exception {
        String service = SERVICE2;
        String format = FORMAT;
        double ra = DEF_RA;
        double dec = DEF_DEC;
        double size = DEF_SIZE;
        int maximages = MAXIMAGES;
        int arg = 0;

        if (args.length == 0) {
            // Built-in no-args unit test.
        } else if (args.length >= 3) {
            ra = Double.parseDouble(args[arg++]);
            dec = Double.parseDouble(args[arg++]);
            size = Double.parseDouble(args[arg++]);

            if (arg < args.length) {
                format = args[arg++];
            }

            if (arg < args.length) {
                maximages = Integer.parseInt(args[arg++]);
            }

            if (arg < args.length) {
                service = args[arg++];
            }
        } else {
            System.out.println(
                "Usage: siap4 ra dec size [format [maximages [URL]]]");
            System.exit(1);
        }

        callSiapService(service, ra, dec, size, format, maximages);
    }

    /* Simple test routine to call a SIA service and summarize results.
     */
    static void callSiapService(String service, double ra, double dec,
        double size, String format, int maximages) throws Exception {
        // Get a new connection to the service.
        SiapConnection siap = new SiapConnection(service);

        // Form the query.
        SiapQuery query = siap.getSiapQuery(ra, dec, size, size, format);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));

        QueryResponse qr = query.execute();

        if (qr.getRecordCount() <= 0) {
            System.out.println("no images found");
            System.exit(1);
        }
        // Summarize query response.
        {
            int nrec = qr.getRecordCount();
            QueryRecord r = qr.getRecord(0);
            int nattr = (r != null) ? r.getAttributeCount() : 0;
            System.out.println("# returns " + nrec + " records containing " +
                nattr + " attributes each");
        }

        // Download the first "maximages" images.
        System.out.println("Downloading images: ");

        for (int i = 0; (i < qr.getRecordCount()) && (i < maximages); i++) {
            QueryRecord r = qr.getRecord(i);
            QRAttribute v = r.getAttribute("AccessReference");

            if (v == null) {
                continue;
            }

            System.out.println("Downloading: " + v.stringValue());

            String path = r.getDataset();

            if (path != null) {
                System.out.println("Downloaded " + path);
            } else {
                System.out.print("Download failed");
            }
        }
    }
}
