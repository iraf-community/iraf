package dalclient;


/**
 * Call a Simple Image Access (SSA) service and download a number of the
 * images referenced in the query result table.
 *
 * Usage:        ssap4 ra dec size [format [maximages [serviceURL]]]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */

public class ssap4 {

    // Default query params if none specified on cmdline (for unit test).
    private static final String SERVICE =
			"http://galex.stsci.edu/gxWS/SSAP/gxSSAP.aspx?";
    private static final double DEF_RA = 350.25;
    private static final double DEF_DEC = -16.4;
    private static final double DEF_SIZE = 0.5;
    private static final String FORMAT = "ALL";
    private static final int MAXIMAGES = 5;

    public static void main(String[] args) throws Exception {

        String service = SERVICE,  format = FORMAT;
        double ra = DEF_RA,  dec = DEF_DEC,  size = DEF_SIZE;
        int maximages = MAXIMAGES;
        int arg = 0;

        if (args.length == 0) {
            // Built-in no-args unit test.

        } else if (args.length >= 3) {
            ra   = Double.parseDouble (args[arg++]);
            dec  = Double.parseDouble (args[arg++]);
            size = Double.parseDouble (args[arg++]);

            if (arg < args.length) format    = args[arg++];
            if (arg < args.length) maximages = Integer.parseInt(args[arg++]);
            if (arg < args.length) service   = args[arg++];

        } else {
            System.out.println(
                "Usage: ssap4 ra dec size [format [maximages [URL]]]");
            System.exit(1);
        }

        callSsapService (service, ra, dec, size, format, maximages);
    }


    /* Simple test routine to call a SSA service and summarize results.
     */
    static void callSsapService (String service, double ra, double dec,
        double size, String format, int maximages) throws Exception {

        // Get a new connection to the service amd form the query.
        SsapConnection ssap = new SsapConnection (service);
        SsapQuery query = ssap.getSsapQuery (ra, dec, size);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));

        QueryResponse qr = query.execute();

        if (qr.getRecordCount() <= 0) {
            System.out.println ("no images found");
            System.exit(1);
        }

        // Summarize query response.
        {
            int nrec = qr.getRecordCount();
            QueryRecord r = qr.getRecord(0);
            int nattr = (r != null) ? r.getAttributeCount() : 0;
            System.out.println ("# returns " + nrec + " records containing " +
                nattr + " attributes each");
        }

        // Download the first "maximages" images.
        System.out.println ("Downloading spectra: ");

        for (int i = 0; (i < qr.getRecordCount()) && (i < maximages); i++) {
            QueryRecord r = qr.getRecord(i);
            QRAttribute v = r.getAttribute("AccessReference");

            if (v == null) {
		System.out.println ("Rec "+i+" -- null reference");
                continue;
	    }

            System.out.println ("Downloading: " + v.stringValue());
            String path = r.getDataset();
            if (path != null)
                System.out.println ("Downloaded " + path);
            else
                System.out.println ("Download failed");
        }
    }
}
