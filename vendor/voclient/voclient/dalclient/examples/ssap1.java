package dalclient;


/**
 * Call a Simple Spectral Access (SSA) service and print selected fields of
 * the query result table.
 *
 * Usage:        ssap1 ra dec size [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 */

public class ssap1 {

    // Default query params if none specified on cmdline (for unit test).
    private static final String DEF_SERVICE = 
			"http://galex.stsci.edu/gxWS/SSAP/gxSSAP.aspx?";
    private static final double DEF_RA   = 350.25;
    private static final double DEF_DEC  = -16.4;
    private static final double DEF_SIZE = 0.5;

    public static void main(String[] args) throws Exception {

        String service = DEF_SERVICE;
        double ra = DEF_RA,  dec = DEF_DEC,  size = DEF_SIZE;
        int arg = 0;

        if (args.length == 0) {
            // Built-in no-args unit test.

        } else if (args.length >= 3) {
            ra   = Double.parseDouble (args[arg++]);
            dec  = Double.parseDouble (args[arg++]);
            size = Double.parseDouble (args[arg++]);

            if (arg < args.length) service = args[arg++];

        } else {
            System.out.println("Usage: ssap1 ra dec size [ssapURL]");
            System.exit(1);
        }

        callSsapService (service, ra, dec, size);
    }


    /* Simple test routine to call an SSA service and summarize results.
     */
    static void callSsapService (String service, double ra, double dec,
        double size) throws Exception {

        // Get a new connection to the service amd form the query.
        SsapConnection ssap = new SsapConnection(service);
        SsapQuery query = ssap.getSsapQuery(ra, dec, size);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));

        QueryResponse qr = query.execute();

        if (qr.getRecordCount() <= 0) {
            System.out.println("no spectra found");
            System.exit(1);
        }

        // Summarize query response.
        {
            int nrec = qr.getRecordCount();
            QueryRecord r = qr.getRecord(0);
            int nattr = (r != null) ? r.getAttributeCount() : 0;
            System.out.println("# returns " + nrec + " records containing " +
                nattr + " attributes each");
            System.out.println("# --- Summary output ---");
        }

        // Summarize and print selected query results.
        for (int i = 0; i < qr.getRecordCount(); i++) {
            QueryRecord r = qr.getRecord(i);
            String s_ra;
            String s_dec;
            String s_naxis;
            String s_title;
            QRAttribute v;

            s_ra = ((v = r.getAttribute("RA")) != null) ? v.stringValue()
                                                        : "<ra>";
            s_dec = ((v = r.getAttribute("DEC")) != null) ? v.stringValue()
                                                          : "<dec>";
            s_naxis = ((v = r.getAttribute("Naxis")) != null) ? v.stringValue()
                                                              : "<naxis>";
            s_title = ((v = r.getAttribute("Title")) != null)
                ? v.stringValue() : "<title>";

            System.out.println(i+"  ra=" + atmost(s_ra, 10) + "    dec=" +
                atmost(s_dec, 10) + "\t[" + s_naxis + "]\t" + 
		atmost(s_title, 32));
        }
    }

    /* Return at most N chars from string S. */
    private static String atmost (String s, int n) {
        return (s.substring (0, (s.length() < n) ? s.length() : n));
    }
}
