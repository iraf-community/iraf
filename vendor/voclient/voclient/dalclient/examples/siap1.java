package dalclient;


/**
 * Call a Simple Image Access (SIA) service and print selected fields of
 * the query result table.
 *
 * Usage:        siap1 ra dec size [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */
public class siap1 {
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
            System.out.println("Usage: siap1 ra dec size [siapURL]");
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
            System.out.println("# --- Summary output ---");
        }

        // Summarize and print selected query results.
        for (int i = 0; i < qr.getRecordCount(); i++) {
            QueryRecord r = qr.getRecord(i);
            String s_ra;
            String s_dec;
            String s_naxes;
            String s_naxis;
            String s_format;
            String s_title;
            QRAttribute v;

            s_ra = ((v = r.getAttribute("RA")) != null) ? v.stringValue()
                                                        : "<unknown>";
            s_dec = ((v = r.getAttribute("DEC")) != null) ? v.stringValue()
                                                          : "<unknown>";
            s_naxes = ((v = r.getAttribute("Naxes")) != null) ? v.stringValue()
                                                              : "<unknown>";
            s_naxis = ((v = r.getAttribute("Naxis")) != null) ? v.stringValue()
                                                              : "<unknown>";
            s_format = ((v = r.getAttribute("Format")) != null)
                ? v.stringValue() : "<unknown>";
            s_title = ((v = r.getAttribute("Title")) != null) ? v.stringValue()
                                                              : (((v = r.getAttribute(
                        "ID_MAIN")) != null) ? v.stringValue() : "<unknown>");

            System.out.println("ra=" + atmost(s_ra, 10) + "\tdec=" +
                atmost(s_dec, 10) + "\t[" + s_naxis + "]\t" + s_format + "\t" +
                atmost(s_title, 32));
        }
    }

    /* Return at most N chars from string S. */
    private static String atmost(String s, int n) {
        return (s.substring(0, (s.length() < n) ? s.length() : n));
    }
}
