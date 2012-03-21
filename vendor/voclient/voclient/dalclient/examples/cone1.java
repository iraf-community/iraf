package dalclient;


/**
 * Call a cone search service and print a summary selected fields of the
 * results.
 *
 * Usage:        cone1 ra dec sr [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */

public class cone1 {

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
            ra  = Double.parseDouble (args[arg++]);
            dec = Double.parseDouble (args[arg++]);
            sr  = Double.parseDouble (args[arg++]);

            if (arg < args.length)
                service = args[arg++];

        } else {
            System.out.println("Usage: cone1 ra dec sr [coneURL]");
            System.exit(1);
        }


        callConeService (service, ra, dec, sr);
    }


    /** Simple test routine to call a cone search service and summarize results.
     */
    static void callConeService(String service, double ra, double dec, 
	double sr) throws Exception {


        // Get a new connection to the service and form the query.
        ConeConnection cone = new ConeConnection(service);
        ConeQuery query = cone.getConeQuery(ra, dec, sr);

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));

        QueryResponse qr = query.execute();

        if (qr.getRecordCount() <= 0) {
            System.out.println("no records matched");
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

        // Summarize and print selected query results.
        System.out.println("# --- Summary output ---");
        for (int i = 0; i < qr.getRecordCount(); i++) {
            QueryRecord r = qr.getRecord(i);
            String s_id;
            String s_ra;
            String s_dec;
            String s_class;
            QRAttribute v;

            s_id = ((v = r.getAttribute("ID_MAIN")) != null) ? v.stringValue()
                                                             : "<none>";
            s_ra = ((v = r.getAttribute("POS_EQ_RA_MAIN")) != null)
                ? v.stringValue() : "<unknown>";
            s_dec = ((v = r.getAttribute("POS_EQ_DEC_MAIN")) != null)
                ? v.stringValue() : "<unknown>";
            s_class = ((v = r.getAttribute("CLASS_OBJECT")) != null)
                ? v.stringValue() : "<unknown>";

            System.out.println("id=" + s_id + "\tra=" + s_ra + "\tdec=" +
                s_dec + "\tclass=" + s_class);
        }
    }
}
