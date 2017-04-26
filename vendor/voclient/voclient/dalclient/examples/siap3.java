package dalclient;

import java.io.*;


/**
 * Call a Simple Image Access (SIA) service and save the results in a VOTable.
 *
 * Usage:        siap3 ra dec size [fname [serviceURL]]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */
public class siap3 {
    // Default query params if none specified on cmdline (for unit test).
    private static final String DEF_SERVICE = "http://skyview.gsfc.nasa.gov/cgi-bin/vo/sia.pl?";
    private static final double DEF_RA = 12.0;
    private static final double DEF_DEC = 0.0;
    private static final double DEF_SIZE = 0.5;
    private static String FNAME = "siap3.xml";

    public static void main(String[] args) throws Exception {
        String service = DEF_SERVICE;
        String fname = FNAME;
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
                fname = args[arg++];
            }

            if (arg < args.length) {
                service = args[arg++];
            }
        } else {
            System.out.println("Usage: siap3 ra dec size [fname [siapURL]]");
            System.exit(1);
        }

        callSiapService(service, ra, dec, size, fname);
    }

    /* Simple test routine to call a SIA service and save the query response
     * VOTable in a file.
     */
    static void callSiapService(String service, double ra, double dec,
        double size, String fname) throws Exception {
        // Get a new connection to the service.
        SiapConnection siap = new SiapConnection(service);

        // Form the query.
        SiapQuery query = siap.getSiapQuery(ra, dec, size);

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
