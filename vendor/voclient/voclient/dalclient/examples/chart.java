package dalclient;


/**
 * Call the SDSS finding chart service and download a graphics image
 * of the given coordinates and size.
 *
 * Usage:        chart ra dec size [serviceURL]
 *
 * Or call with no args for the built-in unit test.
 *
 * Original version - D.Tody August 2005
 */
public class chart {
    // Default query params if none specified on cmdline (for unit test).
    private static final String SERVICE = 
	"http://casjobs.sdss.org/vo/DR4SIAP/SIAP.asmx/getSiapInfo?&FORMAT=image/jpeg&BANDPASS=*&";
    private static final double DEF_RA = 258.127;
    private static final double DEF_DEC = 64.017;
    private static final double DEF_SIZE = 0.4;


    public static void main(String[] args) throws Exception {
        double ra = DEF_RA;
        double dec = DEF_DEC;
        double size = DEF_SIZE;
        String service = SERVICE;
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
            System.out.println("Usage: chart ra dec size [URL]");
            System.exit(1);
        }

        callGetChart(service, ra, dec, size);
    }

    /* Simple test routine to download the first image from a cutout service.
     */
    static void callGetChart(String service, double ra, double dec, double size)
        throws Exception {
        // Get a new connection to the service.
        SiapConnection siap = new SiapConnection(service);

        // Form the query.
        SiapQuery query = siap.getSiapQuery(ra, dec, size);

        // Enable the graphics overlay (SDSS specific parameter).
        query.addParameter("opt", "G");

        // Execute the query and fetch results.
        System.out.println("# Query: " + query.getQueryString(0));

        QueryResponse qr = query.execute();

        if (qr.getRecordCount() <= 0) {
            System.out.println("no images found");
            System.exit(1);
        }

        // Download the image.
        QueryRecord r = qr.getRecord(0);
        String path = r.getDataset();

        if (path != null) {
            System.out.println("Downloaded " + path);
        } else {
            System.out.print("Download failed");
        }
    }
}
