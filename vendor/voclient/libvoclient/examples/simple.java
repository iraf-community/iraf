
public class simple {

   public static void main (String[] argv) {

     //  Load the VOClient shared library.
     try {
        System.loadLibrary ("voclient");

     } catch (UnsatisfiedLinkError Err) {
	System.err.println ("Load Error " + Err);
	System.exit (1);
     }

     //  Call a simple Cone service.
     System.out.println (
	voclient.voc_coneCaller (
	    "http://www.nofs.navy.mil/cgi-bin/vo_cone.cgi?CAT=USNO-B1&",
	    0.0, 0.0, 0.05, 1)
     );

   }
}
