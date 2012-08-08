package voclient;

import edu.jhu.pha.ivoa.*;

import java.util.regex.Pattern;
import java.net.*;
import java.io.*;
import java.util.*;
import java.text.*;


/**
 *  VOCSkybot.java
 * 
 *  A utility class for accessing the IMCCE Skybot ephemerides service.  
 *  The service is called as part of the constructor for the object 
 *  allowing easy access to the result data.  Class methods also exist so
 *  a VO Client interface the same access to the data.
 *
 *  Reference:      http://www.imcce.fr/webservices/skybot/
 *
 *  Class Methods:	
 *  --------------
 *
 *	  skybot = VOCSkybot (ra, dec, sr, epoch)	// Constructors
 *	  skybot = VOCSkybot (ra, dec, rsz, dsz, epoch)
 *
 *      count = skybot.NObjs ()				// Num objects found
 *   str = skybot.getStrAttr (attrname, index)		// Get string attr
 *  dval = skybot.getDblAttr (attrname, index)		// Get double attr
 *
 *  Available Attributes:
 *  ---------------------
 *
 *      number  string	    Asteroid number
 *      name    string	    Asteroid name
 *      ra	double	    J2000 Equatorial RA
 *      dec	double	    J2000 Equatorial Dec
 *      class   string	    Object classification
 *      vmag	double	    Visual magnitude
 *      poserr	double	    Error on position (arcsec)
 *      cdist	double      Body-to-center angular distance
 *      dra	double	    RA motion (arcsec/hr)
 *      ddec	double	    Dec motion (arcsec/hr)
 *      dgeo	double	    Geocentric distance (AU)
 *      dhelio	double	    Heliocentric distance (AU)
 *      px	double	    Mean J2000 heliocentric position vector (AU)
 *      py	double		"	"	"	"	"
 *      pz	double		"	"	"	"	"
 *      vx	double	    Mean J2000 heliocentric position vector (AU/day)
 *      vy	double		"	"	"	"	"
 *      vz	double		"	"	"	"	"
 *      JD0	double	    T0, epoch of position vector (JD)
 *
 *
 *  Example Usage:	
 *  ---------------
 *
 *  1) Find bodies in a 900" radius around (0.0,0.0) on JD 2453939.123
 *
 *	VOCSkybot skybot = new VOCSkybot (0.0,0.0,900,2453939.123);
 *      for (int i=0; i < skybot.getNObjs(); i++)
 *	    System.out.print   ("Name: '" + sb.getStrAttr("name",i)+"' " +
 *				"RA: " + sb.getDblAttr("ra",i) + " " +
 *				"Dec: " + sb.getDblAttr("dec",i) + " " +
 *				"Mv: " + sb.getDblAttr("vmag",i) + " "); 
 *
 *  Initial version - M.Fitzpatrick, NOAO, Aug 2006
 */


/**
 * VOCSkybot -- Find minor planets given a position, search radius and epoch.
 */
public class VOCSkybot {
    public HashMap   objects  = new HashMap ();
    public int       nobjs    = 0;


    // The HTTP-GET version of the Skybot asteroid service.
    String baseURL = "http://www.imcce.fr/webservices/skybot/skybot_query.php?";

    /**
     * Invoke the Sesame web-service and parse the result so we can return
     * an object containing the resolved data.
     *
     * @param	    ra		RA position	(J2000)
     * @param	    dec		Dec of position	(J2000)
     * @param	    sr		Search radius 	(arcsec)
     */
    public VOCSkybot (double ra, double dec, double sr, double epoch) {
        this.skybotClient (ra, dec, sr, sr, epoch);
    }

    /**
     * Invoke the SkyBoT web-service and parse the result so we can return
     * an object containing the found object data.
     *
     * @param	    ra		RA position	(J2000)
     * @param	    dec		Dec of position	(J2000)
     * @param	    rsz		RA box size  	(arcsec)
     * @param	    dsz		Dec box size  	(arcsec)
     * @param	    epoch	Epoch of search (JD)
     */
    public VOCSkybot (double ra, double dec, double rsz, double dsz, double ep)
    {
        this.skybotClient (ra, dec, rsz, dsz, ep);
    }

    /**
     *  Return an object attribute as a String.
     *  @return			Number of objects found
     */
    public int NObjs () 	{ return this.nobjs; 	}

    /**
     *  Return an object attribute as a String.
     *
     *  @param	    attr	object attribute name
     *  @param	    index	object index
     *  @return			Attribute value
     */
    public String getStrAttr (String attr, int index)
    { 
	return objects.get(attr+index).toString();
    }

    /**
     *  Return an object attribute as a double.
     *
     *  @param	    attr	object attribute name
     *  @param	    index	object index
     *  @return			Attribute value
     */
    public double getDblAttr (String attr, int index) 	
    { 
	String val =  objects.get(attr+index).toString();
	boolean sex = Pattern.matches (
	    "[+-]*[0-9]*[: ][0-9]*[: ][0-9]*.[0-9]*", val);

	/*  Note we assume sexigesimal numbers include a seconds field.
	 *  What all we return is the decimal equivalent, the caller must
	 *  adjust for ra/dec conversions to decimal degrees
	 */
	if (sex) {
	    StringTokenizer st = new StringTokenizer (val);
	    double d = Double.parseDouble ( st.nextToken() );
	    double m = Double.parseDouble ( st.nextToken() );
	    double s = Double.parseDouble ( st.nextToken() );

	    return ( d + (m / 60.) + (s / 3600.0) );
	} else 
	    return Double.parseDouble ( val );
    }


    /** 
     *  Client procedure to call the SkyBoT web service and parse the result
     *  to object data.
     * 
     *  @param	    ra		RA position	(J2000)
     *  @param	    dec		Dec of position	(J2000)
     *  @param	    rsz		RA box size  	(arcsec)
     *  @param	    dsz		Dec box size  	(arcsec)
     *  @param	    epoch	Epoch of search (JD)
     */
    private void skybotClient (double ra, double dec, double rsz, 
	double dsz, double ep)
    {
	String svcURL 	= baseURL;

	// Build up the URL from the arguments.
	svcURL += "-ra=" + ra + "&";
	svcURL += "-dec=" + dec + "&";
	if (rsz == dsz)
	    svcURL += "-rs=" + rsz + "&";
	else
	    svcURL += "-bs=" + rsz + "x" + dsz + "&";
	svcURL += "-ep=" + ep + "&";
	svcURL += "-out=all&-mime=votable";
	// System.out.println (svcURL);

        try {
	    // Using JAVOT VOTable parser
	    URL http = new URL(svcURL);
            InputStream in = http.openStream();
            VOTWrap.VOTable vot = VOTWrap.createVOTable (in);

	    int resCount = vot.getResourceCount();
	    if (resCount < 1) {
		; // error message needed here
		return;
	    }

	    VOTWrap.Resource res = vot.getResource (0);
            VOTWrap.TableData tab =  res.getTable(0).getTableData();
        
            int nrows = tab.getTRCount();
            for (int r=0; r < nrows; r++) {
		VOTWrap.TR tr = tab.getTR (r);

		objects.put ("number"+r, tr.getTD( 0).getPCDATA());
		objects.put ("name"+r,   tr.getTD( 1).getPCDATA());
		objects.put ("ra"+r,     tr.getTD( 2).getPCDATA());
		objects.put ("dec"+r,    tr.getTD( 3).getPCDATA());
		objects.put ("class"+r,  tr.getTD( 4).getPCDATA());
		objects.put ("vmag"+r,   tr.getTD( 5).getPCDATA());
		objects.put ("poserr"+r, tr.getTD( 6).getPCDATA());
		objects.put ("cdist"+r,  tr.getTD( 7).getPCDATA());
		objects.put ("dra"+r,    tr.getTD( 8).getPCDATA());
		objects.put ("ddec"+r,   tr.getTD( 9).getPCDATA());
		objects.put ("dgeo"+r,   tr.getTD(10).getPCDATA());
		objects.put ("dhelio"+r, tr.getTD(11).getPCDATA());
		objects.put ("px"+r,     tr.getTD(12).getPCDATA());
		objects.put ("py"+r,     tr.getTD(13).getPCDATA());
		objects.put ("pz"+r,     tr.getTD(14).getPCDATA());
		objects.put ("vx"+r,     tr.getTD(15).getPCDATA());
		objects.put ("vy"+r,     tr.getTD(16).getPCDATA());
		objects.put ("vz"+r,     tr.getTD(17).getPCDATA());
		objects.put ("JD0"+r,    tr.getTD(18).getPCDATA());
            }
	    this.nobjs = nrows;

        } catch ( Exception e ) {
	    System.out.println ("SkyBoT client : " + e);
	}
    }


    //  Test main() for the class.
    public static void main(String[] args) {

        if (args.length == 0) {
            //  Require a target name
            System.out.println("Usage:java VOCSkybot <ra> <dec> <sr> <epoch>");
            System.exit(0);

        } else {
	    // Call the service, we resolve the name and parse the result in
	    // the constructor.
            VOCSkybot sb = new VOCSkybot (
		Double.parseDouble(args[0]), 
		Double.parseDouble(args[1]), 
		Double.parseDouble(args[2]), 
		Double.parseDouble(args[3]));

	    int nobjs = sb.NObjs();
	    System.out.println ("#\n# Found " + nobjs + " objects\n#\n");

	    for (int i=0; i < nobjs; i++) {
	        System.out.print   (i+": ");
	        System.out.print   ("Name: '" + sb.getStrAttr("name",i)+"' ");
	        System.out.print   ("RA: " + sb.getDblAttr("ra",i) + " "); 
	        System.out.print   ("Dec: " + sb.getDblAttr("dec",i) + " "); 
	        System.out.println ("Mv: " + sb.getDblAttr("vmag",i) + " "); 
	    }
        }
    }
}
