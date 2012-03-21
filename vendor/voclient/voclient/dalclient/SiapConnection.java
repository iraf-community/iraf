/*
 * SiapConnection.java
 * $ID*
 */

package dalclient;

import java.util.*;


/**
 * SIAP search connection.  At this point the protocol is stateless and a
 * "connection" is nothing more than a list of service URLs.  Queries require 
 * a connection context be established first.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public
class SiapConnection extends DALConnection {

    /**
     * Create a new SIAP connection context with an initially empty service
     * endpoint list.
     */
    public SiapConnection() {
	// Nothing yet.
    }

    /**
     * Create a new SIAP connection context initially populated with a
     * single service endpoint.
     */
    public SiapConnection(String service) {
	super(service);
    }

    /** Get a new empty SIAP query context for this connection. */
    public SiapQuery getSiapQuery() {
	SiapQuery query = new SiapQuery(this);
	return (query);
    }

    /**
     * Set up a new SIAP context for the parameters RA, DEC, and SIZE.
     * The region of interest (ROI) defines both the default coverage of
     * the image to be returned, for services which can dynamically generate
     * images (e.g., a cutout service), or a search region which will match
     * any image with overlaps the region.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size (width or height) of the region of interest in
     *			degrees.
     */
    public SiapQuery getSiapQuery(double ra, double dec, double size) {
	SiapQuery query = new SiapQuery(this);
	String sval;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE.
	dval = new Double(size);
	query.addParameter("SIZE", dval.toString());

	return (query);
    }

    /**
     * Set up a new SIAP context for the parameters RA, DEC, SIZE, and FORMAT.
     * The region of interest (ROI) defines both the default coverage of
     * the image to be returned, for services which can dynamically generate
     * images (e.g., a cutout service), or a search region which will match
     * any image with overlaps the region.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size (width or height) of the region of interest in
     *			degrees.
     * @param  format	Limit the query response to the specified image file
     *			format, e.g., "all", "image/fits", "image/jpeg",
     *			"image/gif", "graphics", and so forth.
     */
    public SiapQuery getSiapQuery(double ra, double dec,
	    double size, String format) {

	SiapQuery query = new SiapQuery(this);
	String sval;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE.
	dval = new Double(size);  sval = dval.toString();
	query.addParameter("SIZE", sval);

	// Encode FORMAT.
	query.addParameter("FORMAT", format);

	return (query);
    }

    /**
     * Set up a new SIAP context for the parameters RA, DEC, RA_SIZE, and
     * DEC_SIZE.  The region of interest (ROI) defines both the default
     * coverage of the image to be returned, for services which can
     * dynamically generate images (e.g., a cutout service), or a search
     * region which will match any image with overlaps the region.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  ra_size	Width of the region of interest in degrees.
     * @param  dec_size	Height of the region of interest in degrees.
     */
    public SiapQuery getSiapQuery(double ra, double dec,
	    double ra_size,  double dec_size) {

	SiapQuery query = new SiapQuery(this);
	String sval, s1, s2;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE.  Collapse to one value if two values are equal.
	dval = new Double(ra_size);  s1 = dval.toString();
	dval = new Double(dec_size); s2 = dval.toString();
	if (s1.equals(s2))
	    query.addParameter("SIZE", s1);
	else
	    query.addParameter("SIZE", s1 + "," + s2);

	return (query);
    }

    /**
     * Set up a new SIAP context for the parameters RA, DEC, SIZE, and
     * FORMAT.  The region of interest (ROI) defines both the default
     * coverage of the image to be returned, for services which can
     * dynamically generate images (e.g., a cutout service), or a search
     * region which will match any image with overlaps the region.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  ra_size	Width of the region of interest in degrees.
     * @param  dec_size	Height of the region of interest in degrees.
     * @param  format	Limit the query response to the specified image file
     *			format, e.g., "all", "image/fits", "image/jpeg",
     *			"image/gif", "graphics", and so forth.
     */
    public SiapQuery getSiapQuery(double ra, double dec,
	    double ra_size,  double dec_size, String format) {

	SiapQuery query = new SiapQuery(this);
	String sval, s1, s2;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE.  Collapse to one value if two values are equal.
	dval = new Double(ra_size);  s1 = dval.toString();
	dval = new Double(dec_size); s2 = dval.toString();
	if (s1.equals(s2))
	    query.addParameter("SIZE", s1);
	else
	    query.addParameter("SIZE", s1 + "," + s2);

	// Encode FORMAT.
	query.addParameter("FORMAT", format);

	return (query);
    }
}
