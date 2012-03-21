/*
 * SsapConnection.java
 * $ID*
 */

package dalclient;

import java.util.*;


/**
 *  SSAP search connection.  At this point the protocol is stateless and a
 *  "connection" is nothing more than a list of service URLs.  Queries require 
 *  a connection context be established first.  This version is based on the
 *  SiapConnection class.
 *
 * @version	1.0, 29-Jan-2009
 * @author	Mike Fitzpatrick
 */
public
class SsapConnection extends DALConnection {

    /**
     * Create a new SSAP connection context with an initially empty service
     * endpoint list.
     */
    public SsapConnection() {
	// Nothing yet.
    }

    /**
     * Create a new SSAP connection context initially populated with a
     * single service endpoint.
     */
    public SsapConnection(String service) {
	super(service);
    }

    /** 
     * Get a new empty SSAP query context for this connection. 
     */
    public SsapQuery getSsapQuery() {
	return (new SsapQuery(this));
    }


    /**
     * Set up a new SSAP context for the parameters RA, DEC, and a
     * default SIZE of 0.1 degrees.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     */
    public SsapQuery getSsapQuery(double ra, double dec) {
	SsapQuery query = new SsapQuery(this);
	String sval;
	Double dval;

	// Encode POS and default SIZE.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);
	query.addParameter("SIZE", "0.1");

	return (query);
    }


    /**
     * Set up a new SSAP context for the parameters RA, DEC, and SIZE.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size of the region of interest in degrees.
     *
     */
    public SsapQuery getSsapQuery(double ra, double dec, double size) {
	SsapQuery query = new SsapQuery(this);
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
     * Set up a new SSAP context for the parameters RA, DEC, SIZE and a
     * bandpass string (in range-list format).
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size of the region of interest in degrees.
     * @param  band	Spectral bandpass in range-list format.
     *
     */
    public SsapQuery getSsapQuery(double ra, double dec, 
	double size, String band) 
    {
	SsapQuery query = new SsapQuery(this);
	String sval;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE and BAND.
	dval = new Double(size);
	query.addParameter("SIZE", dval.toString());
	query.addParameter("BAND", band);

	return (query);
    }


    /**
     * Set up a new SSAP context for the parameters RA, DEC, SIZE, BAND
     * and FORMAT string.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size of the region of interest in degrees.
     * @param  band	Spectral bandpass in range-list format.
     * @param  time	Time coverage in range-list format.
     * @param  format	Limit the query response to the specified format.
     *
     */
    public SsapQuery getSsapQuery(double ra, double dec, 
	double size, String band, String time, String format) 
    {
	SsapQuery query = new SsapQuery(this);
	String sval;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE, BAND, TIME and FORMAT.
	dval = new Double(size);
	query.addParameter("SIZE", dval.toString());
	query.addParameter("BAND", band);
	query.addParameter("TIME", time);
	query.addParameter("FORMAT", format);

	return (query);
    }


    /**
     * Set up a new SSAP context for the parameters RA, DEC, SIZE, BAND
     * and TIME.
     *
     * @param  ra	Central right ascension of the region of interest
     *			in degrees (ICRS).
     * @param  dec	Central declination of the region of interest in
     *			degrees (ICRS).
     * @param  size	Size of the region of interest in degrees.
     * @param  band	Spectral bandpass in range-list format.
     * @param  time	Time coverage in range-list format.
     *
     */
    public SsapQuery getSsapQuery(double ra, double dec, 
	double size, String band, String time) 
    {
	SsapQuery query = new SsapQuery(this);
	String sval;
	Double dval;

	// Encode POS.
	dval = new Double(ra);  sval = dval.toString();  sval += ",";
	dval = new Double(dec); sval += dval.toString();
	query.addParameter("POS", sval);

	// Encode SIZE, TIME and BAND.
	dval = new Double(size);
	query.addParameter("SIZE", dval.toString());
	query.addParameter("BAND", band);
	query.addParameter("TIME", time);

	return (query);
    }
}
