/*
 * ConeConnection.java
 * $ID*
 */

package dalclient;

import java.util.*;


/**
 * Connection context for a client interface to a Cone search service.
 * Queries require that a connection context be established first.
 * A sequence of queries may be issued for a single connection.  At this
 * point the protocol is stateless and a "connection" is nothing more
 * than a list of service URLs.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public class ConeConnection extends DALConnection {

    /**
     * Create a ConeConnection with an empty service list.
     */
    public ConeConnection() {
	// Nothing yet.
    }

    /**
     * Create a new ConeConnection, initially against a single service.
     *
     * @param  service	The baseURL of the service
     */
    public ConeConnection(String service) {
	super(service);
    }

    /**
     * Create a new empty query context for this connection.  Additional
     * parameters can be added with
     * <code>{@link dalclient.DALQuery#addParameter(String,String)}</code>
     * before executing the query.
     */
    public ConeQuery getConeQuery() {
	ConeQuery query = new ConeQuery(this);
	return (query);
    }

    /**
     * Set up a cone search query context for RA, DEC, SR.  Additional
     * parameters can be later added with
     * <code>{@link #addParameter()}</code> before executing the query.
     *
     * @param  ra	Right ascension in decimal degrees, ICRS
     * @param  dec	Declination in decimal degrees, ICRS
     * @param  sr	Search radius in decimal degrees
     */
    public ConeQuery getConeQuery(double ra, double dec, double sr) {
	ConeQuery query = new ConeQuery(this);
	Double dval;

	dval = new Double(ra);
	query.addParameter("RA", dval.toString());
	dval = new Double(dec);
	query.addParameter("DEC", dval.toString());
	dval = new Double(sr);
	query.addParameter("SR", dval.toString());

	return (query);
    }
}
