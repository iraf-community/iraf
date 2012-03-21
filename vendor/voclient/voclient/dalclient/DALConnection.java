/*
 * DALConnection.java
 * $ID*
 */

package dalclient;

import java.util.*;

/**
 * Generic data access layer (DAL) client connection to a service or
 * set of services.  At this point the protocol is stateless and a
 * "connection" is nothing more than a list of service URLs.  Queries
 * require a connection context be established first.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public
class DALConnection {
    /** Internally we maintain an array of service endpoints. */
    ArrayList<String> services = new ArrayList<String>();

    /**
     * By default we create a connection context with an empty service list.
     * Use addService to add service endpoints to the connection.
     */
    public DALConnection() {
	// Nothing yet.
    }

    /**
     * Create a new connection initially populated with one service endpoint.
     *
     * @param  service	The endpoint (baseURL) of the service
     */
    public DALConnection(String service) {
	services.add(service);
    }

    /**
     * Add a service to an existing connection.
     *
     * @param  service	The endpoint (baseURL) of the service
     */
    public void addService(String service) {
	services.add(service);
    }

    /** Get a count of the number of services in a connection context. */
    public int getServiceCount() {
	return (services.size());
    }

    /**
     * Get an individual service URL.
     *
     * @param  i	The index of the service to be returned.
     */
    public String getServiceURL(int i) {
	return ((String) services.get(i));
    }

    /** Get a new query context for this connection. */
    public DALQuery getDALQuery() {
	return (new DALQuery(this));
    }
}
