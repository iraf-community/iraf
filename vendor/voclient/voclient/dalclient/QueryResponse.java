/*
 * QueryResponse.java
 * $ID*
 */

package dalclient;

import edu.jhu.pha.ivoa.*;		// For VOTWrap
import java.util.*;


/** 
 * Query response class.  Holds a query result set consisting of one query
 * record per data object or table row (corresponding to a VOTable row).
 * Each query record contains a set of keyword=value pairs for each row o
 * the query response.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 *
 * @version	1.1, 12-Feb-2009
 * @author	M. Fitzpatrick
 */
public class QueryResponse {

    /** The query response is an array of dataset or table row descriptors. */
    ArrayList<LinkedHashMap> qr     = new ArrayList<LinkedHashMap>();
    ArrayList<VOTWrap.Field> fields = new ArrayList<VOTWrap.Field>();


    /**
     * LinkedHashMap params = new LinkedHashMap();
     */

    /**
     * Create a query response object for the given resultSet.
     *
     * @param  resultSet	The resultSet containing the query response
     */
    public QueryResponse(ArrayList<LinkedHashMap> resultSet) {
	qr = resultSet;
    }

    /**
     * Create a query response object for the given resultSet, but with
     * auxilliary information about the response table.
     *
     * @param  resultSet	The resultSet containing the query response
     * @param  fieldSet		The low-level FIELD descriptors
     */
    public QueryResponse(ArrayList<LinkedHashMap> resultSet,
	ArrayList<VOTWrap.Field> fieldSet) {

	    qr     = resultSet;
	    fields = fieldSet;
    }


    /** Get the number of records (rows) in the query response resultSet. 
     */
    public int getRecordCount() {
	return (qr.size());
    }

    /**
     * Get the indicated record from the query response resultSet.
     *
     * @param  i	The index of the query record to be returned
     */
    public QueryRecord getRecord(int i) {
	QueryRecord rec = new QueryRecord((LinkedHashMap) qr.get(i));
	return (rec);
    }

    /**
     * Get the indicated field ID from the query response table.
     *
     * @param  i	The index of the query field to be returned
     */
    public String getFieldAttr(int i, String attr)
    {
	if (i < 0 || i >= fields.size()) 
	    return ("");

	VOTWrap.Field f = fields.get(i);

	if (attr.equalsIgnoreCase ("id"))
	    return ( (String) f.getID().trim() );
	if (attr.equalsIgnoreCase ("ucd"))
	    return ( (String) f.getUCD().trim() );
	if (attr.equalsIgnoreCase ("name"))
	    return ( (String) f.getName().trim() );
	if (attr.equalsIgnoreCase ("utype"))
	    return ( (String) f.getUtype().trim() );
	if (attr.equalsIgnoreCase ("datatype"))
	    return ( (String) f.getDataType().trim() );
	if (attr.equalsIgnoreCase ("arraysize"))
	    return ( (String) f.getArraySize().trim() );

	return ("");
    }
}
