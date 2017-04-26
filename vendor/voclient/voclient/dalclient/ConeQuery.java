/*
 * ConeQuery.java
 * $ID*
 */

package dalclient;

import edu.jhu.pha.ivoa.*;
import java.io.*;
import java.net.*;
import java.util.*;


/**
 * Cone Service query.  Provides methods to build up and execute a query
 * against a connection.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public
class ConeQuery extends DALQuery {

    /**
     * Create an initially empty query context.  Subsequent addParameter
     * calls should be used to specify the query parameters.
     *
     * @param  conn	The connection context for this query.
     */
    public ConeQuery(ConeConnection conn) {
	super(conn);
    }

    /**
     * Execute query and process the resultant VOTable into a cone search
     * query response class.  Since for a cone search we deal with general
     * tables which do not have a data model, the query response will use the
     * UCD of each field as the key.
     */
    public QueryResponse execute() throws Exception {
    ArrayList<LinkedHashMap> resultSet = new ArrayList<LinkedHashMap>();
    VOTWrap.VOTable vot;
    try {
	vot = this.executeVOTable();
    } catch (Exception e) {
	throw new Exception("Error executing service request");
    }

    VOTWrap.Resource r = vot.getResource(0);
    VOTWrap.Table table = r.getTable(0);

    // Read the table header; form array of field UCDs.
    ArrayList<String> fieldKeys = new ArrayList<String>();
    ArrayList<VOTWrap.Field> rfields = new ArrayList<VOTWrap.Field>();

    for (int i=0;  i < table.getFieldCount();  i++) {
	String colname;

	// Try to use the UCD as the column heading, otherwise fallback
	// to the Name before giving up.
	VOTWrap.Field field = table.getField(i);
	if ((colname=field.getUCD()) == null || colname.equals("")) {
	    if ((colname=field.getName()) == null || colname.equals("")) {
		colname = "Col" + i;
	    }
	}
	fieldKeys.add (colname.toLowerCase());
	rfields.add (field);
    }

    // Read the table data; form "fields" hashMap for each row.
    VOTWrap.TableData tabledata = table.getTableData();
        int TRCount = ((tabledata != (VOTWrap.TableData) null) ? 
            tabledata.getTRCount() : 0);
	for (int j=0;  j < TRCount;  j++) {
	    LinkedHashMap<String,String> fields = 
		new LinkedHashMap<String,String>();

	    VOTWrap.TR row = table.getTableData().getTR(j);
	    for (int i=0;  i < table.getFieldCount();  i++)
		fields.put(fieldKeys.get(i), row.getTD(i).getPCDATA());

	    resultSet.add(fields);
	}

        // Make a dummy record so we can still query for field info.
        if (TRCount == 0) {
	    LinkedHashMap<String,String> fields = 
		new LinkedHashMap<String,String>();
            for (int i=0;  i < table.getFieldCount();  i++) {
                fields.put(fieldKeys.get(i), null);
            }
            resultSet.add(fields);
        }

	QueryResponse qr = new QueryResponse(resultSet, rfields);

	return (qr);
    }
}
