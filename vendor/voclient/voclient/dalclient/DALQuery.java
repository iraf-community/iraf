/*
 * DALQuery.java
 * $ID*
 */

package dalclient;

import edu.jhu.pha.ivoa.*;
import java.io.*;
import java.net.*;
import java.util.*;


/**
 * Generic DAL query.  Provides methods to build up and execute a query
 * against a predefined service connection context.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public
class DALQuery {
    /** Hash table containing the query parameters and their values. */
    LinkedHashMap<String,String> params = new LinkedHashMap<String,String>();

    /** The DALConnection context for this query. */
    DALConnection connection;

    /**
     *  Save the query response so we can access it from procedures
     *  that don't return it.
     */
    public QueryResponse qresp =  (QueryResponse) null;


    /** Holds the URL of the most recently executed query. */
    String queryURL;

    /**
     * Create an initially empty query context.
     *
     * @param  conn	The connection context for this query
     */
    public DALQuery(DALConnection conn) {
	connection = conn;
    }

    /**
     * Add a query parameter=value pair to the connection context.
     *
     * @param  name	The parameter name.
     * @param  value	The parameter value.
     */
    public void addParameter(String name, String value) {
	params.put(name, value);
    }

    /**
     * Execute the query and write the result to stdout in CSV format.
     */
    public void executeCSV() throws Exception {
	executeDelimited(System.out, ",");
    }

    /**
     * Execute the query and write the result to the named file in CSV
     * format.
     *
     * @param  fname	The name of the file to be created.
     */
    public void executeCSV(String fname) throws Exception {
	OutputStream ostream = new FileOutputStream(fname);
	executeDelimited(new PrintStream(ostream), ",");
    }

    /**
     * Execute query and write to the given output stream in CSV format.
     * At the level of a generic DAL query we do not support queries to
     * multiple services as the tables returned may differ.
     *
     * @param  out	The output stream to be used
     */
    public void executeCSV(PrintStream out) throws Exception {
	executeDelimited(out, ",");
    }


    /**
     * Execute the query and write the result to stdout in CSV format.
     */
    public void executeTSV() throws Exception {
	executeDelimited(System.out, "\t");
    }

    /**
     * Execute the query and write the result to the named file in CSV
     * format.
     *
     * @param  fname	The name of the file to be created.
     */
    public void executeTSV(String fname) throws Exception {
	OutputStream ostream = new FileOutputStream(fname);
	executeDelimited(new PrintStream(ostream), "\t");
    }

    /**
     * Execute query and write to the given output stream in CSV format.
     * At the level of a generic DAL query we do not support queries to
     * multiple services as the tables returned may differ.
     *
     * @param  out	The output stream to be used
     */
    public void executeTSV(PrintStream out) throws Exception {
	executeDelimited(out, "\t");
    }


    /**
     * Execute the query and write the result to stdout in CSV format.
     */
    public void executeASCII() throws Exception {
	executeDelimited(System.out, "  ");
    }

    /**
     * Execute the query and write the result to the named file in CSV
     * format.
     *
     * @param  fname	The name of the file to be created.
     */
    public void executeASCII(String fname) throws Exception {
	OutputStream ostream = new FileOutputStream(fname);
	executeDelimited(new PrintStream(ostream), "  ");
    }

    /**
     * Execute query and write to the given output stream in CSV format.
     * At the level of a generic DAL query we do not support queries to
     * multiple services as the tables returned may differ.
     *
     * @param  out	The output stream to be used
     */
    public void executeASCII(PrintStream out) throws Exception {
	executeDelimited(out, "  ");
    }


    /**
     *  Utility routine for printing out a VOTable as a delimited table.
     *  We use this to generate CSV, TSV and ASCII output tables.
     */
    public void executeDelimited(PrintStream out, String delim) 
		throws Exception 
    {
	ArrayList<LinkedHashMap> resultSet = new ArrayList<LinkedHashMap>();
	VOTWrap.VOTable vot = (VOTWrap.VOTable) null;

	try {
	    vot = this.executeVOTable();
	} catch (Exception e) {
            throw e;
	}

	VOTWrap.Resource r = vot.getResource(0);
        VOTWrap.Table table = r.getTable(0);

	ArrayList<String> fieldKeys = new ArrayList<String>();
	ArrayList<VOTWrap.Field> rfields = new ArrayList<VOTWrap.Field>();

	// Output the table header.
	out.print("#");
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

	    if (i > 0)
		out.print(delim);
	    out.print(colname);
	}
	out.println("");

	// Output the table data.
	VOTWrap.TableData tdata = table.getTableData();
	int TRCount = ((tdata != (VOTWrap.TableData)null) ? 
	    tdata.getTRCount() : 0);
	for (int j=0;  j < TRCount;  j++) {
	    VOTWrap.TR row = table.getTableData().getTR(j);
            LinkedHashMap<String,String> fields = 
                new LinkedHashMap<String,String>();


	    for (int i=0;  i < table.getFieldCount();  i++) {
		String s = row.getTD(i).getPCDATA();

		if (i > 0)
		    out.print(delim);

		if (s.contains(" ") && delim.contains(" "))
		    out.print('"' + s + '"');
		else if (s.contains(",") && delim.contains(","))
		    out.print(s.replace(',',' '));
		else
		    out.print(s);

		fields.put(fieldKeys.get(i), row.getTD(i).getPCDATA());
	    }
	    out.println("");

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

        qresp = new QueryResponse(resultSet, rfields);
    }


    /**
     *
     */
    public QueryResponse getQResponse() {
        return ( this.qresp );
    }


    /**
     * Execute query and return a VOTable.
     */
    public VOTWrap.VOTable executeVOTable() throws Exception {
	return (executeVOTable(0));
    }

    /**
     * Query the specified service and return a VOTable.
     *
     * @param  serviceIndex	The index of the service to be queried
     */
    public VOTWrap.VOTable executeVOTable(int serviceIndex) throws Exception {
	InputStream is;
	try {
	    is = executeRaw(serviceIndex);
	} catch (Exception e) {
            throw e;
	}
	
	if (is != (InputStream) null)
	    return (VOTWrap.createVOTable(is));
	else
            throw new Exception("Error executing service request");
    }

    /**
     * Execute query and return a binary input stream to read raw results.
     * Note multiple service connections are not supported at this level.
     * By default only the first service specified is queried.
     */
    public InputStream executeRaw() throws Exception {
	return (executeRaw(0));
    }

    /**
     * Query the specified service and return a raw i/o stream to the
     * query results.
     *
     * @param  serviceIndex	The index of the service to be queried
     */
    public InputStream executeRaw(int serviceIndex) throws Exception {
	String url = getQueryString(serviceIndex);
	URL http = new URL(queryURL = url);
	InputStream is = (InputStream) null;

	try {
	    HttpURLConnection.setFollowRedirects(true);
	    HttpURLConnection con = (HttpURLConnection) http.openConnection();
	    con.setConnectTimeout(15000);		// 15 sec connect timout
	    is = con.getInputStream();
	} catch (IOException e) {
            throw e;
	}
	return (is);
    }

    /**
     * Get the current query URL as a string for the given service.
     *
     * @param  serviceIndex	The index of the service to be queried
     */
    public String getQueryString(int serviceIndex) {
	Set s = params.entrySet();
	String url;

	// Get service baseURL.
	ArrayList<String> services = connection.services;
	if (services.size() < serviceIndex)
	    return (null);
	String baseURL = (String) services.get(serviceIndex);
	url = baseURL;

	// Try to ensure that the service URL is properly terminated.
	char lastch = url.charAt(url.length() - 1);
	if (lastch != '?' && lastch != '&')
	    url += (url.indexOf('?') > 0) ? '&' : '?';

	// Add query parameters.
	int arg = 0;
	for (Iterator i = s.iterator();  i.hasNext();  arg++) {
	    Map.Entry me = (Map.Entry) i.next();
	    String name = (String) me.getKey();
	    String value = (String) me.getValue();

	    if (arg > 0)
		url += "&";
	    url += name + "=" + value;
	}

	return (url);
    }
}
