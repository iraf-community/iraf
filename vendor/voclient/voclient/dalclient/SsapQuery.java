/*
 * SsapQuery.java
 * $ID*
 */

package dalclient;

import edu.jhu.pha.ivoa.*;
import java.io.*;
import java.net.*;
import java.util.*;


/**
 * SSAP query.  Provides methods to build up and execute a query against
 * a pre-established connection.<p>
 *
 * The SSAP interface elements used in the client API are slightly 
 * abstracted from the names used in the wire protocol (i.e., in the
 * query response VOTable) for simplicity and in order to provide some
 * isolation from the details of the underlying protocol.   The mappings
 * are as follows, with the API keywords at the left:<p>
 * 
 * <pre>
 * </pre>
 *
 * <p>It is the SSA V1.0 keywords which are shown here.  The data model will
 * change quite a bit in V1.1, however if possible the keywords shown here
 * will still be recognized to provide some backwards compatibility.
 *
 * @version	1.0, 29-Jan-2009
 * @author	Mike Fitzpatrick
 */
public
class SsapQuery extends DALQuery {

    /**
     *  Save the query response so we can access it from procedures
     *  that don't return it.
     */ 
    public QueryResponse qresp =  (QueryResponse) null;

    /** Create a new SSAP query context for the given connection. */
    public SsapQuery(SsapConnection conn) {
	super(conn);
    }

    /**
     * Execute query and write to the given output stream in CSV format.
     * For images the column labels are the SSA data model attribute names
     * rather than UTypes.
     *
     * @param  out	The output stream to be written to.
     */
    public void executeCSV(PrintStream out) throws Exception {
	QueryResponse resultSet = execute();
	QueryRecord qr = resultSet.getRecord(0);
	LinkedHashMap m = qr.getMap();
	Set s = m.entrySet();
	Iterator i;
	int arg = 0;


	// Output the table header in CSV.
	for (arg=0, i=s.iterator();  i.hasNext();  arg++) {
	    Map.Entry me = (Map.Entry) i.next();
	    String colname = (String) me.getKey();

	    if (arg > 0)
		out.print(",");
	    out.print(colname);
	}
	out.println("");

	// Output the table data.
	for (int j=0;  j < resultSet.getRecordCount();  j++) {
	    qr = resultSet.getRecord(j);
	    m = qr.getMap();
	    s = m.entrySet();

	    for (arg=0, i=s.iterator();  i.hasNext();  arg++) {
		Map.Entry me = (Map.Entry) i.next();
		String sdata = (String) me.getValue();

		if (arg > 0)
		    out.print(",");
		out.print(sdata.replace(',', ' '));
	    }
	    out.println("");
	}

	this.qresp = resultSet;
    }

    /**
     * Execute query and process the resultant VOTable into a SSAP query
     * response class.  To do this we have to extact the elements of the
     * SSAP data model into attributes in SsapQuery.  Note this version does
     * not yet support querying multiple services and merging the results.
     */
    public QueryResponse execute() throws Exception {
	ArrayList<LinkedHashMap> resultSet = new ArrayList<LinkedHashMap>();
	VOTWrap.VOTable vot = this.executeVOTable();

	// The following ought to be a bit more careful about checking for
	// a valid query response, dealing with multiple resources, etc.

	VOTWrap.Resource r = vot.getResource(0);
        VOTWrap.Table table = r.getTable(0);

	// Read the table header; form array of field to keyword mappings.
	// Fields with UTypes not defined by SSA are passed through as-is.
	// SSA V1.0 does not use UTYPE so we do nothing with UTYPE here.

	ArrayList<String> fieldKeys = new ArrayList<String>();
	ArrayList<VOTWrap.Field> rfields = new ArrayList<VOTWrap.Field>();

	for (int i=0;  i < table.getFieldCount();  i++) {
	    VOTWrap.Field field = table.getField(i);

	    // Try to map the field to a SSA data model attribute name.
	    String utype = field.getUtype().trim();
	    String key = SSAV10map.mapUType(utype);

	    // Make sure we have some sort of key for the field.
	    if (key == null) {
                if ((key=field.getUtype()) == null || key.equals("")) {
                    if ((key=field.getName()) == null || key.equals("")) {
                        key = "Col" + i;
                    }
                }
            }

	    fieldKeys.add(key.toLowerCase());
	    rfields.add (field);
	}

	// Read the table data; produce a "fields" hashMap for each row.
	VOTWrap.TableData tabledata = table.getTableData();
	int TRCount = ((tabledata != (VOTWrap.TableData) null) ? 
	    tabledata.getTRCount() : 0);
	for (int j=0;  j < TRCount;  j++) {
	    LinkedHashMap<String,String> fields = 
		new LinkedHashMap<String,String>();

	    VOTWrap.TR row = table.getTableData().getTR(j);
	    for (int i=0;  i < table.getFieldCount();  i++) {
		fields.put(fieldKeys.get(i),
		    row.getTD(i).getPCDATA());
	    }
	    if (resultSet != null)
	        resultSet.add(fields);
	}

	// Make a dummy record so we can still query for field info.
	if (TRCount == 0) {
	    LinkedHashMap<String,String> fields = 
		new LinkedHashMap<String,String>();
	    for (int i=0;  i < table.getFieldCount();  i++) {
		fields.put(fieldKeys.get(i), null);
	    }
	    if (resultSet != null)
	        resultSet.add(fields);
	}

	QueryResponse qr = new QueryResponse(resultSet, rfields);

	this.qresp = qr;
	return (qr);
    }

    /**
     *
     */
    public QueryResponse getQResponse() {
	return ( super.qresp );
    }




    /*
     * Map SSA V1.0 UTypes to SSA interface attributes.  This is where
     * we map the SSAP-defined UTypes in the rows of the VOTable to the
     * protocol-independent SSA dataset descriptor data model.  This
     * isolates the client code from changes to details of the underlying
     * protocol, including versioning (except for changes to the data model 
     * itself).
     */
    private static class SSAV10map {
	static final LinkedHashMap m = new LinkedHashMap(32);

	static {
	    // SSA dataset attribute	// SSA V1.0 UType in VOTable
	    enter("Title",		"ssa:DataID.Title");
	    enter("Instrument",		"ssa:DataID.Instrument");

	    enter("RA",			"");
	    enter("DEC",		"");
	    enter("Pos",                "ssa:Char.SpatialAxis.Coverage.Location.Value");

	    enter("MJDateObs",          "ssa:Char.TimeAxis.Coverage.Location.Value");
	    enter("Naxes",		"");
	    enter("Naxis",		"ssa:Dataset.Length");
	    enter("Scale",		"ssa:Data.SpectralAxis.Resolution");
	    enter("Width",  		"ssa:Char.SpectralAxis.Coverage,Bounds.Extent");
	    enter("Format",		"ssa:Access.Format");
	    enter("DataModel",		"ssa:DataSet.DataModel");

	    enter("CoordRefFrame",	"ssa:CoordSys.SpaceFrame.Name");
	    enter("CoordEquinox",	"ssa:CoordSys.SpaceFrame.Equinox");
	    enter("CoordRefValue",	"ssa:CoordsSys.SpaceFrame.RefPos");
	    enter("CoordProjection",	"");
	    enter("CoordRefPixel",	"");
	    enter("CDMatrix",		"");

	    enter("BandPass_ID",	"ssa:DataID.Bandpass");
	    enter("BandPass_Unit",	"ssa:Char.SpectralAxis.Unit");
	    enter("BandPass_RefValue",  "ssa:Char.SpectralAxis.Coverage.Location.Value");
	    enter("BandPass_HiLimit",   "ssa:Char.SpectralAxis.Coverage.Bounds.Start");
	    enter("BandPass_LoLimit",   "ssa:Char.SpectralAxis.Coverage.Bounds.Stop");
	    enter("BandPass_Extent",    "ssa:Char.SpectralAxis.Coverage.Bounds.Extent");

	    enter("AccessReference",	"ssa:Access.Reference");
	    enter("Filesize",		"ssa:Access.Size");
	}

	// Enter an attribute-UType pair into the hashmap (makes UType the key).
	static void enter(String attribute, String val) {
	    m.put(val.toLowerCase(), attribute.toLowerCase() );
	}

	// Map a SSA V1.0 UType to the corresponding data model attribute name.
	static String mapUType (String val) {
	    return ((String) m.get(val.toLowerCase()));
	}

	// Get the hashmap.
	static LinkedHashMap getMap() {
	    return (m);
	}
    }

}
