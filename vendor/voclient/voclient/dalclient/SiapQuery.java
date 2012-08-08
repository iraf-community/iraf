/*
 * SiapQuery.java
 * $ID*
 */

package dalclient;

import edu.jhu.pha.ivoa.*;
import java.io.*;
import java.net.*;
import java.util.*;


/**
 * SIAP query.  Provides methods to build up and execute a query against
 * a pre-established connection.<p>
 *
 * The SIAP interface elements used in the client API are slightly 
 * abstracted from the names used in the wire protocol (i.e., in the
 * query response VOTable) for simplicity and in order to provide some
 * isolation from the details of the underlying protocol.   The mappings
 * are as follows, with the API keywords at the left:<p>
 * 
 * <pre>
 *     "Title"			"VOX:Image_Title"
 *     "RA"			"POS_EQ_RA_MAIN"
 *     "DEC"			"POS_EQ_DEC_MAIN"
 *     "Instrument"		"INST_ID"
 *     "MJDateObs"		"VOX:Image_MJDateObs"
 *     "Naxes"			"VOX:Image_Naxes"
 *     "Naxis"			"VOX:Image_Naxis"
 *     "Scale"			"VOX:Image_Scale"
 *     "Format"			"VOX:Image_Format"
 * 
 *     "CoordRefFrame"		"VOX:STC_CoordRefFrame"
 *     "CoordEquinox"		"VOX:STC_CoordEquinox"
 *     "CoordProjection"	"VOX:STC_CoordProjection"
 *     "CoordRefPixel"		"VOX:STC_CoordRefPixel"
 *     "CoordRefValue"		"VOX:STC_CoordRefValue"
 *     "CDMatrix"		"VOX:STC_CDMatrix"
 * 
 *     "BandPass_ID"		"VOX:BandPass_ID"
 *     "BandPass_Unit"		"VOX:BandPass_Unit"
 *     "BandPass_RefValue"	"VOX:BandPass_RefValue"
 *     "BandPass_HiLimit"	"VOX:BandPass_HiLimit"
 *     "BandPass_LoLimit"	"VOX:BandPass_LoLimit"
 * 
 *     "PixFlags"		"VOX:Image_PixFlags"
 *     "AccessReference"	"VOX:Image_AccessReference"
 *     "AccessRefTTL"		"VOX:Image_AccessRefTTL"
 *     "Filesize"		"VOX:Image_FileSize"
 * </pre>
 *
 * <p>It is the SIA V1.0 keywords which are shown here.  The data model will
 * change quite a bit in V1.1, however if possible the keywords shown here
 * will still be recognized to provide some backwards compatibility.
 *
 * @version	1.0, 25-Aug-2005
 * @author	Doug Tody
 */
public
class SiapQuery extends DALQuery {

    /**
     *  Save the query response so we can access it from procedures
     *  that don't return it.
     */ 
    public QueryResponse qresp =  (QueryResponse) null;

    /** Create a new SIAP query context for the given connection. */
    public SiapQuery(SiapConnection conn) {
	super(conn);
    }

    /**
     * Execute query and write to the given output stream in CSV format.
     * For images the column labels are the SIA data model attribute names
     * rather than UCDs.
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
     * Execute query and process the resultant VOTable into a SIAP query
     * response class.  To do this we have to extact the elements of the
     * SIAP data model into attributes in SiapQuery.  Note this version does
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
	// Fields with UCDs not defined by SIA are passed through as-is.
	// SIA V1.0 does not use UTYPE so we do nothing with UTYPE here.

	ArrayList<String> fieldKeys = new ArrayList<String>();
        ArrayList<VOTWrap.Field> rfields = new ArrayList<VOTWrap.Field>();

	for (int i=0;  i < table.getFieldCount();  i++) {
	    VOTWrap.Field field = table.getField(i);

	    // Try to map the field to a SIA data model attribute name.
	    String ucd = field.getUCD();
	    String key = SIAV10map.mapUCD(ucd);

	    // Make sure we have some sort of key for the field.
	    if (key == null) {
                if ((key=field.getUCD()) == null || key.equals("")) {
                    if ((key=field.getName()) == null || key.equals("")) {
                        key = "Col" + i;
                    }
                }
            }

	    fieldKeys.add (key.toLowerCase());
	    rfields.add (field);
	}

	// Read the table data; produce a "fields" hashMap for each row.
	VOTWrap.TableData tabledata = table.getTableData();
	int TRCount = ((tabledata != (VOTWrap.TableData) null) ? 
	    tabledata.getTRCount() : 0);
	for (int j=0;  j < TRCount;  j++) {
	    LinkedHashMap fields = new LinkedHashMap();

	    VOTWrap.TR row = table.getTableData().getTR(j);
	    for (int i=0;  i < table.getFieldCount();  i++) {
		fields.put(fieldKeys.get(i), row.getTD(i).getPCDATA());
	    }
	    if (resultSet != null)
	        resultSet.add(fields);
	}

	// Make a dummy record so we can still query for field info.
	if (TRCount == 0) {
	    LinkedHashMap fields = new LinkedHashMap();
	    for (int i=0;  i < table.getFieldCount();  i++) {
		fields.put(fieldKeys.get(i), null);
	    }
	    if (resultSet != null)
	        resultSet.add(fields);
	}

	QueryResponse qr =  new QueryResponse(resultSet, rfields);

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
     * Map SIA V1.0 UCDs to SIA interface attributes.  This is where
     * we map the SIAP-defined UCDs in the rows of the VOTable to the
     * protocol-independent SIA dataset descriptor data model.  This
     * isolates the client code from changes to details of the underlying
     * protocol, including versioning (except for changes to the data model 
     * itself).
     */
    private static class SIAV10map {
	static final LinkedHashMap m = new LinkedHashMap(32);

	static {
	    // SIA dataset attribute	// SIA V1.0 UCD in VOTable
	    enter("Title",		"VOX:Image_Title");
	    enter("RA",			"POS_EQ_RA_MAIN");
	    enter("DEC",		"POS_EQ_DEC_MAIN");
	    enter("Instrument",		"INST_ID");
	    enter("MJDateObs",		"VOX:Image_MJDateObs");
	    enter("Naxes",		"VOX:Image_Naxes");
	    enter("Naxis",		"VOX:Image_Naxis");
	    enter("Scale",		"VOX:Image_Scale");
	    enter("Format",		"VOX:Image_Format");

	    enter("CoordRefFrame",	"VOX:STC_CoordRefFrame");
	    enter("CoordEquinox",	"VOX:STC_CoordEquinox");
	    enter("CoordProjection",	"VOX:STC_CoordProjection");
	    enter("CoordRefPixel",	"VOX:STC_CoordRefPixel");
	    enter("CoordRefValue",	"VOX:STC_CoordRefValue");
	    enter("CDMatrix",		"VOX:STC_CDMatrix");

	    enter("BandPass_ID",	"VOX:BandPass_ID");
	    enter("BandPass_Unit",	"VOX:BandPass_Unit");
	    enter("BandPass_RefValue",	"VOX:BandPass_RefValue");
	    enter("BandPass_HiLimit",	"VOX:BandPass_HiLimit");
	    enter("BandPass_LoLimit",	"VOX:BandPass_LoLimit");

	    enter("PixFlags",		"VOX:Image_PixFlags");
	    enter("AccessReference",	"VOX:Image_AccessReference");
	    enter("AccessRefTTL",	"VOX:Image_AccessRefTTL");
	    enter("Filesize",		"VOX:Image_FileSize");
	}

	// Enter an attribute-UCD pair into the hashmap (makes UCD the key).
	static void enter(String attribute, String ucd) {
	    m.put(ucd.toLowerCase(), attribute);
	}

	// Map a SIA V1.0 UCD to the corresponding data model attribute name.
	static String mapUCD(String ucd) {
	    return ((String) m.get(ucd.toLowerCase()));
	}

	// Get the hashmap.
	static LinkedHashMap getMap() {
	    return (m);
	}
    }
}
