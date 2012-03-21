package voclient;

import java.io.*;
import java.util.*;

import org.us_vo.www.*;


/*
 * VOCRegistryQueryResult -- Utility class to represent the result of a
 * Registry query.   Basically we just wrap the Resource object from the
 * service we're using to provide a convenient set of methods to interface.
 *
 * Class RegistryQueryResult
 * --------------------------
 * 
 *     res = new RegistryQueryResult  (ArrayOfVOTResource[] qResults)
 *     
 *        count = res.getResultCount  ()
 * 
 * 	            str = res.getStr  (attribute, index)
 *               dval = res.getFloat  (attribute, index)
 * 	           ival = res.getInt  (attribute, index)
 * 
 *  M. Fitzpatrick, NOAO, July 2006
 *
 *  Revisions:
 *  	Updated for Registry 1.0 interface,  MJF  Jul 2008
 */

/* VOCRegistryQueryResult -- Result of a Registry query.
 */

public class VOCRegistryQueryResult {

    VOTResource[] qResults = null; 


    /**
     *   Class Constructor
     *   @param	results		query result array
     */
    VOCRegistryQueryResult (VOTResource[] results) 
    {
	qResults = results;
    }


    /**
     *  Count the number of results.
     *  @returns	the number of Resource records
     */
    public int getResultCount () 
    { 
	int count = 0;
	try {
	    count = qResults.length; 
	} catch (NullPointerException ex) {
	    return (0);
	}
	return ( count );
    }


    /*  Some utility methods for accessing results.  
     */


    /**
     *  Get a resource value as a string-valued parameter.
     *  @param attr	Resource attribute we want
     *  @param index	Resource record number in the result array
     *  @returns	the Resource field requested
     */
    public String getString (String attr, int index)
    {
      try {

	if ((attr.compareToIgnoreCase ("Title") == 0) ||
	    (attr.compareToIgnoreCase ("Name") == 0)) {
	        return ( qResults[index].getTitle() );

	} else if (attr.compareToIgnoreCase ("ShortName") == 0) {
	    return ( qResults[index].getShortName() );

	} else if (attr.compareToIgnoreCase ("Description") == 0) {
	    return ( qResults[index].getDescription() );

	} else if ((attr.compareToIgnoreCase ("Creator") == 0) ||	// 0.4
	           (attr.compareToIgnoreCase ("Publisher") == 0)) {
	    return ( qResults[index].getPublisher() );

	} else if (attr.compareToIgnoreCase ("Identifier") == 0) {
	    return ( qResults[index].getIdentifier() );

	} else if ((attr.compareToIgnoreCase ("Date") == 0) ||		// 0.4
	           (attr.compareToIgnoreCase ("Updated") == 0)) {
	    return ( qResults[index].getUpdated().toString() );

	} else if (attr.compareToIgnoreCase ("Version") == 0) {
	    return ( qResults[index].getVersion() );

	} else if (attr.compareToIgnoreCase ("ResourceID") == 0) {
	    return ( qResults[index].getResourceID() );

	} else if (attr.compareToIgnoreCase ("PublisherID") == 0) {
	    return ( qResults[index].getPublisherID() );

	} else if (attr.compareToIgnoreCase ("ReferenceURL") == 0) {
	    return ( qResults[index].getReferenceURL() );


						/* String Array values 	*/

	} else if (attr.compareToIgnoreCase ("Tags") == 0) {
	    return ( arrayToStr(qResults[index].getTags()));

	} else if (attr.compareToIgnoreCase ("Subject") == 0) {
	    return ( arrayToStr(qResults[index].getSubject()));

	} else if ((attr.compareToIgnoreCase ("ServiceURL") == 0) ||    // 0.4
	           (attr.compareToIgnoreCase ("AccessURL") == 0)) {
	    return ( arrayToStr(qResults[index].getAccessURL()));

	} else if ((attr.compareToIgnoreCase ("ResourceType") == 0) ||
		   (attr.compareToIgnoreCase ("ServiceType") == 0) ||
		   (attr.compareToIgnoreCase ("CapabilityClass") == 0)) {
	    return (arrayToStr(qResults[index].getCapabilityClass()) );

	} else if ((attr.compareToIgnoreCase ("CoverageSpectral") == 0) || //0.4
		   (attr.compareToIgnoreCase ("Waveband") == 0)) {
	    return (arrayToStr(qResults[index].getWaveband()));

	} else if (attr.compareToIgnoreCase ("ContentLevel") == 0) {
	    return (arrayToStr(qResults[index].getContentLevel()));

	} else if (attr.compareToIgnoreCase ("CapabilityName") == 0) {
	    return (arrayToStr(qResults[index].getCapabilityName()));

	} else if (attr.compareToIgnoreCase ("CapabilityClass") == 0) {
	    return (arrayToStr(qResults[index].getCapabilityClass()));

	} else if (attr.compareToIgnoreCase ("CapabilityStandardID") == 0) {
	    return (arrayToTypeStr(qResults[index]) );

	} else if (attr.compareToIgnoreCase ("CapabilityValidationLevel") == 0){
	    return (arrayToStr(qResults[index].getCapabilityValidationLevel()));

	} else if (attr.compareToIgnoreCase ("InterfaceClass") == 0) {
	    return (arrayToStr(qResults[index].getInterfaceClass()));

	} else if (attr.compareToIgnoreCase ("InterfaceVersion") == 0) {
	    return (arrayToStr(qResults[index].getInterfaceVersion()));

	} else if (attr.compareToIgnoreCase ("InterfaceRole") == 0) {
	    return (arrayToStr(qResults[index].getInterfaceRole()));

	} else if (attr.compareToIgnoreCase ("SupportedInputParam") == 0) {
	    return (arrayToStr(qResults[index].getSupportedInputParam()));

						/* Integer values	*/
	} else if (attr.compareToIgnoreCase ("RegionOfRegard") == 0) {
	    java.lang.Integer ror = qResults[index].getRegionOfRegard();
	    return ( ror.toString() );


						/* Integer Array values	*/
	} else if (attr.compareToIgnoreCase ("MaxRadius") == 0) {
	    return (arrayToIntString(qResults[index].getMaxRadius()));

	} else if (attr.compareToIgnoreCase ("MaxRecords") == 0) {
	    return (arrayToIntString(qResults[index].getMaxRecords()));



	/* Deprecated 0.4 resource field names.  
         */
	} else if ((attr.compareToIgnoreCase ("Type") == 0) ||
	           (attr.compareToIgnoreCase ("Facility") == 0) ||
	           (attr.compareToIgnoreCase ("Contributor") == 0) ||
	           (attr.compareToIgnoreCase ("CoverageTemporal") == 0) ||
	           (attr.compareToIgnoreCase ("Type") == 0) ||
		   (attr.compareToIgnoreCase ("Instrument") == 0)) {
	    return ( null );

	}

      } catch (Exception ex) {
	return (null);
      }

	return (null);
    }


    /**
     *  Get a real-valued parameter (double-precision)
     *  @param attr	Resource attribute we want
     *  @param index	Resource record number in the result array
     *  @returns	the Resource field requested
     */
    public double getFloat (String attr, int index)
    {
	/*  No float values implemented at this time.
	*/
	return (0.0);
    }


    /**
     *  Get an int-valued parameter
     *  @param attr	Resource attribute we want
     *  @param index	Resource record number in the result array
     *  @returns	the Resource field requested
     */
    public int getInt(String attr, int index)
    {
	if (attr.compareToIgnoreCase ("MaxRecords") == 0) {
	    int v = (int) qResults[index].getMaxRecords()[0];
	    return ( v );

	} else if (attr.compareToIgnoreCase ("MaxRadius") == 0) {
	    int v = (int) qResults[index].getMaxRadius()[0];
	    return ( v );

								// Class data
	} else if (attr.compareToIgnoreCase ("index") == 0) {
	    int v = (int) qResults[index].getIndex();
	    return ( v );
	} else if (attr.compareToIgnoreCase ("rank") == 0) {
	    int v = (int) qResults[index].getRank();
	    return ( v );
	}

	return (0);
    }


    /** 
     *  Utility to convert a String array to a CSV of the list
     *  @param in	input String array
     *  @return 	a CSV String of the array contents
     */
    private String arrayToStr (java.lang.String[] in)
    {
	String  out = "";
	int n = in.length;

        if (in == null || in.length == 0)
	    return (null);

	for (int i=0; i < n; i++) {
	    out += in[i];
	    if (i < (n-1)) 
		out += ",";
	}
 
	return ( new String (out) );
    }


    /** 
     *  Utility to convert a Integer array to a CSV string of the list
     *  @param in	input String array
     *  @return 	a CSV String of the array contents
     */
    private String arrayToIntString (java.lang.Integer[] in)
    {
	String  out = "";
	int n = in.length;

        if (in == null || in.length == 0)
	    return (null);

	for (int i=0; i < n; i++) {
	    out += in[i].toString();
	    if (i < (n-1)) 
		out += ",";
	}
 
	return ( new String (out) );
    }


    /** 
     *  Utility to convert a Integer array to a CSV string of the list
     *  @param in	input String array
     *  @return 	a CSV String of the array contents
     */
    private String arrayToTypeStr (VOTResource vr)
    {
	String[] in = vr.getCapabilityStandardID();
	String  out = "", s = "";
		

	/*  If we get no results for the standardID, use the Type.
	*/
	if (in == null || in.length == 0)
	    return ( new String (vr.getType()[0]) );

	for (int i=0; i < in.length; i++) {

	    if (in[i].contains("Cone"))
		s = "CONE";
	    else if (in[i].contains("SIA"))
		s = "SIAP";
	    else if (in[i].contains("SSA"))
		s = "SSAP";
	    else
		s = vr.getType()[0];

	    out += s;
	    if (i < (in.length - 1)) 
		out += ",";
	}
 
	return ( new String (out) );
    }
}
