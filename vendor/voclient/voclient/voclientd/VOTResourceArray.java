/**
**   VOTResourceArray.java
**
**   M. Fitzpatrick, NOAO, July 2008
**   
*/

package voclient;

import java.io.*;                       // general classes
import java.net.*;
import java.util.*;

import net.ivoa.www.xml.VOTable.v1_1.*; // for parsing the VOTable result


/**
 *
 */
public class VOTResourceArray {

    private static final boolean DEBUG	= false;
    private static final boolean VDEBUG	= false;

    private VOTResource[] votResource = null;



    public VOTResourceArray (VOTABLE vot, String svcType, String waveband,
	boolean DALOnly, String[] sterms, boolean doSort)
    {
	ArrayList <VOTResource> VRarry = new ArrayList <VOTResource> ();

	Resource[] res = vot.getRESOURCE();
	Table[] tab = res[0].getTABLE();
	TableData tdat = tab[0].getDATA().getTABLEDATA();
	Field[] fields = tab[0].getFIELD();
	int  nfields = fields.length;
	String nam =  null;
	String val =  null;

	ArrayList names = new ArrayList(); 		// Read the header
        for (int i=0;  i < nfields;  i++) {
	    names.add (fields[i].getID());
	}

	Tr[] tr = tdat.getTR();
	int nrows = tr.length;

	if (DEBUG)
	    System.err.println ("nrows="+nrows+"   nfields="+nfields);

        for (int i=0;  i < nrows;  i++) { 		// Read the table data
	    VOTResource vr = new VOTResource (i);

	    Td[] td = tr[i].getTD();
            for (int j=0;  j < nfields;  j++) {

		/* Get the name of column j and the associated value.
		*/
		nam =  (String) fields[j].getID().toString();
		val =  (String) td[j].get_value();

		if (VDEBUG)
		    System.err.println (":"+nam+":=:"+val+":");
		    

		// kludge = FIXME	////////////////////////////
		//		 				  //
		if ( nam.equals("regionOfRegard") )		  //
		    continue;					  //
		if ( nam.equals("maxRadius") )		 	  //
		    continue;		 	 	 	  //
		if ( nam.equals("maxRecords") )		 	  //
		    break;		 	 	 	  //
	        if (j==(nfields-2)&&nam.equals("referenceURL")) { //
		    vr.setByFieldName ( nam, val );		  //
	            break;		 			  //
		}		  				  //
		//		 				  //
		// kludge = FIXME	////////////////////////////

		vr.setByFieldName ( nam, val );
	    }

	    try {
		if (doSort)
	            vr.setRank (sterms);	// Set result ranking.
	    } catch (Exception ex) {
		;
	    }


	    /* Check whether the resource matches the waveband and resource
	    ** type constraints.
	    */
	    if ( waveband != null ) { 
		String[] w = vr.getWaveband();
		if (w != null) {
		    String s = arrayToStr ( w ) ;
		    if ( s != null && s.contains ( waveband ) ) {
			if (DEBUG)
			    System.err.println ("Skipping waveband :"+s+":");
	    	        continue;
		    }
		}
	    }
	    if ( svcType != null) {
		String[] c = vr.getCapabilityStandardID();
		if (c != null && c.length > 0) {
		    String s = arrayToStr (c).toLowerCase();
		    if (s != null && 
			( s.contains(svcType.toLowerCase()) ||
			  svcType.contains(s.toLowerCase()) ||
			  s.equals(svcType.toLowerCase())) ) {
	        	      ;		// added to the list below
		    }
		} else {
		    if (DEBUG)
		        System.err.println ("Skipping svc :"+svcType+":");
		    continue;
		}
	    }

	    /*  If we're asking for DAL resources, expand the resource record
	    **  so there is one entry for each capability/interface. 
	    */ 
	    if ( DALOnly ) {
		VOTResource[] vra = vr.expandResources();

		for (int k = 0; k < vra.length; k++) {
		    String std = arrayToStr(vra[k].getCapabilityStandardID());

		    if (std != null && std.contains("std")) {
	        	VRarry.add (vra[k]);		// add to the list
		    }
		}

	    } else {
	        VRarry.add (vr);			// add to the list
	    }
	}

	if (doSort)
	    Collections.sort(VRarry);			// sort by ranking

	votResource  = new VOTResource[VRarry.size()];	// convert to array
	VRarry.toArray( votResource );
    }


    public VOTResource[] getVOTResource () {
        return votResource;
    }

    public void setVOTResource (VOTResource[] votResource) {
        this.votResource = votResource;
    }

    public VOTResource getVOTResource (int i) {
        return votResource[i];
    }

    public void setVOTResource (int i, VOTResource value) {
        this.votResource[i] = value;
    }



    /** 
     *  Utility to convert a String array to a CSV of the list
     *  @param in       input String array
     *  @return         a CSV String of the array contents
     */
    private String arrayToStr (java.lang.String[] in)
    {
        String  out = "";

        if (in == null || in.length == 0)
	    return (null);
        for (int i=0; i < in.length; i++) {
            out += in[i];
            if (i < (in.length-1))
                out += ",";
        }

        return ( new String (out) );
    }

    private boolean __hashCodeCalc = false;
    public synchronized int hashCode () 
    {
        if (__hashCodeCalc) {
            return 0;
        }

        __hashCodeCalc = true;
        int _hashCode = 1;
        if (getVOTResource() != null) {
            for (int i=0;
                 i<java.lang.reflect.Array.getLength(getVOTResource());
                 i++) {
                    java.lang.Object obj = 
		        java.lang.reflect.Array.get(getVOTResource(), i);

                    if (obj != null && !obj.getClass().isArray()) {
                        _hashCode += obj.hashCode();
                    }
            }
        }
        __hashCodeCalc = false;
        return _hashCode;
    }
}
