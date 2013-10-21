package voclient;

import java.io.*;			// general classes
import java.net.*;
import java.util.*;
import java.text.*;

import java.rmi.RemoteException;	// for the Registry webservice call
import javax.xml.rpc.ServiceException;

import net.ivoa.www.xml.VOTable.v1_1.*;	// for parsing the VOTable result
import v10.riws.net.ivoa.*;


/*
**  VOCRegistryQuery -- Utility class to act as a client interface to
**  the NVO Registry service.
**
**    The high-level RegistrySearch() methods are meant to be used as the
** simplest interface that immediately executes the query and returns a
** QueryResult object.  The 'term' may be either a whitespace-delimited list
** of keywords or an SQL WHERE-clause predicate.  Keyword lists may optionally
** prefaced by the string "keyw" to force a keyword search of those terms,
** otherwise a simple heuristic is used to determine whether a search term
** is a keyword list or an SQL predicate.  Complex queries (e.g. find SIAP
** services described as having "radio galaxy" data) may be composed by
** providing both the keyword list and an SQL predicate.  For example,
**
**   res = RegistrySearch ("radio galaxy", 
**			 "ResourceType like '%siap%'")
**
**     The RegistryQueryResult object is used to provide a convenient interface
** to the results of a query.  
**
**
** Class RegistryQuery
** ----------------------
** 
**  High-Level Query:
** 
**     res = voclient.RegistrySearch (term)
**     res = voclient.RegistrySearch (term, term)
**     res = voclient.RegistrySearch (term, term, orValue)
** 
** 
**  Programmatic Query:
** 
**         query = new RegistryQuery (term) 		// usually SQL
**         query = new RegistryQuery (term, orValues) 	// OR keyword list?
** 
**               query.addSearchTerm (term)		// default AND of terms
**               query.addSearchTerm (term, orValue)	// OR term w/ previous
**            query.removeSearchTerm (term)		// remove search term
**  count = query.getSearchTermCount ()
** 
**        str = query.getQueryString ()			// GET form of query
** 
**               res = query.execute ()			// return result obj
**            str = query.executeRaw ()			// return raw XML
** 
**     For this implementation, we've chose to use the NVO Registry at
** JHU/STScI, specifically the QueryRegistry() method which provides a
** 'VOTResource' form of the resource record.  Support for the newer
** IVOA standard will be added later, for now we can quickly access the most
** commonly used fields of a resource using both a keyword and SQL form of
** the search.
** 
**  M. Fitzpatrick, NOAO, July 2006
**   
**  Revisions:  
**	Modified for Registry 1.0 Interface,	June 2008   
**   
**   
*/


/* VOCRegistryQuery.  Provides methods to build up and execute a query 
 * against a Registry service.
 */

public class VOCRegistryQuery {

    /*  Set the default Registry to use.    
     */
    private static final boolean  DEBUG        = false; 
    //private static final String 
    //        DEF_REGISTRY = "http://nvo.stsci.edu/vor10/NVORegInt.asmx/";
    private static final String 
            DEF_REGISTRY = "http://vao.stsci.edu/directory/NVORegInt.asmx/";
    //private static final String DEF_METHOD   = "VOTPredicate";
    private static final String DEF_METHOD   = "VOTPredOpt";


    /* Constraint strings for the service type and waveband.  We'll need
    ** support from the Registry service providers in order to avoid doing
    ** the filtering entirely on the client side.  (7/13/08)
    */
    private static String  svcType  = null;
    private static String  waveband = null;
    private static boolean DALOnly  = false;
    private static boolean sortRes  = true;


    ArrayList <searchTerm> searchTerms = new ArrayList <searchTerm> ();
    String  queryURL;


    /**
     *   Class Constructor
     */
    VOCRegistryQuery ()
    {
	svcType  = null;
	waveband = null;
	DALOnly  = false;
	sortRes  = true;
    }

    /**
     *   Class Constructor
     *   @param	terms		initial set of search terms
     */
    VOCRegistryQuery (String terms) 
    {
	svcType  = null;
	waveband = null;
	DALOnly  = false;
	sortRes  = true;
	searchTerms.add ( new searchTerm (terms, true) );
    }


    /**
     *   Class Constructor
     *   @param	terms		initial set of search terms
     *   @param	orValues	OR the search terms (presumed keywords)
     */
    VOCRegistryQuery (String terms, boolean orValues) 
    {
	svcType  = null;
	waveband = null;
	DALOnly  = false;
	sortRes  = true;
	searchTerms.add ( new searchTerm (terms, orValues) );
    }


    // ------------------------------------------------------------------

    /**
     *   High level procedure to form a query and execute.
     *   @param	term		initial set of search terms
     *
     */
    public VOCRegistryQueryResult RegistrySearch (String term)
    {
	VOCRegistryQueryResult res = (VOCRegistryQueryResult) null;
        VOCRegistryQuery     query = new VOCRegistryQuery (term, false);

	try {
	    res = query.execute ();

	} catch (ServiceException ex) {
	    System.err.println ("ServiceException in RegistrySearch(): " +
		ex.getMessage());

	} catch (RemoteException ex) {
	    System.err.println ("RemoteException in RegistrySearch(): " +
		ex.getMessage());

	} catch (Exception ex) {
	    System.err.println ("Exception in RegistrySearch(): " +
		ex.getMessage());
	    ex.printStackTrace();
	}

	return (res);
    }


    /**
     *   High level procedure to form a query and execute.
     *   @param	term1		initial set of search terms
     *   @param	term1		initial set of search terms
     *
     */
    public VOCRegistryQueryResult VOCRegistrySearch (String term1, String term2)
    {
	VOCRegistryQueryResult res = (VOCRegistryQueryResult) null;
        VOCRegistryQuery     query = new VOCRegistryQuery (term1, false);

	query.addSearchTerm (term2);

	try {
	    res = query.execute ();

	} catch (ServiceException ex) {
	    System.err.println ("ServiceException in RegistrySearch(): " +
		ex.getMessage());

	} catch (RemoteException ex) {
	    System.err.println ("RemoteException in RegistrySearch(): " +
		ex.getMessage());

	} catch (Exception ex) {
	    System.err.println ("Exception in RegistrySearch(): " +
		ex.getMessage());
	    ex.printStackTrace();
	}

	return (res);
    }


    /**
     *   High level procedure to form a query and execute.
     *   @param	term1		initial set of search terms
     *   @param	term2		initial set of search terms
     *   @param	orValues	OR values in search term2 if keyword list?
     *
     */
    public VOCRegistryQueryResult VOCRegistrySearch (String term1, String term2,
	boolean orValues)
    {
	VOCRegistryQueryResult res = (VOCRegistryQueryResult) null;
        VOCRegistryQuery     query = new VOCRegistryQuery (term1, false);

	query.addSearchTerm (term2, orValues);

	try {
	    res = query.execute ();

	} catch (ServiceException ex) {
	    System.err.println ("ServiceException in RegistrySearch(): " +
		ex.getMessage());

	} catch (RemoteException ex) {
	    System.err.println ("RemoteException in RegistrySearch(): " +
		ex.getMessage());

	} catch (Exception ex) {
	    System.err.println ("Exception in RegistrySearch(): " +
		ex.getMessage());
	    ex.printStackTrace();
	}

	return (res);
    }



    /**
     *   High level procedure to form a query and execute on a specified
     *   service.  This is a convenience procedure that forms a query on the
     *   ResourceType along with the specified term.
     *
     *   @param	svc		initial set of search terms
     *   @param	term		initial set of search terms
     *   @param	orValues	OR values in search term if keyword list?
     *
     */
    public VOCRegistryQueryResult VOCRegSearchByService (String svc, 
	String term, boolean orValues)
    {
        svcType = getSvcType (svc);	/* results are filtered in the query */

	VOCRegistryQueryResult res = VOCRegistrySearch (null, term, orValues);

	return (res);
    }

    /**
     *   High level procedure to form a query and execute on a specified
     *   service.  This is a convenience procedure that forms a query on the
     *   ResourceType along with the specified term.
     *
     *   @param	svc		initial set of search terms
     *   @param	term		initial set of search terms
     *   @param	orValues	OR values in search term if keyword list?
     *
     */
    public VOCRegistryQueryResult VOCRegSearchByWaveband (String wave, 
	String term, boolean orValues)
    {
        waveband = getWaveType (wave);	/* results are filtered in the query */

	VOCRegistryQueryResult res = VOCRegistrySearch (null, term, orValues);

	return (res);
    }

    /**
     *   High level procedure to form a query and execute on a specified
     *   service.  This is a convenience procedure that forms a query on the
     *   ResourceType along with the specified term.
     *
     *   @param	svc		initial set of search terms
     *   @param	term		initial set of search terms
     *   @param	orValues	OR values in search term if keyword list?
     *
     */
    public VOCRegistryQueryResult VOCRegSearchBySvcWave (String svc, 
	String wave, String term, boolean orValues)
    {
        svcType = getSvcType (svc);	/* results are filtered in the query */
        waveband = getWaveType (wave);	/* results are filtered in the query */

	VOCRegistryQueryResult res = VOCRegistrySearch (null, term, orValues);

	return (res);
    }


    public void setSvcType (String svc) {
	this.svcType = getSvcType (svc);
    }

    public void setWaveband (String bpass) {
	this.waveband = bpass;
    }

    public void setDALOnly (boolean value) {
	this.DALOnly = value;
    }

    public void setSortRes (boolean value) {
	this.sortRes = value;
    }


    private static String getSvcType (String svc)
    {
	if (svc.equalsIgnoreCase("catalog") || svc.equalsIgnoreCase("table")) { 
	    return ("conesearch"); 

	} else if (svc.equalsIgnoreCase ("image")) { 
	    return ("SimpleImageAccess"); 

	} else if (svc.equalsIgnoreCase ("spectra")) { 
	    return ("SimpleSpectralAccess"); 

	} else
	    return (null);
    }



    private static String getWaveType (String wave)
    {
	if (wave.equalsIgnoreCase ("radio")) {
	    return ("radio");
	}
	if (wave.equalsIgnoreCase ("millimeter")) {
	    return ("millimeter");
	}
	if (wave.equalsIgnoreCase ("infrared") ||
	    wave.equalsIgnoreCase ("IR")) {
		return ("infrared");
	}
	if (wave.equalsIgnoreCase ("optical")) {
	    return ("optical");
	}
	if (wave.equalsIgnoreCase ("ultraviolet") ||
	    wave.equalsIgnoreCase ("ultra-violet") ||
	    wave.equalsIgnoreCase ("EUV") ||
	    wave.equalsIgnoreCase ("UV")) {
		return ("ultraviolet");
	}
	if (wave.equalsIgnoreCase ("xray") ||
	    wave.equalsIgnoreCase ("x-ray")) {
		return ("x-ray");
	}
	if (wave.equalsIgnoreCase ("gamma") ||
	    wave.equalsIgnoreCase ("gammaray") ||
	    wave.equalsIgnoreCase ("gamma-ray")) {
		return ("gamma-ray");
	}

	return (null);
    }



    // ------------------------------------------------------------------

    /**
     *  Add a query search term.  The term is a simple string that may
     *  represent an SQL predicate or a list of keywords.  We'll parse
     *  it appropriately when we execute the query.
     *
     *  @param	term		search term (keyword list or SQL predicate)
     */
    public void addSearchTerm (String term) 
    {
	searchTerms.add ( new searchTerm (term, false) );
    }

    /**
     *  Add a query search term.  The term is a simple string that may
     *  represent an SQL predicate or a list of keywords.  We'll parse
     *  it appropriately when we execute the query.
     *
     *  @param	term		search term (keyword list or SQL predicate)
     *  @param	orValues	OR the search terms (presumed keywords)
     */
    public void addSearchTerm (String term, boolean orValues) 
    {
	searchTerms.add ( new searchTerm (term, orValues) );
    }


    /**
     *  Remove a query search term.  The term is a simple string that may
     *  represent an SQL predicate or a list of keywords.  We'll parse
     *  it appropriately when we execute the query.
     *
     *  @param	term		search term (keyword list or SQL predicate)
     *  @param	orValues	OR the search terms (presumed keywords)
     */
    public void removeSearchTerm (String term)
    {
	// no-op at the moment
    }


    /**
     *  Count the number of terms we're dealing with.
     *
     *  @returns	the number of search constraints
     */
    public int getSearchTermCount () { return ( searchTerms.size() ); }



    /**
     *   Query the specified registry. 
     *
     *   @param	serviceIndex	Registry service index to query
     */
    public InputStream executeRaw ()
	throws Exception 
    {
        String url = getQueryString ();
        URL http = new URL (queryURL = url);

        return ( http.openStream() );
    }


    /**
     *  Execute query as a QueryRegistry and return a binary input 
     *  stream to read raw results.
     *
     *  @returns	a RegistryQueryResult object
     */
    public VOCRegistryQueryResult execute () 
	throws ServiceException, RemoteException 
    {
	String q="", query = "";
 	VOTResource[] queryResults = null;
	String[] swords = null;
	boolean andKeys = true;
	boolean keywOnly = true;
        String keyword;



	/*  Check the search terms to see whether we have any predicates
	 *  or just keywords.
	 */
	int nwords = 0;
	String qterm = "";
        for (int arg = 0; arg < searchTerms.size(); arg++) {
            searchTerm s = (searchTerm) searchTerms.get (arg);
	    String v = (String) s.term;
	        
	    qterm = qterm + v + " ";

	    if (!isKeywordSearch (s.term)) {
		keywOnly = false;
		break;
	    } else {
                StringTokenizer st = new StringTokenizer(s.term);
		swords = new String[st.countTokens()];
                for (int i = st.countTokens(); i > 0; i--)
                    swords[nwords++] = st.nextToken();
	    }
        }
	regLOG ("Registry query: " + qterm);


	/*  Build up the search terms.
	 */
        for (int arg = 0; arg < searchTerms.size(); arg++) {
            searchTerm s = (searchTerm) searchTerms.get (arg);
	    String v = (String) s.term;

	    if (keywOnly && (svcType == null && waveband == null)) {
		// If we're doing a keyword-only search, allow an 'OR' in
		// the terms.
		if (v.equalsIgnoreCase("OR"))
		    andKeys = false;
		else
		    query += v + " ";

	    } else {
	        query += ((v.trim().startsWith("keyw") || isKeywordSearch(v)) ?
		    s.formKeywSearchTerm () : s.formSQLSearchTerm () );

                if (arg < searchTerms.size()-1) {
	            query += (s.orValue ? " OR " : " AND ");
                }
	    }
        }
	//System.err.println ("\n\nQuery String:\n    " + query + "\n\n");
	

	try {
            /* Get a registry service object and an interface object to
	     * process the query.
	     */
//            v10.riws.net.ivoa.NVORegInt regService = new NVORegIntLocator();
//            NVORegIntSoap regIf = regService.getNVORegIntSoap();

            /* Now submit the query and save the results.
	     */
            VOTABLE results = (VOTABLE) null;

	    if (DEBUG) {
	        System.err.println ("query = :"+query+":");
		System.err.println ("svcType = :"+svcType+":");
		System.err.println ("waveband = :"+waveband+":");
		System.err.println ("dalOnly = :"+DALOnly+":");
	    }

	    if (System.getenv ("VAO_REGTEST") != null) {
              v10.riws.net.ivoa.NVOTestRegInt regService = new NVOTestRegIntLocator();
              NVOTestRegIntSoap regIf = regService.getNVOTestRegIntSoap();

	      System.err.println ("INFO: Using vaotest.stsci.edu Registry....");
	      if (svcType != null && waveband != null) {
	        if (DEBUG)
		    System.err.println ("Calling VOTCapBandPredOpt()....."+
			"svctype="+svcType+"  waveband="+waveband);
            	results = regIf.VOTCapBandPredOpt(query, svcType, waveband, 2);
	      } else if (svcType != null) {
	        if (DEBUG)
		    System.err.println ("Calling VOTCapabilityPredOpt()....."+
			"svctype="+svcType);
            	results = regIf.VOTCapabilityPredOpt(query, svcType, 2);
	      } else if (keywOnly) {
	        if (DEBUG)
		    System.err.println ("Calling VOTKeyOpt().....");
            	results = regIf.VOTKeyOpt(query, andKeys, 2);
	      } else {
	        if (DEBUG)
		    System.err.println ("Calling VOTPredOpt().....");
            	results = regIf.VOTPredOpt(query, 2);
	      }


	    } else {
              v10.riws.net.ivoa.NVORegInt regService = new NVORegIntLocator();
              NVORegIntSoap regIf = regService.getNVORegIntSoap();

	      if (svcType != null && waveband != null) {
	        if (DEBUG)
		    System.err.println ("Calling VOTCapBandPredOpt()....."+
			"svctype="+svcType+"  waveband="+waveband);
            	results = regIf.VOTCapBandPredOpt(query, svcType, waveband, 2);
	      } else if (svcType != null) {
	        if (DEBUG)
		    System.err.println ("Calling VOTCapabilityPredOpt()....."+
			"svctype="+svcType);
            	results = regIf.VOTCapabilityPredOpt(query, svcType, 2);
	      } else if (keywOnly) {
	        if (DEBUG)
		    System.err.println ("Calling VOTKeyOpt().....");
            	results = regIf.VOTKeyOpt(query, andKeys, 2);
	      } else {
	        if (DEBUG)
		    System.err.println ("Calling VOTPredOpt().....");
            	results = regIf.VOTPredOpt(query, 2);
	      }

	    }


	    /* Turn the VOTable into an Array of VOTResource objects.  We
	     * need to do this in order to expand the resources that have
	     * multiple capabilities into individual resource objects.
	     */
 	    queryResults = 
	        new VOTResourceArray (results, svcType, waveband,
		    DALOnly, swords, sortRes).getVOTResource();


	} catch (ServiceException ex) {
	    System.err.println ("Failed to connect to Registry: " +
		ex.getMessage());

	} catch (RemoteException ex) {
	    System.err.println ("Failed to process query: " + ex.getMessage());
	    ex.printStackTrace();

	} catch (Exception ex) {
	    /*
	    System.err.println ("Query Exception: " + ex.getMessage());
	    ex.printStackTrace();
	    */
	    if (DEBUG) {
	        System.err.println ("No Results found (or invalid query)....");
	        System.err.println ("Exception: " + ex.getMessage());
	    }
 	    queryResults = (VOTResource[]) null;
	}

        // Return the results as a new object.
	return ( new VOCRegistryQueryResult (queryResults) );
    }



    /**
     * Generate a URL for the search service from the baseURL and
     * the stored search terms.  In mornal search mode we make use
     * of the WSDL-generated classes, however having access to the
     * GET form of the service gives us access to the raw XML return.
     *
     *   @param	serviceIndex	Registry service index to query
     */
    public String getQueryString () {

        String serviceMethod = "QueryRegistry";
        String url = this.buildBaseURL ();


        // Add the constraint SQL parameters.
	String searchKeys = "";
	String value = "";

        for (int arg = 0; arg < searchTerms.size(); arg++) {
            searchTerm s = (searchTerm) searchTerms.get (arg);

            value = (s.isKeywordSearch() ?
			s.formKeywSearchTerm() : s.formSQLSearchTerm() );
	
	    searchKeys += URLEncoder.encode("("+value+")");
            if (arg < searchTerms.size()-1) {
                if (! s.isKeywordSearch()) {
                    searchKeys += " AND ";
		} else if (s.orValue) {
                    searchKeys += " OR ";
		} else {
                    searchKeys += " AND ";
		}
            }
        }
        url += ("&predicate=" + searchKeys);
        url += ("&VOTStyleOption=2");

        return (url);
    }


    /**
     *  Utility class to make a really simple decision about whether a
     *  given search term is likely either the SQL or keyword flavor.
     *
     *  @param	term	The search term
     */
    public boolean isKeywordSearch (String term)
    {
	boolean value = true;

	if (term.contains ("like") || 
	    term.contains (">") ||
	    term.contains ("<") ||
	    term.contains ("=") ) {
		value = false;
	}
	return (value);
    }


    /*  Build a base URL for a Registry GET service.
     */
    private String buildBaseURL ()
    {
        String url = DEF_REGISTRY;

	String testURL = System.getenv ("VAO_REGURL");
	if (testURL != null) {
	    System.err.println ("DEV: Using test URL: " + testURL);
	    url = testURL;
	}

        // Try to ensure that the service URL is properly terminated.
        char lastch = url.charAt(url.length() - 1);

        if (lastch == '/') 				// add the method name
            url += DEF_METHOD;

        if ((lastch != '?') && (lastch != '&'))
            url += ((url.indexOf('?') > 0) ? '&' : '?');

	return (url);
    }


    /**
     *  Private class to hold a Registry search term.
     */
    private class searchTerm 
    {
	String  term;
	boolean orValue;


	/**
	 *  Constructor.
	 *  @param term		search term string (sql predicate or keyw list)
	 *  @param orValue	indicates values should be OR'd
	 */
	searchTerm (String term, boolean orValue) {
	    this.term    = term;
	    this.orValue = orValue;
	}


	/**
	 *
	 */
    	public String formSQLSearchTerm () 
    	{
	    return ( "(" + (String) this.term + ")" );
    	}


	/**
	 *
	 */
    	public String formKeywSearchTerm ()
    	{
            StringTokenizer st = new StringTokenizer(this.term);
	    String qstring = "";
            String keyword;

            for (int i = st.countTokens(); i > 0; i--) {

		keyword = st.nextToken();		// skip leading "keyw"
		if (keyword.startsWith ("keyw"))
                    keyword = st.nextToken();

	        String qterm = 
		    "(Title like '%" + keyword + "%' OR " +
		    "ShortName like '%" + keyword + "%' OR " +
		    "Identifier like '%" + keyword + "%' OR " +
		    "[content/subject] like '%" + keyword + "%' OR " +
		    "[curation/publisher] like '%" + keyword + "%' OR " +
		    "[content/Description] like '%" + keyword + "%')";

	        qstring += qterm;
	        if (st.hasMoreTokens()) {
		    qstring += (this.orValue ? " OR " : " AND ");
	        }
            }

	    return ( "(" + (String) qstring + ")" );
    	}

        public boolean isKeywordSearch ()
        {
	    boolean value = true;

	    if (this.term.contains ("like") || 
	        this.term.contains (">") ||
	        this.term.contains ("<") ||
	        this.term.contains ("=") ) {
		    value = false;
	    }
	    return (value);
        }
    }


    /**
     *
     */
    private void regLOG (String str) 
    {
        Date d = new Date();
        DateFormat df = DateFormat.getDateTimeInstance();

        //  Just until we do a proper logging interface.
        System.out.println (df.format(d) + ": " + str);
    }
}
