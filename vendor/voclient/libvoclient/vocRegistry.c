/**
 *  VOC_REGISTRYQUERY -- Utility code to act as a client interface to
 *  the NVO Registry service.
 *
 *  RegistryQuery
 *  ----------------------
 * 
 *  High-Level Query:
 * 
 *           res = voc_regSearch (sql, keywords, orValues)
 *   res =voc_regSearchByService (svc, term, orValues)
 * 
 *  Programmatic Query:
 * 
 *          query = voc_regQuery (term, orValues) 	// OR keyword list?
 * 
 *           voc_regConstSvcType (query, svcType)	// search constraints
 *          voc_regConstWaveband (query, waveband)
 *                voc_regDALOnly (query, value)
 *                voc_regSortRes (query, value)
 * 
 *          voc_regAddSearchTerm (query, term, orValue)	// OR term w/ previous
 *       voc_regRemoveSearchTerm (query, term)		// remove search term
 *     count = voc_regGetSTCount (query)
 * 
 *   str = voc_regGetQueryString (query)		// GET form of query
 * 
 *          res = voc_regExecute (query)		// return result obj
 *       str = voc_regExecuteRaw (query)		// return raw XML
 * 
 *  RegistryQueryResult
 * 
 *     count = voc_resGetCount  (res)
 * 
 *         str = voc_resGetStr  (res, attribute, index)
 *      dval = voc_resGetFloat  (res, attribute, index)
 *        ival = voc_resGetInt  (res, attribute, index)
 * 
 *     For this implementation, we've chose to use the NVO Registry at
 *  JHU/STScI, specifically the QueryRegistry() method which provides a
 *  'SimpleResource' form of the resource record.  Support for the newer
 *  IVOA standard will be added later, for now we can quickly access the most
 *  commonly used fields of a resource using both a keyword and SQL form of
 *  the search.
 * 
 *
 *  @file       vocRegistry.c
 *  @author     Michael Fitzpatrick
 *  @version    July 2006
 *
 *************************************************************************
 */


#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


extern VOClient *vo;                    /* Interface runtime struct     */




/**
 *  VOC_REGSEARCH --  High-level procedure to form a query and execute it
 *  immediately.  We allow that 'term1' may be a complex SQL WHERE predicate,
 *  and that 'term2' (or vice versa) is a search-keyword list.  The
 *  'orValues' applies to the keyword list (if present), otherwise it applies
 *  to the two search term elements.  The default action if two terms are
 *  specified is to logically AND them.
 *
 *  The thinking here is that one might want SIAP services for Quasars.  This
 *  is easily expressed in an SQL form to get SIAP resources, however a
 *  Quasar may be known as a QSO, AGN, active-nuclei, etc and so we need a 
 *  easy way to OR the keywords but AND that result with the SQL predicate.
 *
 *  @brief  High-level Registry query interface
 *  @fn	    res = voc_regSearch (char *term1, char *term2, int orValues)
 *
 *  @param  term1	first search term
 *  @param  term2	second search term
 *  @param  orValues	logically OR values?
 *  @returns		handle to Registry result object
 */
RegResult 
voc_regSearch (char *term1, char *term2, int orValues)
{
    RegResult res = (RegResult) VOC_NULL;
    vocRes_t *result = (vocRes_t *) NULL;
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (0, "regSearch", 0);

    if (vo == (VOClient *) NULL) {
	fprintf(stderr,"FATAL: VOClient not initialized in regSearch()\n");
	exit (1);
    }


    if (term1 && term1[0]) {
        msg_addStringParam (msg, term1);
    }
    if (term2 && term2[0]) {
        msg_addStringParam (msg, term2);
        msg_addIntParam (msg, orValues);
    }

    /* Send message and read result.
     */
    if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: regSearch() failed\n");
    } else
        res = msg_getIntResult (result, 0);

    free ((void *)msg);     	/* free the pointers            */
    free ((void *)result);

    return ((RegResult) res);
}


/**
 *  VOC_REGSEARCHBYSERVICE -- Search the Registry using a search term and
 *  constrain by service type. 
 *
 *  @brief  Search Registry using a search term and service constraint
 *  @fn	    res = voc_regSearchByService (char *svc, char *term, int orValues)
 *
 *  @param  svc 	service type constraint
 *  @param  term	keyword search term
 *  @param  orValues	logically OR values?
 *  @returns		handle to Registry result object
 */
RegResult 
voc_regSearchByService (char *svc,  char *term, int orValues)
{
    RegResult res = (RegResult) VOC_NULL;
    vocRes_t *result = (vocRes_t *) NULL;
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (0, "regSearchBySvc", 0);


    if (vo == (VOClient *) NULL) {
	fprintf(stderr,"FATAL: VOClient not initialized in regSearchBySvc()\n");
	exit (1);
    }

    msg_addStringParam (msg, svc);
    msg_addStringParam (msg, term);
    msg_addIntParam (msg, orValues);

    /* Send message and read result.
     */
    if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: regSearchBySvc() failed\n");
    } else
        res = msg_getIntResult (result, 0);

    free ((void *)msg);     	/* free the pointers            */
    free ((void *)result);

    return ((RegResult) res);
}


/**
 *  VOC_REGQUERY --  Create a RegistryQuery object.
 *
 *  @brief  Create a RegistryQuery object.
 *  @fn	    v = voc_regQuery (char *term, int orValues)
 *
 *  @param  term	keyword search term
 *  @param  orValues	logically OR values?
 *  @returns		handle to Registry Query object
 */
RegQuery  
voc_regQuery (char *term, int orValues)
{
    RegQuery query = (RegQuery) VOC_NULL;

    if (vo == (VOClient *) NULL) {
	fprintf(stderr,"FATAL: VOClient not initialized in regQuery()\n");
	exit (1);
    }

    if (term) { 
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (0, "regQuery", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, term);
	msg_addIntParam (msg, orValues);

    	/* Send message and read result.
    	 */
    	if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: regQuery() failed\n");
    	} else
            query = msg_getIntResult (result, 0);

    	free ((void *)msg);         	/* free the pointers            */
    	free ((void *)result);
    } else if (!vo->quiet) 
	fprintf (stderr, "ERROR: empty search term\n");

    return (query);
}


/**
 *  VOC_REGADDSEARCHTERM -- Add a search term (sql predicate or keyword list)
 *  to the specified query.
 *
 *  @brief  Add a search term to the specified query
 *  @fn	    voc_regAddSearchTerm (RegQuery query, char *term, int orValue)
 *
 *  @param  query	Registry query handle
 *  @param  term	keyword search term
 *  @param  orValues	logically OR values?
 *  @returns		nothing
 */
void
voc_regAddSearchTerm (RegQuery query, char *term, int orValue)
{
    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, 
	    "regAddSearchTerm", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, term);
	msg_addIntParam (msg, orValue);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: regAddSearchTerm() failed\n");
	}

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to regAddSearchTerm\n");
}


/**
 *  VOC_REMOVESEARCHTERM -- Remove the search term from the query.
 *
 *  @brief  Remove a search term to the specified query
 *  @fn	    voc_regRemoveSearchTerm (RegQuery query, char *term)
 *
 *  @param  query	Registry query handle
 *  @param  term	keyword search term
 *  @returns		nothing
 */
void
voc_regRemoveSearchTerm (RegQuery query, char *term)
{
    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, 
	    "regRemoveSearchTerm", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, term);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: regRemoveSearchTerm() failed\n");
	}

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to regRemoveSearchTerm\n");
}


/**
 *  VOC_REGCONSTWAVEBAND -- Constrain the Registry search by waveband.
 *
 *  @brief  Constrain the Registry search by waveband.
 *  @fn	    voc_regConstWaveband (RegQuery query, char *waveband)
 *
 *  @param  query	Registry query handle
 *  @param  waveband	waveband string
 *  @returns		nothing
 */
void
voc_regConstWaveband (RegQuery query, char *waveband)
{
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regConstWaveband", 0);
    vocRes_t *result = (vocRes_t *) NULL;

    msg_addStringParam (msg, waveband);

    /* Send message and read result.
     */
    if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
        if (!vo->quiet)
    	    fprintf (stderr, "ERROR: regConstWaveband() failed\n");
    }

    free ((void *)msg);     		/* free the pointers            */
    free ((void *)result);
}


/**
 *   VOC_REGCONSTSVCTYPE -- Constraing the Registry search by service type.
 *
 *  @brief  Constrain the Registry search by service type.
 *  @fn	    voc_regConstWaveband (RegQuery query, char *svcType)
 *
 *  @param  query	Registry query handle
 *  @param  svcType	service type string
 *  @returns		nothing
 */
void
voc_regConstSvcType (RegQuery query, char *svcType)
{
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regConstSvcType", 0);
    vocRes_t *result = (vocRes_t *) NULL;

    msg_addStringParam (msg, svcType);

    /* Send message and read result.
     */
    if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: regConstSvcType() failed\n");
    }

    free ((void *)msg);     	/* free the pointers            */
    free ((void *)result);
}


/**
 *  VOC_REGDALONLY -- Set the "DAL Only" flag.  If set, we expand a resource
 *  search to break out the individual DAL services into separate results.
 *
 *  @brief  Set the "DAL Only" flag
 *  @fn	    voc_regDALOnly (RegQuery query, int value)
 *
 *  @param  query	Registry query handle
 *  @param  value	value of the DAL-only flag
 *  @returns		nothing
 */
void
voc_regDALOnly (RegQuery query, int value)
{
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regDALOnly", 0);
    vocRes_t *result = (vocRes_t *) NULL;

    msg_addIntParam (msg, value);

    /* Send message and read result.
     */
    if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: regDALOnly() failed\n");
    }

    free ((void *)msg);     	/* free the pointers            */
    free ((void *)result);
}


/**
 *  VOC_REGSORTRES -- Set the resource "sort" flag.   If enabled, we try to
 *  order the resource table by some logical means.
 *
 *  @brief  Set the resource "sort" flag
 *  @fn	    voc_regSortRes (RegQuery query, int value)
 *
 *  @param  query	Registry query handle
 *  @param  value	value of the sort flag
 *  @returns		nothing
 */
void
voc_regSortRes (RegQuery query, int value)
{
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regSortRes", 0);
    vocRes_t *result = (vocRes_t *) NULL;

    msg_addIntParam (msg, value);

    /* Send message and read result.
     */
    if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: regSortRes() failed\n");
    }

    free ((void *)msg);     	/* free the pointers            */
    free ((void *)result);
}


/**
 *  VOC_REGGETSTCOUNT -- Get the number of search terms in the current query.
 *
 *  @brief  Get the number of search terms in the current query.
 *  @fn	    count = voc_regGetSTCount (RegQuery query)
 *
 *  @param  query	Registry query handle
 *  @returns		nothing
 */
int
voc_regGetSTCount (RegQuery query)
{
    int count = 0;

    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regGetSTCount", 0);
        vocRes_t *result = (vocRes_t *) NULL;

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: regGetSTCount() failed\n");
        } else
            count = msg_getIntResult (result, 0);

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to regGetSTCount\n");

    return (count);
}


/**
 *  VOC_REGGETQUERYSTRING -- Get the current query as an http GET URL.
 *
 *  @brief  Get the current query as an http GET URL.
 *  @fn	    url = voc_regGetQueryString (RegQuery query)
 *
 *  @param  query	Registry query handle
 *  @returns		query URL
 */
char *
voc_regGetQueryString (RegQuery query)
{
    char     *val = NULL, *qstring = NULL;
    int       len = 0;

    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, 
	    "regGetQueryString", 0);
        vocRes_t *result = (vocRes_t *) NULL;

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resGetQueryStr() failed\n");
        } else {
            val = msg_getStringResult (result, 0);
            qstring = calloc (1, (len = strlen(val)+1));
            strncpy (qstring, val, len);
        }

        free ((void *) msg);     	/* free the pointers            */
        free ((void *) result);
        free ((void *) val);
    }

    return (qstring);
}


/**
 *  VOC_REGEXECUTE -- Execute the specified query, returning a result object
 *  code or NULL.
 *
 *  @brief  Execute the specified query
 *  @fn	    res = voc_regExecute (RegQuery query)
 *
 *  @param  query	Registry query handle
 *  @returns		registry result object handle
 */
RegResult
voc_regExecute (RegQuery query)
{
    RegResult res = (RegResult) VOC_NULL;

    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "regExecute", 0);
        vocRes_t *result = (vocRes_t *) NULL;

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: regExecute() failed\n");
        } else
            res = msg_getIntResult (result, 0);

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    }

    return ((RegResult) res);
}


/**
 *  VOC_REGEXECUTERAW -- Execute the specified query and return the raw
 *  resulting XML string.
 *
 *  @brief  Execute the specified query and return raw result string
 *  @fn	    str = voc_regExecuteRaw (RegQuery query)
 *
 *  @param  query	Registry query handle
 *  @returns		raw data return from data
 */
char *
voc_regExecuteRaw (RegQuery query)
{
    char     *val = NULL, *raw = NULL;
    int       len = 0;

    if (query > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (query, "resExecuteRaw", 0);
        vocRes_t *result = (vocRes_t *) NULL;

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resExecuteRaw() failed\n");
        } else {
            val = msg_getStringResult (result, 0);
            raw = calloc (1, (len = strlen(val)+1));
            strncpy (raw, val, len);
        }

        free ((void *) msg);     	/* free the pointers            */
        free ((void *) result);
        free ((void *) val);
    }

    return (raw);
}


/*****************************************************************************/
/*****		      RegistryQueryResult Methods  			******/
/*****************************************************************************/


/**
 *  VOC_RESGETCOUNT -- Return a count of the number of results records.
 *
 *  @brief  Return a count of the number of results records.
 *  @fn	    count = voc_resGetCount (RegResult res)
 *
 *  @param  res 	Registry result handle
 *  @returns		number of result records
 */
int
voc_resGetCount (RegResult res)
{
    int count = 0;

    if (res > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (res, "resGetCount", 0);
        vocRes_t *result = (vocRes_t *) NULL;

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resGetCount() failed\n");
        } else
            count = msg_getIntResult (result, 0);

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to resGetCount\n");

    return (count);
}


/**
 *  VOC_GETSTR -- Get a string-valued attribute from the result resource
 *  record.  Currently recognized real-valued attributes include:
 *
 *	Title			Resource title (long version)
 *	ShortName		Short name of Resource
 *	ServiceURL		Service URL (if appropriate)
 *	ReferenceURL		URL to reference about Resource
 *	Description		Text description of resource
 *	Identifier		Standard ivo identifier of resource
 *	ServiceType		Service Type (Cone, Siap, etc)
 *	Type			Resource Type (catalog, survey, etc)
 *	CoverageSpatial		Spatial coverage (STC)
 *	CoverageTemporal	Temporal coverage of data

 *	CoverageSpectral	Spectral coverage (csv list of bandpasses)
 *	ContentLevel		Content level (research, EPO, etc -- csv list)
 *
 *  Attribute strings are case-insensitive.
 *
 *
 *  @brief  Get a string-valued attribute from the result resource record
 *  @fn	    str = voc_resGetStr (RegResult res, char *attr, int index)
 *
 *  @param  res 	Registry result handle
 *  @param  attr 	record attribute
 *  @param  index 	record index
 *  @returns		string-valued attribute
 */
char *
voc_resGetStr (RegResult res, char *attribute, int index)
{
    char     *val = NULL, *str = NULL;
    int       len = 0;


    if (res > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (res, "resGetString", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, attribute);
	msg_addIntParam (msg, index);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resGetStr() failed\n");
        } else {
            val = msg_getStringResult (result, 0);

	    if (*val && strncmp (val, "null", 4) != 0) {
		char *ip = val;
                len = strlen(val);
                str = calloc (1, len+1);

		/* Trim string left and right (right side first).
		*/
		for (ip=(val+len-1); *ip && (ip > val); ip--) {
		    if (ip && (*ip == ' ' || *ip == '\t' || *ip == '\n'))
		        *ip = '\0';
		    else
			break;
		}
		for (ip=val; ip && (*ip == ' ' || *ip == '\t' || *ip == '\n'); )
		    ip++;
                strncpy (str, ip, strlen(ip));
	    }
        }

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
	if (val)
            free ((void *)val);
    }

    return (str);
}


/**
 *  VOC_GETFLOAT -- Get a real-valued attribute from the result resource
 *  record.  Currently recognized real-valued attributes include:
 *
 *	MaxSR			maximum search radius
 *
 *  Attribute string are case-insensitive.
 *
 *  @brief  Get a real-valued attribute from the result resource record
 *  @fn	    dval = voc_resGetFloat (RegResult res, char *attr, int index)
 *
 *  @param  res 	Registry result handle
 *  @param  attr 	record attribute
 *  @param  index 	record index
 *  @returns		string-valued attribute
 */
double
voc_resGetFloat (RegResult res, char *attribute, int index)
{
    double   dval = 0;

    if (res > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (res, "resGetFloat", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, attribute);
	msg_addIntParam (msg, index);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resGetInt() failed\n");
        } else
            dval = msg_getFloatResult (result, 0);

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to resGetFloat\n");

    return (dval);
}


/**
 *  VOC_GETINT -- Get a integer-valued attribute from the result resource
 *  record.  Currently recognized real-valued attributes include:
 *
 *	MaxRecords		maximum records returned by the service
 *
 *  Attribute string are case-insensitive.
 *
 *  @brief  Get an int-valued attribute from the result resource record
 *  @fn	    ival = voc_resGetInt (RegResult res, char *attr, int index)
 *
 *  @param  res 	Registry result handle
 *  @param  attr 	record attribute
 *  @param  index 	record index
 *  @returns		string-valued attribute
 */
int
voc_resGetInt (RegResult res, char *attribute, int index)
{
    int      ival = 0;

    if (res > 0) {
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (res, "resGetInt", 0);
        vocRes_t *result = (vocRes_t *) NULL;

	msg_addStringParam (msg, attribute);
	msg_addIntParam (msg, index);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: resGetInt() failed\n");
        } else
            ival = msg_getIntResult (result, 0);

        free ((void *)msg);     	/* free the pointers            */
        free ((void *)result);
    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null result record to resGetInt\n");

    return (ival);
}
