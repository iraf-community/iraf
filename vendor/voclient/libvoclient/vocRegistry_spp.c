/**
 *  VOC_REGISTRYQUERY -- Utility code to act as a client interface to
 *  the NVO Registry service.
 *
 *  RegistryQuery
 *  ----------------------
 * 
 *  High-Level Query:
 * 
 *           res = voc_regSearch (term1, term2, orValues)
 * 
 *  Programmatic Query:
 * 
 *          query = voc_regQuery (term, orValues) 	// OR keyword list?
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
 *  @file       vocRegistry_spp.c
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


/* Procedure name mapping.
 */
#ifdef _NO_US_

#define vx_regsearch 		vxregh
#define vx_regsearchbysvc 	vxregc
#define vx_regquery 		vxregy
#define vx_raddsearchterm 	vxradm
#define vx_rremovesearchterm 	vxrrem
#define vx_rgetstcount 		vxrget
#define vx_rgetquerystring 	vxrgeg
#define vx_rexecute 		vxrexe
#define vx_rexecuteraw 		vxrexw


#define vx_rscount 		vxrsct
#define vx_rsstr 		vxrssr
#define vx_rsfloat 		vxrsft
#define vx_rsint 		vxrsit

#else

#define vx_regsearch 		vxregh_
#define vx_regsearchbysvc 	vxregc_
#define vx_regquery 		vxregy_
#define vx_raddsearchterm 	vxradm_
#define vx_rremovesearchterm 	vxrrem_
#define vx_rgetstcount 		vxrget_
#define vx_rgetquerystring 	vxrgeg_
#define vx_rexecute 		vxrexe_
#define vx_rexecuteraw 		vxrexw_

#define vx_rscount 		vxrsct_
#define vx_rsstr 		vxrssr_
#define vx_rsfloat 		vxrsft_
#define vx_rsint 		vxrsit_

#endif

extern VOClient *vo;                    /* Interface runtime struct     */



/* SPP Type definitions.
 */
#define XCHAR           short
#define PKCHAR          char
#define XINT            int
#define XEOS            NULL


/*  Public interface procedures.
 *
 */


RegResult   vx_regsearch (XCHAR *term1,  XCHAR *term2, int *orValues);
RegResult   vx_regsearchbysvc (XCHAR *svc,  XCHAR *term, int *orValues);
RegQuery    vx_regquery (XCHAR *term, int *orValues);
void 	    vx_raddsearchterm (RegQuery *query, XCHAR *term, int *orValue);
void 	    vx_rremovesearchterm (RegQuery *query, XCHAR *term);
int	    vx_rgetstcount (RegQuery *query);
int	    vx_rgetquerystring (RegQuery *query, XCHAR *qstring, int *maxch);
RegResult   vx_rexecute (RegQuery *query);
int	    vx_rexecuteraw (RegQuery *query, XCHAR *result, int *maxch);

int	    vx_rgcount (RegResult *res);
int	    vx_rgstr (RegResult *res, XCHAR *attribute, int *index, 
			XCHAR *result, int *maxch);
double	    vx_rgfloat (RegResult *res, XCHAR *attribute, int *index);
int 	    vx_rgint (RegResult *res, XCHAR *attribute, int *index);





/*  Private interface procedures.
 */
extern PKCHAR *spp2c (XCHAR *instr,  int maxch);
extern int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
extern int    spplen (XCHAR *str);



/**
 *  VX_REGSEARCH --  High-level procedure to form a query and execute it
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
 *  @fn     res = vx_regsearch (XCHAR *term1, XCHAR *term2, int *orValues)
 *
 *  @param  term1       first search term
 *  @param  term2       second search term
 *  @param  orValues    logically OR values?
 *  @returns            handle to result object
 */
RegResult 
vx_regsearch (XCHAR *term1,  XCHAR *term2, int *orValues)
{
    char *_term1 = spp2c (term1, spplen (term1));
    char *_term2 = spp2c (term2, spplen (term2));

    RegResult _result = voc_regSearch (_term1, _term2, *orValues);

    free ((char *) _term1);
    free ((char *) _term2);
    return (_result);
}


/**
 *  VX_REGSEARCHBYSERVICE -- Search the Registry using a search term and
 *  constrain by service type.
 *
 *  @brief  Search Registry using a search term and service constraint
 *  @fn     res = vx_regsearchbysvc (XCHAR *svc, XCHAR *term, int *orValues)
 *
 *  @param  svc         service type constraint
 *  @param  term        keyword search term
 *  @param  orValues    logically OR values?
 *  @returns            handle to result object
 */
RegResult 
vx_regsearchbysvc (XCHAR *svc,  XCHAR *term, int *orValues)
{
    char *_svc = spp2c (svc, spplen (svc));
    char *_term = spp2c (term, spplen (term));

    RegResult _result = voc_regSearchByService (_svc, _term, *orValues);

    free ((char *) _svc);
    free ((char *) _term);
    return (_result);
}


/**
 * VX_REGQUERY --  Get a RegistryQuery object.
 *
 *  @brief  Create a RegistryQuery object.
 *  @fn     query = vx_regquery (XCHAR *term, int *orValues)
 *
 *  @param  term        keyword search term
 *  @param  orValues    logically OR values?
 *  @returns            handle to query object
 */
RegQuery  
vx_regquery (XCHAR *term, int *orValues)
{
    char *_term = spp2c (term, spplen (term));
    RegQuery query = voc_regQuery (_term, *orValues);

    free ((char *) _term);
    return (query);
}


/**
 *  VX_REGADDSEARCHTERM -- Add a search term (sql predicate or keyword list)
 *  to the specified query.
 *
 *  @brief  Add a search term to the specified query
 *  @fn     vx_raddsearchterm (RegQuery *query, XCHAR *term, int *orValue)
 *
 *  @param  query       Registry query handle
 *  @param  term        keyword search term
 *  @param  orValues    logically OR values?
 *  @returns            nothing
 */
void
vx_raddsearchterm (RegQuery *query, XCHAR *term, int *orValue)
{
    char *_term = spp2c (term, spplen (term));

    (void) voc_regAddSearchTerm (*query, _term, *orValue);

    free ((char *) _term);
}


/**
 *  VX_REMOVESEARCHTERM -- Remove the search term from the query.
 *
 *  @brief  Remove a search term to the specified query
 *  @fn     call vx_rremovesearchterm (RegQuery *query, XCHAR *term)
 *
 *  @param  query       Registry query handle
 *  @param  term        keyword search term
 *  @returns            nothing
 */
void
vx_rremovesearchterm (RegQuery *query, XCHAR *term)
{
    char *_term = spp2c (term, spplen (term));

    (void) voc_regRemoveSearchTerm (*query, _term);

    free ((char *) _term);
}


/**
 *  VX_REGGETSTCOUNT -- Get the number of search terms in the current query.
 *
 *  @brief  Get the number of search terms in the current query.
 *  @fn     count = vx_rgetstcount (RegQuery *query)
 *
 *  @param  query       Registry query handle
 *  @returns            count of search terms
 */
int
vx_rgetstcount (RegQuery *query)
{
    return ( voc_regGetSTCount (*query) );
}


/**
 *  VX_REGGETQUERYSTRING -- Get the current query as an http GET URL.
 *
 *  @brief  Get the current query as an http GET URL.
 *  @fn     len = vx_rgetquerystring (RegQuery *query, XCHAR *qstr, int *maxch)
 *
 *  @param  query       Registry query handle
 *  @param  qstr        returned query string
 *  @param  maxch       length of query string
 *  @returns            nothing
 */
int
vx_rgetquerystring (RegQuery *query, XCHAR *qstring, int *maxch)
{
    char *_qstring = voc_regGetQueryString (*query);

    int len = c2spp (_qstring, qstring, *maxch);

    free ((char *) _qstring);
    return (len);
}


/**
 *  VX_REGEXECUTE -- Execute the specified query, returning a result object
 *  code or NULL.
 *
 *  @brief  Execute the specified query
 *  @fn     res = vx_rexecute (RegQuery *query)
 *
 *  @param  query       Registry query handle
 *  @returns            nothing
 */
RegResult
vx_rexecute (RegQuery *query)
{
    return ( voc_regExecute (*query) );
}


/**
 *  VX_REGEXECUTERAW -- Execute the specified query and return the raw
 *  resulting XML string.
 *
 *  @brief  Execute the specified query and return raw result string
 *  @fn     len = vx_rexecuteraw (RegQuery *query, XCHAR *raw, int *maxch)
 *
 *  @param  query       Registry query handle
 *  @param  raw         raw result string
 *  @param  maxch       length of result string
 *  @returns            nothing
 */
int
vx_rexecuteraw (RegQuery *query, XCHAR *result, int *maxch)
{
    char *_raw = voc_regExecuteRaw (*query);

    int len = c2spp (_raw, result, *maxch);

    free ((char *) _raw);
    return (len);
}


/****************************************************************************/
/*********************  RegistryQueryResult Methods  ************************/
/****************************************************************************/


/**
 *  VX_RESGETCOUNT -- Return a count of the number of results records.
 *
 *  @brief  Return a count of the number of results records.
 *  @fn     count = vx_rscount (RegResult *res)
 *
 *  @param  res         Registry result handle
 *  @returns            number of result records
 */
int
vx_rscount (RegResult *res)
{
    return ( voc_resGetCount (*res) );
}


/**
 *  VX_GETSTR -- Get a string-valued attribute from the result resource
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
 *
 *	CoverageSpectral	Spectral coverage (csv list of bandpasses)
 *	ContentLevel		Content level (research, EPO, etc -- csv list)
 *
 *  Attribute string are case-insensitive.
 *
 *  @brief  Get a string-valued attribute from the result resource record
 *  @fn     len = vx_rsstr (RegResult *res, XCHAR *attr, int *index,
 *                      XCHAR *result, int *maxch)
 *
 *  @param  res         Registry result handle
 *  @param  attr        record attribute
 *  @param  index       record index
 *  @param  result      attribute string
 *  @param  maxch       length of attribute string
 *  @returns            nothing
 */
int
vx_rsstr (RegResult *res, XCHAR *attribute, int *index, 
	XCHAR *result, int *maxch)
{
    char *_attribute = spp2c (attribute, spplen (attribute));

    char *_result = voc_resGetStr (*res, _attribute, *index);
    int len = c2spp (_result, result, *maxch);

    free ((char *) _result);
    return (len);
}


/**
 *  VX_GETFLOAT -- Get a real-valued attribute from the result resource
 *  record.  Currently recognized real-valued attributes include:
 *
 *	MaxSR			maximum search radius
 *
 *  Attribute string are case-insensitive.
 *
 *  @brief  Get a real-valued attribute from the result resource record
 *  @fn     dval = vx_rsfloat (RegResult *res, XCHAR *attr, int *index)
 *
 *  @param  res         Registry result handle
 *  @param  attr        record attribute
 *  @param  index       record index
 *  @returns            double-precision value
 */
double
vx_rsfloat (RegResult *res, XCHAR *attribute, int *index)
{
    char *_attribute = spp2c (attribute, spplen (attribute));

    double dval = voc_resGetFloat (*res, _attribute, *index);

    free ((char *) _attribute);
    return (dval);
}


/**
 *  VX_GETINT -- Get a integer-valued attribute from the result resource
 *  record.  Currently recognized real-valued attributes include:
 *
 *	MaxRecords		maximum records returned by the service
 *
 *  Attribute string are case-insensitive.
 *
 *  @brief  Get an int-valued attribute from the result resource record
 *  @fn     ival = vx_rsint (RegResult *res, XCHAR *attr, int *index)
 *
 *  @param  res         Registry result handle
 *  @param  attr        record attribute
 *  @param  index       record index
 *  @returns            integer value
 */
int
vx_rsint (RegResult *res, XCHAR *attribute, int *index)
{
    char *_attribute = spp2c (attribute, spplen (attribute));

    int ival = voc_resGetInt (*res, _attribute, *index);

    free ((char *) _attribute);
    return (ival);
}
