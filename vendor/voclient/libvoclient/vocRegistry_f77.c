/*
**  VIC_REGISTRYQUERY -- Utility code to act as a client interface to
**  the NVO Registry service.
**
** RegistryQuery
** ----------------------
** 
**  High-Level Query:
** 
**           res = voc_regSearch (term1, term2, orValues)
**      res = voc_regSearchBySvc (svc, term, orValues)
** 
**  Programmatic Query:
** 
**          query = voc_regQuery (term, orValues) 	// OR keyword list?
** 
**          voc_regAddSearchTerm (query, term, orValue)	// OR term w/ previous
**       voc_regRemoveSearchTerm (query, term)		// remove search term
**     count = voc_regGetSTCount (query)
** 
**   str = voc_regGetQueryString (query)		// GET form of query
** 
**          res = voc_regExecute (query)		// return result obj
**       str = voc_regExecuteRaw (query)		// return raw XML
** 
** RegistryQueryResult
** 
**     count = voc_resGetCount  (res)
** 
**         str = voc_resGetStr  (res, attribute, index)
**      dval = voc_resGetFloat  (res, attribute, index)
**        ival = voc_resGetInt  (res, attribute, index)
** 
**     For this implementation, we've chose to use the NVO Registry at
** JHU/STScI, specifically the QueryRegistry() method which provides a
** 'SimpleResource' form of the resource record.  Support for the newer
** IVOA standard will be added later, for now we can quickly access the most
** commonly used fields of a resource using both a keyword and SQL form of
** the search.
** 
**  M. Fitzpatrick, NOAO, July 2006
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

#define VF_REGSEARCH 		vfregsearch
#define VF_REGSEARCHBYSVC	vfregsearchbysvc
#define VF_REGQUERY 		vfregquery
#define VF_REGADDSEARCHTERM 	vfregaddsearchterm
#define VF_REGREMOVESEARCHTERM 	vfregremovesearchterm
#define VF_REGGETSTCOUNT 	vfreggetstcount
#define VF_REGGETQUERYSTRING 	vfreggetquerystring
#define VF_REGEXECUTE 		vfregexecute
#define VF_REGEXECUTERAW 	vfregexecuteraw
#define VF_RESGETCOUNT 		vfresgetcount
#define VF_RESGETSTR 		vfresgetstr
#define VF_RESGETFLOAT 		vfresgetfloat
#define VF_RESGETINT 		vfresgetint

#else

#define VF_REGSEARCH 		vfregsearch_
#define VF_REGSEARCHBYSVC	vfregsearchbysvc_
#define VF_REGQUERY 		vfregquery_
#define VF_REGADDSEARCHTERM 	vfregaddsearchterm_
#define VF_REGREMOVESEARCHTERM 	vfregremovesearchterm_
#define VF_REGGETSTCOUNT 	vfreggetstcount_
#define VF_REGGETQUERYSTRING 	vfreggetquerystring_
#define VF_REGEXECUTE 		vfregexecute_
#define VF_REGEXECUTERAW 	vfregexecuteraw_
#define VF_RESGETCOUNT 		vfresgetcount_
#define VF_RESGETSTR 		vfresgetstr_
#define VF_RESGETFLOAT 		vfresgetfloat_
#define VF_RESGETINT 		vfresgetint_

#endif


/* Prototype declarations.
*/
void VF_REGSEARCH (char *term1,  char *term2, int *orValues, RegResult *result,
			int *ier, int len1, int len2);
void VF_REGSEARCHBYSVC (char *svc,  char *term, int *orValues, 
			RegResult *result, int *ier, int slen, int tlen);
void VF_REGQUERY (char *term, int *orValues, RegQuery *query, int *ier, 
			int len);
void VF_REGADDSEARCHTERM (RegQuery *query, char *term, int *orValue, int tlen);
void VF_REMOVESEARCHTERM (RegQuery *query, char *term, int tlen);
void VF_REGGETSTCOUNT (RegQuery *query, int *count);
void VF_REGGETQUERYSTRING (RegQuery *query, char *qstr, int *len, int qlen);
void VF_REGEXECUTE (RegQuery *query, RegResult *result);
void VF_REGEXECUTERAW (RegQuery *query, char *raw, int *len, int rlen);
void VF_RESGETCOUNT (RegResult *res, int *count);
void VF_RESGETSTR (RegResult *res, char *attr, int *index, char *str, int *len,
			int alen, int slen);
void VF_RESGETFLOAT (RegResult *res, char *attr, int *index, double *dval,
			int alen);
void VF_RESGETINT (RegResult *res, char *attr, int *index, int *ival, int alen);


extern VOClient *vo;                    /* Interface runtime struct     */


/** Private interface declarations.
**/
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);




/*****************************************************************************
**  VF_REGSEARCH --  High-level procedure to form a query and execute it
**  immediately.  We allow that 'term1' may be a complex SQL WHERE predicate,
**  and that 'term2' (or vice versa) is a search-keyword list.  The
**  'orValues' applies to the keyword list (if present), otherwise it applies
**  to the two search term elements.  The default action if two terms are
**  specified is to logically AND them.
**
**  The thinking here is that one might want SIAP services for Quasars.  This
**  is easily expressed in an SQL form to get SIAP resources, however a
**  Quasar may be known as a QSO, AGN, active-nuclei, etc and so we need a 
**  easy way to OR the keywords but AND that result with the SQL predicate.
*/
void
VF_REGSEARCH (char *term1,  char *term2, int *orValues, RegResult *result, 
	int *ier, int len1, int len2)
{
    char *_term1 = sstrip (term1, len1);
    char *_term2 = sstrip (term2, len2);

    *result = voc_regSearch (_term1, _term2, *orValues);
    *ier = (*result ? OK : ERR);

    free ((char *) _term1);
    free ((char *) _term2);
}


void
VF_REGSEARCHBYSVC (char *svc,  char *term, int *orValues, RegResult *result, 
	int *ier, int slen, int tlen)
{
    char *_svc = sstrip (svc, slen);
    char *_term = sstrip (term, tlen);

    *result = voc_regSearchByService (_svc, _term, *orValues);
    *ier = (*result ? OK : ERR);

    free ((char *) _svc);
    free ((char *) _term);
}


/*****************************************************************************
** VF_REGQUERY --  Get a RegistryQuery object.
*/
void  
VF_REGQUERY (char *term, int *orValues, RegQuery *query, int *ier, int len)
{
    char *_term = sstrip (term, len);

    *query = voc_regQuery (_term, *orValues);
    *ier = (*query ? OK : ERR);

    free ((char *) _term);
}


/*****************************************************************************
**  VF_REGADDSEARCHTERM -- Add a search term (sql predicate or keyword list)
**  to the specified query.
*/
void
VF_REGADDSEARCHTERM (RegQuery *query, char *term, int *orValue, int len)
{
    char *_term = sstrip (term, len);

    voc_regAddSearchTerm (*query, _term, *orValue);

    free ((char *) _term);
}


/*****************************************************************************
**  VF_REMOVESEARCHTERM -- Remove the search term from the query.
*/
void
VF_REMOVESEARCHTERM (RegQuery *query, char *term, int len)
{
    char *_term = sstrip (term, len);

    voc_regRemoveSearchTerm (*query, _term);

    free ((char *) _term);
}


/*****************************************************************************
**  VF_REGGETSTCOUNT -- Get the number of search terms in the current query.
*/
void
VF_REGGETSTCOUNT (RegQuery *query, int *count)
{
    *count = voc_resGetCount (*query);
}


/*****************************************************************************
**  VF_REGGETQUERYSTRING -- Get the current query as an http GET URL.
*/
void
VF_REGGETQUERYSTRING (RegQuery *query, char *qstr, int *len, int qlen)
{
    char *_result = voc_regGetQueryString (*query);

    memset (qstr, 0, qlen);
    if ((*len = strlen(_result)) > qlen)
        fprintf (stderr, "Warning: truncating result string: len=%d maxch=%d\n",
            *len, qlen);
    spad (strncpy (qstr, _result, *len), qlen);

    free ((char *) _result);
}


/*****************************************************************************
**  VF_REGEXECUTE -- Execute the specified query, returning a result object
**  code or NULL.
*/
void
VF_REGEXECUTE (RegQuery *query, RegResult *result)
{
    *result = voc_regExecute (*query);
}


/*****************************************************************************
**  VF_REGEXECUTERAW -- Execute the specified query and return the raw
**  resulting XML string.
*/
void
VF_REGEXECUTERAW (RegQuery *query, char *raw, int *len, int rlen)
{
    char *_result = voc_regExecuteRaw (*query);

    memset (raw, 0, rlen);
    if ((*len = strlen(_result)) > rlen)
        fprintf (stderr, "Warning: truncating result string: len=%d maxch=%d\n",
            *len, rlen);
    spad (strncpy (raw, _result, *len), rlen);

    free ((char *) _result);
}


/****************************************************************************/
/*********************  RegistryQueryResult Methods  ************************/
/****************************************************************************/


/*****************************************************************************
**  VF_RESGETCOUNT -- Return a count of the number of results records.
*/
void
VF_RESGETCOUNT (RegResult *res, int *count)
{
    *count = voc_resGetCount (*res);
}


/*****************************************************************************
**  VF_GETSTR -- Get a string-valued attribute from the result resource
**  record.  Currently recognized real-valued attributes include:
**
**	Title			Resource title (long version)
**	ShortName		Short name of Resource
**	ServiceURL		Service URL (if appropriate)
**	ReferenceURL		URL to reference about Resource
**	Description		Text description of resource
**	Identifier		Standard ivo identifier of resource
**	ServiceType		Service Type (Cone, Siap, etc)
**	Type			Resource Type (catalog, survey, etc)
**	CoverageSpatial		Spatial coverage (STC)
**	CoverageTemporal	Temporal coverage of data
**
**	CoverageSpectral	Spectral coverage (csv list of bandpasses)
**	ContentLevel		Content level (research, EPO, etc -- csv list)
**
**  Attribute string are case-insensitive.
*/
void
VF_RESGETSTR (RegResult *res, char *attr, int *index, char *str, int *len,
	int alen, int slen)
{
    char *_attr = sstrip (attr, alen);

    char *_result = voc_resGetStr (*res, _attr, *index-1);

    memset (str, 0, slen);
    if ((*len = strlen(_result)) > slen)
        fprintf (stderr, "Warning: truncating result string: len=%d maxch=%d\n",
            *len, slen);
    spad (strncpy (str, _result, *len), slen);

    free ((char *) _result);
    free ((char *) _attr);
}


/*****************************************************************************
**  VF_GETFLOAT -- Get a real-valued attribute from the result resource
**  record.  Currently recognized real-valued attributes include:
**
**	MaxSR			maximum search radius
**
**  Attribute string are case-insensitive.
*/
void
VF_RESGETFLOAT (RegResult *res, char *attr, int *index, double *dval, int alen)
{
    char *_attr = sstrip (attr, alen);

    *dval = voc_resGetFloat (*res, _attr, *index-1);

    free ((char *) _attr);
}


/*****************************************************************************
**  VF_GETINT -- Get a integer-valued attribute from the result resource
**  record.  Currently recognized real-valued attributes include:
**
**	MaxRecords		maximum records returned by the service
**
**  Attribute string are case-insensitive.
*/
void
VF_RESGETINT (RegResult *res, char *attr, int *index, int *ival, int alen)
{
    char *_attr = sstrip (attr, alen);

    *ival = voc_resGetInt (*res, _attr, *index-1);

    free ((char *) _attr);
}
