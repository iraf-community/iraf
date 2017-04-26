/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_prstat
#define import_xwhen
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "construct.h"
#include "ytab.h"		/* pick up yacc token #defines		*/

#include "voclient.h"

#define SZ_RESBUF		(32*SZ_LINE)
#define DEF_RESATTR		"ServiceURL"
#define MAX_ATTRS		32


extern  int do_error;           /* runtime error handling               */
extern  ErrCom  errcom;

extern int     optbl[];
extern char   *ifnames[];

static int    debug			= 0;
static int    reg_nresolved 		= 0;

int    	      VOClient_initialized 	= 0;

char   	     *voGetStrArg();
int           voGetIntArg();
double        voGetDblArg();

#ifdef FOO
typedef int   DAL;                 	/* DAL type aliases             */
typedef int   Query;			/* DAL Query object		*/
typedef int   QResponse;		/* Query Response object	*/
typedef int   QRecord;			/* Query Record (row) object	*/
typedef int   QRAttribute;		/* Query Attr (col)  object	*/
typedef int   RegQuery;                 /* Registry Query object        */
typedef int   RegResult;                /* Query Result object          */
#endif

    

/**
 *   VO Client Library:
 *   ----------------------
 *
 *	 stat = initVOClient (opts)				    int [opt]
 *       stat = closeVOClient (quit_flag)		    	    int [opt]
 *     stat = restartVOClient (quit_flag)		    	    int [opt]
 *
 *         stat = validateObj (obj)		    	    	    int
 *            stat = vocReady ()		    	    	    bool
 *
 *   DAL Service Interface:   		(task prefix 'dal')
 *   ----------------------
 *
 *    Hi-Level Interface:
 *
 *          qres = dalConeSvc (url, ra, dec, sr)                     int
 *          qres = dalSiapSvc (url, ra, dec, rsize[, dsize[, fmt]])  int
 *
 *     count = dalRecordCount (qres)                                 int
 *
 *          stat = dalGetData (qres, recnum, fname)                  int
 *
 *            fname = getData (acref)                    	     string
 *            fname = getData (acref, fname)                         string
 *            fname = getData (rec, acref, fname)                    string
 *
 *         rec = dalGetRecord (qres, recnum)                         int
 *            str = dalGetStr (qres, attrname, recnum)               string
 *           ival = dalGetInt (qres, attrname, recnum)               int
 *           dval = dalGetDbl (qres, attrname, recnum)               double
 *
 *
 *    Low-Level Interface:
 *
 *       count = dalAttrCount (rec)                                  int
 *     attrname = dalAttrName (rec, index)                           string
 *          ival = dalIntAttr (rec, attrname)                        int
 *        dval = dalFloatAttr (rec, attrname)                        double
 *           str = dalStrAttr (rec, attrname)                        string
 *
 *        nscan = dalAttrScan (rec, attr_list, <p1>...<pN>)          *NYI
 *
 *
 *   Registry Search Interface:   	(task prefix 'reg')
 *   --------------------------
 *
 *	   str = regResolver (shortName,[svctype[,attr[,index]]])    string
 *	       N = nresolved ()					     int
 *
 *       resource = regSearch (term [, orValues])                    int     
 *       resource = regSearch (keywords, orValues)                   int     
 *       resource = regSearch (sql, keywords, orValues)              int     
 *
 *    resource = regSvcSearch (searchTerm, svcType, bpass, 
 *					   clevel, orValues)         int
 *
 *     count = regResultCount (resource)                             int     
 *             str = regValue (resource, attr_list, resIndex)        int
 *
 *            nscan = regScan (resource, attribute, resIndex, <p>)   *NYI
 */
 



/* VOCOP --  Process a VO Client request.
 */
vocop (opcode, op_index, nargs)
int     opcode;
int     op_index;
int     nargs;
{
    struct	operand o;
    int 	op = optbl[op_index];	


    if (debug)
	printf ("vocop:  opcode=%d  index=%d  nargs=%d\n", 
	    opcode, op_index, nargs);

#ifndef USE_VOCOP
    if (opcode != OP_INITVOC && opcode != OP_RESTARTVOC) {
        if (VOClient_initialized) {
	    int voc_status = voc_ready();
	    if (voc_status != 0) {
    	        VOClient_initialized = 0;
                voc_closeVOClient (0);
                if (voc_initVOClient (envget("vo_runid")) != OK)
                    cl_error (E_UERR, "Error re-initializing VOClient");
	    }
        } else {
            if (voc_initVOClient (envget("vo_runid")) != OK)
                cl_error (E_UERR, "Error initializing VOClient");
            else {
    		VOClient_initialized = 1;
		sleep (3);
            }
        }
    }
#else
    VOClient_initialized = 1;
#endif

    switch (opcode) {
    case OP_INITVOC:
	/* Initialize the VOClient
	 */
    	if (nargs > 1)
            cl_error (E_UERR, "usage: initVOClient([opt_string])\n");
	else
	    cl_initVOClient (nargs);
	break;
		
    case OP_CLOSEVOC:
	/* Shut down the VO Client connection	
	 */
	if (nargs != 1)
            cl_error (E_UERR, "usage: closeVOClient(shutdown_flag)\n");
	else
	    cl_closeVOClient ();
	break;

    case OP_RESTARTVOC:
	/* Restart the VO Client connection	
	 */
	if (nargs > 0)
            cl_error (E_UERR, "usage: restartVOClient()\n");
	else
	    cl_restartVOClient ();
	break;

    case OP_CONESVC:
	/* Query a Cone Service
	 */
	if (nargs < 4)
	    cl_error (E_UERR, "usage: res = dalConeSvc(url, ra, dec, sr)");
	else
	    cl_dalConeSvc ();
	break;
		
    case OP_SIAPSVC:
	/* Query a SIAP Service
	 */
	if (nargs < 4)
	    cl_error (E_UERR, 
		"usage: res = dalSiapSvc(url, ra, dec, rasz[, decsz[, fmt]])");
	else
	    cl_dalSiapSvc (nargs);
        break;

    case OP_RECCNT:
	/* Count DAL query result records
	 */
	if (nargs != 1)
            cl_error (E_UERR, "usage: dalRecordCount(qres)\n");
	else
	    cl_dalRecordCount ();
        break;

    case OP_GETREC:
	/* Get a QRecord handle from a QResponse
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: dalGetRecord(qres, recnum)\n");
	else
	    cl_dalGetRecord ();
        break;

    case OP_GETDATA:
	/* Get a dataset pointed to by an AccessReference field at the
	 * given record number in the query response.  Download to the
	 * named file.
	 */
	if (nargs != 3)
            cl_error (E_UERR, "usage: dalGetData(qres, recnum, fname)\n");
	else
	    cl_dalGetData ();
        break;

    case OP_GETSTR:
	/* Get a string-valued attribute from a DAL response table.
	 */
	if (nargs != 3)
            cl_error (E_UERR, "usage: dalGetStr(qres, attrname, recnum)\n");
	else
	    cl_dalGetStr ();
        break;

    case OP_GETINT:
	/* Get an integer-valued attribute from a DAL response table.
	 */
	if (nargs != 3)
            cl_error (E_UERR, "usage: dalGetInt(qres, attrname, recnum)\n");
	else
	    cl_dalGetInt ();
        break;

    case OP_GETDBL:
	/* Get a real-valued attribute from a DAL response table.
	 */
	if (nargs != 3)
            cl_error (E_UERR, "usage: dalGetDbl(qres, attrname, recnum)\n");
	else
	    cl_dalGetDbl ();
        break;

    case OP_ATTRCNT:
	/* Count number of attributes in a QRecord
	 */
	if (nargs != 1)
            cl_error (E_UERR, "usage: dalAttrCount(rec)\n");
	else
	    cl_dalAttrCount ();
        break;

    case OP_ATTRNAME:
	/* Get an attribute name for the given column index
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: dalAttrName(rec,index)\n");
	else
	    cl_dalAttrName ();
        break;

    case OP_INTATTR:
	/* Get a an attribute as an integer.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: dalIntAttr(rec,attrname)\n");
	else
	    cl_dalIntAttr ();
        break;

    case OP_FLOATATTR:
	/* Get a an attribute as a real.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: dalFloatAttr(rec,attrname)\n");
	else
	    cl_dalFloatAttr ();
        break;

    case OP_STRATTR:
	/* Get a an attribute as a string.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: dalStrAttr(rec,attrname)\n");
	else
	    cl_dalStrAttr ();
        break;

    case OP_DATASET:
	/* Download the dataset at the given acref to a named file
	 */
	if (nargs > 3)
            cl_error (E_UERR, "usage: dalDataset([rec,] acref [,fname])\n");
	else
	    cl_dalDataset (nargs);
        break;

    case OP_ATTRSCAN:
	/* Get the value of one or more attributes.
	 */
	if (nargs < 2)
            cl_error (E_UERR, "usage: dalAttrScan(rec,attrList,p1,...,pN)\n");
	else
	    cl_dalAttrScan (nargs);
        break;


    case OP_REGSEARCH:
	/* Get the value of the named resource attribute.
	 *       resource = regSearch (term [, orValues])
	 *       resource = regSearch (keywords, orValues)
	 *       resource = regSearch (sql, keywords, orValues)
	 */
	if (nargs < 1 || nargs > 3)
            cl_error (E_UERR, "usage: regSearch(res,attr)\n");
	else
	    cl_regSearch (nargs);
        break;

    case OP_REGSVCSEARCH:
	/* Get the value of the named resource attribute.
	 *    resource = regSvcSearch (term, svcType, bpass, clevel, orValues)
	 */
	if (nargs != 5)
            cl_error (E_UERR,
		"usage: regSvcSearch(term,svcType,bpass,clevel,orVals)\n");
	else
	    cl_regSvcSearch ();
        break;

    case OP_REGCOUNT:
	/* Get the value of the named resource attribute.
	 */
	if (nargs > 1)
            cl_error (E_UERR, "usage: regResultCount(res)\n");
	else
	    cl_regResultCount ();
        break;

    case OP_REGBPASS:
	/* Set a Bandpass constraint on a query.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: regSetBandpass(query, bpass)\n");
	else
	    cl_regSetBandpass ();
        break;

    case OP_REGSVC:
	/* Set a ServiceType constraint on a query.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: regSetService(res, svcType)\n");
	else
	    cl_regSetService ();
        break;

    case OP_REGCONTENT:
	/* Set a ContentLevel constraint on a query.
	 */
	if (nargs != 2)
            cl_error (E_UERR, "usage: regSetContent(res, clevel)\n");
	else
	    cl_regSetContent ();
        break;

    case OP_REGVALUE:
	/* Get the value of the named resource attribute.
	 *    nscan = regValue (resource, attribute, resIndex, <p>)
	 */
	if (nargs < 2 || nargs > 3)
            cl_error (E_UERR, "usage: regValue(res,attr)\n");
	else
	    cl_regValue ();
        break;


    case OP_REGRESOLVER:
	/* Get the number of resources found with the last regResolver() call.
	 */
	if (nargs < 1 || nargs > 4)
            cl_error (E_UERR, 
		"usage: regResolver(id_str [, svctyp [, attr [, index] ] ])\n");
	else
	    cl_regResolver (nargs);
        break;

    case OP_NRESOLVED:
	/* Get the number of resources found with the last regResolver() call.
	 */
	if (nargs > 0)
            cl_error (E_UERR, "usage: nresolved()\n");
	else
	    cl_regNResolved ();
        break;

    case OP_VALIDOBJ:
	/* Validate an object in the VO Client.
	 */
	if (nargs != 1)
            cl_error (E_UERR, "usage: validObj(obj)\n");
	else
	    cl_validObj ();
        break;

    case OP_VOCREADY:
	/* Verify that the VO Client is ready.
	 */
	if (nargs != 1)
            cl_error (E_UERR, "usage: vocReady()\n");
	else
	    cl_vocReady ();
        break;

    default:
    	cl_error (E_IERR, e_badsw, op, "voclient has invalid intrfunc()");
        break;
    }

    return (OK);
}


/* Initialize the VO Client.
 */
int
cl_initVOClient (nargs)
int nargs;
{
    char *opts = NULL, *runid = NULL;
    struct	operand o;

    if (nargs)
        opts = voGetStrArg ();

    runid = envget ("vo_runid");	/* Get the system RUNID	 	*/
    if (runid) {
	if (opts)
	    strcat (opts, ",");
	strcat (opts, runid);
    }

    o.o_type = OT_INT;
    if (! VOClient_initialized)
        o.o_val.v_i = voc_initVOClient (opts);
    else
        o.o_val.v_i = OK;
    pushop (&o);

    VOClient_initialized = 1;

    if (opts)
        free ((char *) opts);
    return (OK);
}
		

/* Close the VO Client connection.
 */
int
cl_closeVOClient ()
{
    int shutdown = 0;
    struct	operand o;

    shutdown = voGetIntArg ();

    /* The close is a void function so we simply return a dummy OK
     * value to the caller.
     */
    if (VOClient_initialized)
        (void) voc_closeVOClient (shutdown);

    o.o_type = OT_INT;
    o.o_val.v_i = OK;
    pushop (&o);

    VOClient_initialized = 0;

    return (OK);
}


/* Restart the VO Client.
 */
int
cl_restartVOClient ()
{
    char *opts = NULL;
    struct	operand o;

    o.o_type = OT_INT;
    voc_closeVOClient (1);
    if (VOClient_initialized)
        o.o_val.v_i = voc_initVOClient (envget("vo_runid"));
    else
        o.o_val.v_i = 0;
    pushop (&o);

    VOClient_initialized = 1;

    return (OK);
}


/* Validate an object in the VO Client.
 */
int
cl_validObj ()
{
    struct	operand o;

    int obj  = voGetIntArg ();

    o.o_type = OT_INT;
    if (! VOClient_initialized) {
	/* Client not initialized, obviously not valid.
	 */
        o.o_val.v_i = 0;
    } else
        o.o_val.v_i = voc_validateObject (obj);
    pushop (&o);

    return (OK);
}
		

/* Verify the VOClient is ready.
 */
int
cl_vocReady ()
{
    struct	operand o;

    o.o_type = OT_BOOL;
    if (! VOClient_initialized) {
	/* Client not initialized, obviously not valid.
	 */
        o.o_val.v_i = 0;
    } else
        o.o_val.v_i = voc_ready ();
    pushop (&o);

    return (OK);
}
		

/* Call a Cone service.
 */
int 
cl_dalConeSvc ()
{
    char      *url;
    double    sr, ra, dec;
    DAL       cone;
    Query     query;
    QResponse result = (QResponse) ERR;
    struct    operand o;


    /* Initialize the VO Client if it isn't already running. */
    if (! VOClient_initialized) {
	cl_initVOClient (NULL);
	o = popop ();			/* pop status posted by initVOClient */
    }

    /* Args are on the stack in reverse order, pop 'em now. */
    sr  = voGetDblArg ();
    dec = voGetDblArg ();
    ra  = voGetDblArg ();
    url = voGetStrArg ();

    /* Open a connection to the url, form a query and execute it.
     */
    cone = voc_openConeConnection (url);
    query = voc_getConeQuery (cone, ra, dec, sr);
    result = voc_executeQuery (query);

    o.o_type = OT_INT;				   /* push result on stack */
    o.o_val.v_i = result;
    pushop (&o);

    if (debug)
	printf ("dalConeSvc=%d: url='%s'\n\tra=%g dec=%g sr=%g\n", 
	   result, url, ra, dec, sr);

    if (url) free ((char *) url);

    return (OK);
}


/* Call a Siap service.
 */
int 
cl_dalSiapSvc (nargs)
int nargs;
{
    char     *fmt, *url;
    double    rasz, decsz, ra, dec;
    DAL       siap;
    Query     query;
    QResponse result;
    struct    operand o;


    /* Initialize the VO Client if it isn't already running. */
    if (! VOClient_initialized) {
	cl_initVOClient (NULL);
	o = popop ();			/* pop status posted by initVOClient */
    }

    /* Args are on the stack in reverse order, pop 'em now. */
    if (nargs > 5)
        fmt   = voGetStrArg ();
    else
        fmt   = "image/fits";
    if (nargs > 4) {
        decsz = voGetDblArg ();
        rasz  = voGetDblArg ();
    } else if (nargs == 4) {
        rasz = decsz = voGetDblArg ();
    }
    dec   = voGetDblArg ();
    ra    = voGetDblArg ();
    url   = voGetStrArg ();

    /* Open a connection to the url, form a query and execute it.
     */
    if (url && url[0]) {
        siap = voc_openSiapConnection (url);
        query = voc_getSiapQuery (siap, ra, dec, rasz, decsz, fmt);

        result = voc_executeQuery (query);
    } else
	cl_error (E_UERR, "Invalid or NULL service URL specified.");

    if (debug)
	printf ("dalSiapSvc: siap=%d query=%d result=%d\n",
	    siap, query, result);

    o.o_type = OT_INT;
    o.o_val.v_i = result;
    pushop (&o);

    if (debug)
	printf ("dalSiapSvc=%d: url='%s'\n\tra=%g dec=%g sz=%g/%g fmt=%s\n", 
	   result, url, ra, dec, rasz, decsz, fmt);

    if (url) free ((char *) url);
    if (fmt && nargs > 5) free ((char *) fmt);

    return ( (result ? OK : ERR) );
}


/* Get a count of the number of return records.
 */
int 
cl_dalRecordCount ()
{
    QResponse qres;
    int       count = -1;
    int       stat = OK;
    struct    operand o;


    qres = (QResponse) voGetIntArg ();

    if (qres > 0)
        count = voc_getRecordCount (qres);
    else
	stat = ERR;

    o.o_type = OT_INT;
    o.o_val.v_i = count;
    pushop (&o);

    return (stat);
}


/* Get the record handle.
 */
int 
cl_dalGetRecord ()
{
    QRecord   rec;
    QResponse qres;
    int       stat, count, recnum = -1;
    struct    operand o;

    recnum = voGetIntArg ();
    qres   = (QResponse) voGetIntArg ();

    if (voc_validateObject (qres)) {
	/* Do the bounds checking here so we can return a sensible error.
	 */
        count = voc_getRecordCount (qres);
	if (recnum < 0 || recnum > (count-1))
            cl_error (E_UERR, "dalGetRecord: index out-of-bounds\n");
	else
            rec = voc_getRecord (qres, recnum);
    } else 
        cl_error (E_UERR, "dalGetRecord: invalid `qres' argument\n");

    o.o_type = OT_INT;
    o.o_val.v_i = (int) rec;
    pushop (&o);

    return (stat);
}


/* Download a dataset referred to by a URL.
 */
int 
cl_dalGetData ()
{
    QRecord   rec;
    QResponse qres;
    QRAttribute attr;
    char      *fname, *acref, fbuf[SZ_FNAME];
    int       recnum, count = -1;
    int       stat = OK;
    struct    operand o;


    bzero (fbuf, SZ_FNAME);

    fname  = voGetStrArg ();
    recnum = voGetIntArg ();
    qres   = (QResponse) voGetIntArg ();

    if (voc_validateObject (qres)) {
	rec  = voc_getRecord (qres, recnum);
	if (voc_validateObject (rec)) {
            attr = voc_getAttribute (rec, "AccessReference");
	    if (voc_validateObject (attr)) {
	        acref = voc_stringValue (attr);
                stat  = voc_getDataset (rec, acref, fname);
	    } else
                cl_error (E_UERR, "getData: invalid `attr' argument\n");
	} else 
            cl_error (E_UERR, "getData: invalid `rec' argument\n");
    } else
        cl_error (E_UERR, "getData: invalid `qres' argument\n");

    o.o_type = OT_STRING;
    strcpy (fbuf, fname);
    o.o_val.v_s = fbuf;
    pushop (&o);

    if (fname) free ((char *) fname);
    if (acref) voc_freePointer( ((char *) acref) );
    return (stat);
}


/* Get a string-valued attribute from a DAL response table. 
 */
int 
cl_dalGetStr ()
{
    QRecord     rec;
    QResponse   qres;
    QRAttribute attr;
    char        *attrname, *aval, sbuf[SZ_LINE];
    int         recnum, stat = ERR;
    struct      operand o;


    recnum   = voGetIntArg ();
    attrname = voGetStrArg ();
    qres     = (QResponse) voGetIntArg ();

    bzero (sbuf, SZ_LINE);
    if (voc_validateObject (qres)) {
	rec  = voc_getRecord (qres, recnum);
	if (voc_validateObject (rec)) {
            attr = voc_getAttribute (rec, attrname);
	    if (voc_validateObject (attr)) {
    		if ( (aval = voc_stringValue (attr)) ) {
		    strcpy (sbuf, aval);
		    voc_freePointer( ((char *) aval) );
		    stat = OK;
		} 
	    } 
	}
    }

    o.o_type = OT_STRING;
    o.o_val.v_s = sbuf;
    pushop (&o);

    if (attrname) free ((char *) attrname);
    return (stat);
}


/* Get an integer-valued attribute from a DAL response table. 
 */
int 
cl_dalGetInt ()
{
    QRecord     rec;
    QResponse   qres;
    QRAttribute attr;
    char        *attrname;
    int         ival = INDEFI, recnum, stat = OK;
    struct      operand o;


    recnum   = voGetIntArg ();
    attrname = voGetStrArg ();
    qres     = (QResponse) voGetIntArg ();

    if (voc_validateObject (qres)) {
	rec  = voc_getRecord (qres, recnum);
	if (voc_validateObject (rec)) {
            attr = voc_getAttribute (rec, attrname);
	    if (voc_validateObject (attr))
    		ival = voc_intValue (attr);
	}
    }

    o.o_type = OT_INT;
    o.o_val.v_i = ival;
    pushop (&o);

    if (attrname) free ((char *) attrname);
    return (stat);
}


/* Get an integer-valued attribute from a DAL response table. 
*/
int 
cl_dalGetDbl ()
{
    QRecord     rec;
    QResponse   qres;
    QRAttribute attr;
    char        *attrname;
    int         recnum, stat = OK;
    double 	dval = INDEFD;
    struct      operand o;


    recnum   = voGetIntArg ();
    attrname = voGetStrArg ();
    qres     = (QResponse) voGetIntArg ();

    if (voc_validateObject (qres)) {
	rec  = voc_getRecord (qres, recnum);
	if (voc_validateObject (rec)) {
            attr = voc_getAttribute (rec, attrname);
	    if (voc_validateObject (attr))
    		dval = voc_floatValue (attr);
	}
    }

    o.o_type = OT_REAL;
    o.o_val.v_r = dval;
    pushop (&o);

    if (attrname) free ((char *) attrname);
    return (stat);
}



/* Return a count of the number of attributes (i.e. columns) in a record (row).
 */
int 
cl_dalAttrCount ()
{
    QRecord   rec;
    int       count = -1;
    int       stat = OK;
    struct    operand o;


    rec = (QRecord) voGetIntArg ();

    if (voc_validateObject (rec))
        count = voc_getAttrCount (rec);
    else {
        cl_error (E_UERR, "dalAttrCount: invalid `rec' argument\n");
	stat = ERR;
    }

    o.o_type = OT_INT;
    o.o_val.v_i = count;
    pushop (&o);

    return (stat);
}


/* Get the name of the attribute (i.e. column) given the index.
 */
int 
cl_dalAttrName ()
{
    QRecord   rec;
    int       index, count = -1;
    int       stat = OK;
    struct    operand o;


    index = voGetIntArg ();
    rec   = (QRecord) voGetIntArg ();

    if (voc_validateObject (rec)) {
	char *attr_list = voc_getAttrList (rec);
	char *ip = attr_list;

    } else {
        cl_error (E_UERR, "dalAttrCount: invalid `rec' argument\n");
	stat = ERR;
    }

    o.o_type = OT_STRING;
    o.o_val.v_s = "foo";
    pushop (&o);

    return (stat);
}


/* Get the requested attribute as an integer.
 */
int 
cl_dalStrAttr ()
{
    QRAttribute attr;
    QRecord   rec;
    char      *name;
    int       stat = OK;
    struct    operand o;


    name = voGetStrArg ();
    rec = (QRecord) voGetIntArg ();

    if (voc_validateObject (rec))
        attr = voc_getAttribute (rec, name);
    else {
        cl_error (E_UERR, "dalAttrCount: invalid `rec' argument\n");
	stat = ERR;
    }

    o.o_type = OT_STRING;
    o.o_val.v_s = voc_stringValue (attr);
    pushop (&o);

    if (name) free ((char *) name);

    return (stat);
}


/* Get the requested attribute as an integer.
 */
int 
cl_dalFloatAttr ()
{
    QRAttribute attr;
    QRecord   rec;
    char      *name;
    int       stat = OK;
    struct    operand o;


    name = voGetStrArg ();
    rec = (QRecord) voGetIntArg ();

    if (voc_validateObject (rec))
        attr = voc_getAttribute (rec, name);
    else {
        cl_error (E_UERR, "dalAttrCount: invalid `rec' argument\n");
	stat = ERR;
    }

    o.o_type = OT_REAL;
    o.o_val.v_r = voc_floatValue (attr);
    pushop (&o);

    if (name) free ((char *) name);

    return (stat);
}


/* Get the requested attribute as an integer.
 */
int 
cl_dalIntAttr ()
{
    QRAttribute attr;
    QRecord   rec;
    char      *name;
    int       stat = OK;
    struct    operand o;


    name = voGetStrArg ();
    rec = (QRecord) voGetIntArg ();

    if (voc_validateObject (rec))
        attr = voc_getAttribute (rec, name);
    else {
        cl_error (E_UERR, "dalAttrCount: invalid `rec' argument\n");
	stat = ERR;
    }

    o.o_type = OT_INT;
    o.o_val.v_s = voc_intValue (attr);
    pushop (&o);

    if (name) free ((char *) name);

    return (stat);
}


/* Download a dataset referred to by a URL.
 */
int 
cl_dalDataset (nargs)
int nargs;
{
    QRecord   rec = (QRecord) NULL;
    char      *fname, *acref, *res, *val, fbuf[SZ_FNAME];
    int       count = -1, stat = OK, urlsize = (2 * 1024 * 1024);
    struct    operand o;


    bzero (fbuf, SZ_FNAME);

    switch (nargs) {
    case 3:
        fname = voGetStrArg ();
        acref = voGetStrArg ();
        rec   = (QRecord) voGetIntArg ();
	break;
    case 2:
        fname = voGetStrArg ();
        acref = voGetStrArg ();
	break;
    case 1:
	fname = "uparm$url_file";
        acref = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "dalDataset: invalid argument list\n");
	break;
    }

    if (acref && acref[0]) {
	if (rec && voc_validateObject (rec)) {
            stat = voc_getDataset (rec, acref, fname);
        } else {
	    int  fd, nb, nbytes;

            res = voc_getRawURL (acref, &nbytes);
	    if (res == NULL)
        	cl_error (E_UERR, "getDataset: cannot access URL\n");

	    if (c_access (fname, 0, 0)) 
	 	c_delete (fname);
	    if ((fd = c_open (fname, NEW_FILE, BINARY_FILE)) != ERR) {
		nb = c_write (fd, res, nbytes);
		if (nb != nbytes)
		    printf ("Warning: short file write\n");
		c_close (fd);
	    } else {
        	cl_error (E_UERR, "dalDataset: cannot open output file\n");
	    }
	    voc_freePointer ((char *) res);
        }
    } else {
        cl_error (E_UERR, "dalDataset: invalid `acref' argument\n");
	stat = ERR;
    }

    o.o_type = OT_STRING;
    strcpy (fbuf, fname);
    o.o_val.v_s = fbuf;
    pushop (&o);

    if (acref) free ((char *) acref);
    if (nargs > 1 && fname) free ((char *) fname);

    return (stat);
}


/* Scan attribute values to local parameters.  Returns number of values
 * read (always the number requested, empty fields filled w/ INDEF) or EOF.
 */
int 
cl_dalAttrScan (nargs)
int nargs;
{
    QRecord     rec;
    QRAttribute attr;
    char      *alist, *val = NULL;
    char      *ip, *op, *attr_list[MAX_ATTRS];
    int       i, nattrs=0, stat = OK;
    struct    operand o;


    bzero (attr_list, MAX_ATTRS); 		/* clear arrays		    */

    alist = voGetStrArg ();			/* get the attribute list   */
    for (nattrs=0, ip=alist; *ip && nattrs < MAX_ATTRS; ) {
	attr_list[nattrs++] = op = ip;
	while (*ip && *ip != ',')
	    ip++;
	if (*ip == ',')
	    *ip++ = '\0';
	else
	    break;
    }
/*
for(i=0; i < nattrs; i++)
  printf ("%d:  attr_list='%s'\n", i, attr_list[i]);
*/

    rec = (QRecord) voGetIntArg ();

    if (attr) {
	for (i = (nattrs-1); i > 0; i--) {
            attr = voc_getAttribute (rec, attr_list[i]);
            val = voc_getStringAttr (rec, attr);
/*
printf ("%d:  attr='%s'  =>  '%s'\n", i, attr_list[i], val);
*/
	}
    } else
	stat = ERR;

    o.o_type = OT_STRING;
    o.o_val.v_i = val;
    pushop (&o);

    if (alist) free ((char *) alist);

    return (stat);
}




/*  Do a Registry search using keyword terms and/or sql predicates.  We 
 *  return a resource pointer to the records that can be queried using the
 *  regValue() builtin for specific items.
 *
 *  Usage:
 *       resource = regSearch (sql)
 *       resource = regSearch (keywords, orValues)
 *       resource = regSearch (sql, keywords, orValues)
 */
int 
cl_regSearch (nargs)
int nargs;
{
    RegResult res;
    char      *sql = NULL, *keyw = NULL;
    int       stat = OK, orValues = 0;
    struct    operand o;



    /* Initialize the VO Client if it isn't already running. */
    if (! VOClient_initialized) {
        cl_initVOClient (NULL);
        o = popop ();                   /* pop status posted by initVOClient */
    }

    /* Get the calling arguments in reverse order from the stack.
     */
    switch (nargs) {
    case 3:
        orValues = voGetIntArg ();
	keyw     = voGetStrArg ();
	sql      = voGetStrArg ();
	break;
    case 2:
        orValues = voGetIntArg ();
	keyw     = voGetStrArg ();
	break;
    case 1:
	sql      = voGetStrArg ();
	break;
    default:
        cl_error (E_UERR, "regSearch: invalid number of arguments\n");
	return (ERR);
    }


    /*  Do the registry query. */
    res = voc_regSearch (sql,  keyw, orValues);

    o.o_type = OT_INT;
    o.o_val.v_i = res;
    pushop (&o);

    /* Clean up and return.
     */
    if (keyw) free ((char *) keyw);
    if (sql)  free ((char *) sql);

    return (stat);
}


/*  Do a Registry search and limit results to a specific service type.
 *  Search term may be either a keyword list or sql predicate.
 */
int 
cl_regSvcSearch ()
{
    RegResult res;
    int       count = -1;
    char      *svc, *term, *bpass, *clevel;
    int       stat = OK, orValues = 0;
    struct    operand o;


    /* Initialize the VO Client if it isn't already running. */
    if (! VOClient_initialized) {
        cl_initVOClient (NULL);
        o = popop ();                   /* pop status posted by initVOClient */
    }

    orValues = voGetIntArg ();
    clevel   = voGetStrArg ();
    bpass    = voGetStrArg ();
    svc      = voGetStrArg ();
    term     = voGetStrArg ();


    /*  Do the registry query. */
    res = voc_regSearchByService (svc, term, orValues);


    o.o_type = OT_INT;
    o.o_val.v_i = res;
    pushop (&o);

    /* Clean up and return.
     */
    if (term) free ((char *) term);
    if (svc)  free ((char *) svc);

    return (stat);
}


/* Return a count of the number of attributes (i.e. columns) in a record (row).
 */
int 
cl_regResultCount ()
{
    RegResult res;
    int       count = -1;
    int       stat = OK;
    struct    operand o;

    res = (RegResult) voGetIntArg ();

    if (res)
        count = voc_resGetCount (res);
    else
	stat = ERR;

    o.o_type = OT_INT;
    o.o_val.v_i = count;
    pushop (&o);

    return (stat);
}

	    
/*  Set a bandpass constraint on a query.
 */
int
cl_regSetBandpass ()
{
    char     *bpass = NULL;
    RegQuery  query = 0;
    int       stat = OK;
    struct    operand o;

    bpass = voGetStrArg ();
    query = (RegQuery) voGetIntArg ();

    if (query)
        voc_regConstWaveband (query, bpass);
    else
	stat = ERR;

    o.o_type = OT_INT;
    o.o_val.v_i = stat;
    pushop (&o);

    return (stat);
}

	    
/*  Set a ServiceType constraint on a query.
 */
int
cl_regSetService ()
{
    char     *svctype = NULL;
    RegQuery  query = 0;
    int       stat = OK;
    struct    operand o;

    svctype = voGetStrArg ();
    query = (RegQuery) voGetIntArg ();

    if (query)
        voc_regConstSvcType (query, svctype);
    else
	stat = ERR;

    o.o_type = OT_INT;
    o.o_val.v_i = stat;
    pushop (&o);

    return (stat);
}

	    
/*  Set a ContentLevel constraint on a query.
 */
int
cl_regSetContent ()			/* FIXME - Not Yet Implemented */
{
    struct operand o;
    int    stat = OK;

    o.o_type = OT_INT;
    o.o_val.v_i = stat;
    pushop (&o);

    return (stat);
}


/*  Get the attribute value from the named resource query handle.
 */
int 
cl_regValue (nargs)
int	nargs;
{
    RegResult res;
    int      index = 0, stat = OK;
    char     *attr, *val, sbuf[SZ_RESBUF];
    struct    operand o;


    /* Get the arguments.  Remember they're on the stack backwards.
     */
    if (nargs > 2)
	index = voGetIntArg ();
    attr = voGetStrArg ();
    res = (RegResult) voGetIntArg ();

    bzero (sbuf, SZ_RESBUF);

    if (res)
        val = (char *) voc_resGetStr (res, attr, index);
    else
	stat = ERR;

    if (val && val[0])
	strcpy (sbuf, val);

    if (strlen (sbuf) > 255)
	sbuf[255] = '\0';

    o.o_type = OT_STRING;
    o.o_val.v_s = sbuf;
    pushop (&o);

    /* Clean up and return.
     */
    if (attr) free ((char *) attr);
    if (val) voc_freePointer ((char *) val);

    return (stat);
}


/*  Resolve a (presumed) ShortName or Identifier string to one or more
 *  Registry resource attributes.  By default we assume we are interested
 *  only in the ServiceURL, however optional arguments allow us to narrow
 *  the search to particular service types or individual records.  Examples:
 *
 *  1) Find the ServiceURL for the GSC2.2 catalog
 *
 *	cl> =regResolver ("GSC2.2")
 * 	http://chart.stsci.edu/GSCVO/GSC22VO.jsp?
 *	cl> =nresolved()
 *	2				# found more than one resource
 *
 *  2) Print the Title and ServiceType for each record found for USNO-B1:
 *
 *	cl> print (regResolver ("USNO-B1","","ServiceType,Title",-1))
 *	CONE    USNO-B1 Catalogue
 *	SKYNODE USNO-B1 SkyNode from VizieR
 *
 *	Note that in usage such as this we are still limited by the length
 *	of output permitted by the print() function (currently 32*SZ_LINE).
 *
 *  3) Get the ServiceURL for the USNO-B1 Skynode service:
 *
 *	cl> =regResolver ("USNO-B1","skynode")
 *	http://cdsws.u-strasbg.fr/USNO-B1BasicSkyNode/services/BasicSkyNode
 *
 */

int 
cl_regResolver (nargs)
int	nargs;
{
    int       i, j, nreturns=0, nattrs=0, comma = ',', stat = OK;
    int	      exact=-1, recnum=0, istart, iend;
    char      *qterm=NULL, *attr=NULL, *type=NULL, *attr_val=NULL;
    char      *ip, *op, *attr_list[MAX_ATTRS];
    char      qstring[SZ_LINE], svcstr[SZ_LINE];
    char      attr_str[SZ_LINE], sbuf[SZ_RESBUF];
    struct    operand o;
    RegQuery  query = 0;
    RegResult resource = 0;


    bzero (qstring, SZ_LINE);
    bzero (svcstr, SZ_LINE);
    bzero (sbuf, SZ_RESBUF);

    strcpy (attr_str, DEF_RESATTR);


    /* Initialize the VO Client if it isn't already running. */
    if (! VOClient_initialized) {
        cl_initVOClient (NULL);
        o = popop ();                   /* pop status posted by initVOClient */
    }

    /* Parse any remaining (optional) arguments.  Remember that the args are
     * on the stack in the reverse order!  The 1st arg is required and will 
     * be either an ivo: identifier or is presumed to be the ShortName.
     */
    attr_list[0] = "ShortName";
    attr_list[1] = "Identifier";
    attr_list[2] = "ServiceUrl";
    nattrs = 3;

    switch (nargs) {
    case 4: 
        recnum = voGetIntArg ();
	/*  fall thru */

    case 3: 
        attr = voGetStrArg ();
	if (attr && attr[0]) {
	    for (nattrs=2, ip=attr; *ip && nattrs < MAX_ATTRS; ) {
		attr_list[nattrs++] = op = ip;
		while (*ip && *ip != ',')
		    ip++;
		if (*ip == ',')
		    *ip++ = '\0';
		else
		    break;
	    }
	}
	/*  fall thru */

    case 2: 
        type = voGetStrArg ();
	if (type && type[0]) {
	    /*  Aliases for service type include:
	     *
	     *     Cone	-> catalog, table
	     *     SIAP	-> image, archive
	     *     SSAP	-> spectrum
	     *
	     */
	    if (strcmp(type,"catalog") == 0 || strcmp(type,"table") == 0)
	        sprintf (svcstr, "ServiceType like '%%cone%%'");
	    else if (strcmp(type,"image") == 0 || strcmp(type,"archive") == 0)
	        sprintf (svcstr, "ServiceType like '%%siap%%'");
	    else if (strcmp(type,"spectrum") == 0)
	        sprintf (svcstr, "ServiceType like '%%ssap%%'");
	    else 
	        sprintf (svcstr, "ServiceType like '%%%s%%'", type);
	}
	/*  fall thru */

    case 1: 
        qterm = voGetStrArg ();

	if (strncmp (qterm, "ivo://", 6) == 0) {	/* remove svc num. */
	    char *ip = qterm;
	    for (ip=qterm; *ip && *ip != '#'; ip++)
		;
	    *ip = '\0';
	}
	sprintf (qstring, 
	    "(Identifier like '%%%s%%') OR (ShortName like '%%%s%%')",
	    qterm, qterm);
	break;

    default:
        cl_error (E_UERR, "regResolver: invalid number of arguments\n");
	return (ERR);
    }



    /*  Do the registry query.
     */
    query = voc_regQuery (qstring, 0);
    if (type && type[0])
	voc_regAddSearchTerm (query, svcstr, 0);

    if (debug) {
	printf ("regResolver: qterm='%s'  type='%s'  attr='%s'  recnum=%d\n", 
	    (qterm ? qterm : "null"), 
	    (type ? type : "null"), 
	    (attr ? attr : "null"), recnum);
	printf ("query string:\n\n%s\n\n", voc_regGetQueryString (query));
    }

    /*  Execute the query.
     */
    resource = voc_regExecute (query);

    /* Save the number of resolved resources and get the requested attribute
     * (or the default service URL).
     */
    if ((reg_nresolved = voc_resGetCount (resource)) > 0) {
	nreturns = (recnum >= 0 ? 1 : reg_nresolved);

	exact = 0;
	if (recnum == 0 && reg_nresolved > 1) {
	    /* We didn't specify a record number but have more than one
	     * result.  Look for an exact match in the ShortName which was
	     * a hidden part of the query.
	     */
	    for (i=0; i < reg_nresolved; i++) {
                attr_val = voc_resGetStr (resource, attr_list[0], i);
		if (strncasecmp (qterm, attr_val, strlen(attr_val)) == 0) {
		    exact = i;
		    break;
		}
                attr_val = voc_resGetStr (resource, attr_list[1], i);
		if (strncasecmp (qterm, attr_val, strlen(attr_val)) == 0) {
		    exact = i;
		    break;
		}
	    }
	}

	if (exact >= 0 && recnum < 1 && nreturns == 1) {
	    istart = exact;
	    iend   = exact + 1;
	} else {
	    istart = 0;
	    iend   = nreturns;
	}

	/*  For a negative recnum we list the attr for all records.
	 */
	for (i=istart; i < iend; i++) {
	    for (j=2; j < nattrs; j++) {
                attr_val = voc_resGetStr (resource, attr_list[j], i);

                if (attr_val) {
		    strcat (sbuf, attr_val);
		    if (j < (nattrs-1))
			strcat (sbuf, "\t");
		    voc_freePointer ((char *) attr_val);
		} else
		    strcat (sbuf, "INDEF\t");
	    }
	    if (nreturns > 1 && i < (reg_nresolved-1))
		strcat (sbuf, "\n");
	}

        o.o_type = OT_STRING;
        o.o_val.v_s = sbuf;
	attr_val = (char *) NULL;

    } else {
	for (j=0; j < nattrs; j++) {
            strcpy (sbuf, "INDEF");
	    if (j < (nattrs-1))
		strcat (sbuf, "\t");
	}

        o.o_type = OT_STRING;
        o.o_val.v_s = sbuf;
    }

    /* Push the result operand on the stack.
     */
    pushop (&o);


    /* Clean up and return.
     */
    if (qterm) free ((char *) qterm);
    if (type)  free ((char *) type);
    if (attr)  free ((char *) attr);

    return (stat);
}


/*  Get a count of the number of return records.
 */
int 
cl_regNResolved ()
{
    struct    operand o;

    o.o_type = OT_INT;
    o.o_val.v_i = reg_nresolved;
    pushop (&o);

    return (OK);
}


/* ========================================================
 *
 * End of VO Client functions.
 * What follows is their support code.
 *
 * ========================================================*/


int
voGetIntArg ()
{
    struct	operand o;
    int ival;


    o = popop();

    switch (o.o_type & OT_BASIC) {
    case OT_BOOL:
    case OT_INT: 	ival = (int) o.o_val.v_i; 	 break;
    case OT_REAL: 	ival = (int) o.o_val.v_r; 	 break;
    case OT_STRING: ival = (int) atoi (o.o_val.v_s); break;
    default:
        cl_error (E_UERR, "printf: bad operand type\n");
    }

    if (debug)
        printf ("ival arg = '%d'\n", ival);

    return (ival);
}


double
voGetDblArg ()
{
    struct	operand o;
    double dval;


    o = popop();

    switch (o.o_type & OT_BASIC) {
    case OT_BOOL:
    case OT_INT: 	dval = (double) o.o_val.v_i; 	    break;
    case OT_REAL: 	dval = (double) o.o_val.v_r; 	    break;
    case OT_STRING: dval = (double) atof (o.o_val.v_s); break;
    default:
        cl_error (E_UERR, "printf: bad operand type\n");
    }

    if (debug)
        printf ("dval arg = '%g'\n", dval);

    return (dval);
}


/* Note: caller must free the pointer!
 */
char *
voGetStrArg ()
{
    struct  operand o;
    char   *str = NULL;

    o = popop();

    switch (o.o_type & OT_BASIC) {
    case OT_BOOL:
    case OT_INT: 	
        str = calloc (1, SZ_FNAME);
        sprintf (str, "%d", o.o_val.v_i);  
        break;
    case OT_REAL: 	
        str = calloc (1, SZ_FNAME);
        sprintf (str, "%f", o.o_val.v_r);  
        break;
    case OT_STRING: 
        str = calloc (1, strlen(o.o_val.v_s)+1);
        sprintf (str, "%s", o.o_val.v_s);  
        break;
    default:
        cl_error (E_UERR, "printf: bad operand type\n");
    }

    if (debug)
        printf ("str arg = '%s'\n", str);

    return (str);
}

