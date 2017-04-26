/***************************************************************************
**
**  VOCF77.C  -- Fortran binding for the VOClient interface.  As part of 
**  the binding we map the interface procedure names and convert string
**  constants as needed per Fortran rules.  Note that another aspect of
**  the fortran calling convention is that the length of strings in the
**  argument list are appended to the call stack.  As C code we need to
**  take this into account when defining the interface.
**
**  M. Fitzpatrick, NOAO, Jul 2006
**
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/*  Fortran Interface Definitions.
**  
**  Fortran compilers on various platforms may append one or more trailing 
**  underscores to symbol names, we'll use macros for the interface names 
**  and use defines to see what the symbol name is.
*/

#ifdef _NO_US_
#define VF_CONECALLTOFILE 	vfconetofile
#define VF_SIAPCALLTOFILE 	vfsiaptofile
#define VF_SSAPCALLTOFILE 	vfssaptofile
#define	VF_GETRAWURL 		vfgetrawurl

#define	VF_OPENCONNECTION 	vfopenconnection
#define	VF_OPENCONECONNECTION 	vfopenconeconnection
#define	VF_OPENSIAPCONNECTION 	vfopensiapconnection
#define	VF_OPENSSAPCONNECTION 	vfopenssapconnection
#define	VF_CLOSECONNECTION 	vfcloseconnection
#define	VF_GETSERVICECOUNT 	vfgetservicecount
#define	VF_ADDSERVICEURL 	vfaddserviceurl
#define	VF_GETSERVICEURL 	vfgetserviceurl
#define	VF_GETQUERY 		vfgetquery
#define	VF_GETCONEQUERY 	vfgetconequery
#define	VF_GETSIAPQUERY 	vfgetsiapquery
#define	VF_GETSSAPQUERY 	vfgetssapquery
#define	VF_ADDINTPARAM 		vfaddintparam
#define	VF_ADDFLOATPARAM 	vfaddfloatparam
#define	VF_ADDSTRINGPARAM 	vfaddstringparam
#define	VF_GETQUERYSTRING 	vfgetquerystring
#define	VF_EXECUTEQUERY 	vfexecquery
#define	VF_GETQUERYRESPONSE 	vfgetqr
#define	VF_EXECUTECSV 		vfexeccsv
#define	VF_EXECUTETSV 		vfexectsv
#define	VF_EXECUTEASCII		vfexecascii
#define	VF_EXECUTEVOTABLE 	vfexecvotable
#define	VF_EXECUTEQUERYAS 	vfexecqueryas
#define	VF_GETRECORDCOUNT 	vfgetrecordcount
#define	VF_GETRECORD 		vfgetrecord
#define	VF_GETFIELDATTR		vfgetfieldattr
#define	VF_GETATTRCOUNT 	vfgetattrcount
#define	VF_GETATTRLIST 		vfgetattrlist
#define	VF_GETATTRIBUTE 	vfgetattribute
#define	VF_GETINTATTR 		vfgetintattr
#define	VF_GETFLOATATTR 	vfgetfloatattr
#define	VF_GETSTRINGATTR 	vfgetstringattr
#define	VF_INTVALUE 		vfintvalue
#define	VF_FLOATVALUE 		vffloatvalue
#define	VF_STRINGVALUE 		vfstringvalue
#define	VF_GETDATASET 		vfgetdataset
#define VF_SETINTATTR 		vfsetintattr
#define VF_SETFLOATATTR 	vfsetfloatattr
#define VF_SETSTRINGATTR 	vfsetstringattr

#else
#define VF_CONECALLTOFILE 	vfconetofile_
#define VF_SIAPCALLTOFILE 	vfsiaptofile_
#define VF_SSAPCALLTOFILE 	vfssaptofile_
#define	VF_GETRAWURL 		vfgetrawurl_

#define	VF_OPENCONNECTION 	vfopenconnection_
#define	VF_OPENCONECONNECTION 	vfopenconeconnection_
#define	VF_OPENSIAPCONNECTION 	vfopensiapconnection_
#define	VF_OPENSSAPCONNECTION 	vfopenssapconnection_
#define	VF_CLOSECONNECTION 	vfcloseconnection_
#define	VF_GETSERVICECOUNT 	vfgetservicecount_
#define	VF_ADDSERVICEURL 	vfaddserviceurl_
#define	VF_GETSERVICEURL 	vfgetserviceurl_
#define	VF_GETQUERY 		vfgetquery_
#define	VF_GETCONEQUERY 	vfgetconequery_
#define	VF_GETSIAPQUERY 	vfgetsiapquery_
#define	VF_GETSSAPQUERY 	vfgetssapquery_
#define	VF_ADDINTPARAM 		vfaddintparam_
#define	VF_ADDFLOATPARAM 	vfaddfloatparam_
#define	VF_ADDSTRINGPARAM 	vfaddstringparam_
#define	VF_GETQUERYSTRING 	vfgetquerystring_
#define	VF_EXECUTEQUERY 	vfexecquery_
#define	VF_GETQUERYRESPONSE 	vfgetqr_
#define	VF_EXECUTECSV 		vfexeccsv_
#define	VF_EXECUTETSV 		vfexectsv_
#define	VF_EXECUTEASCII 	vfexecascii_
#define	VF_EXECUTEVOTABLE 	vfexecvotable_
#define	VF_EXECUTEQUERYAS 	vfexecqueryas_
#define	VF_GETRECORDCOUNT 	vfgetrecordcount_
#define	VF_GETRECORD 		vfgetrecord_
#define	VF_GETFIELDATTR 	vfgetfieldattr_
#define	VF_GETATTRCOUNT 	vfgetattrcount_
#define	VF_GETATTRLIST 		vfgetattrlist_
#define	VF_GETATTRIBUTE 	vfgetattribute_
#define	VF_GETINTATTR 		vfgetintattr_
#define	VF_GETFLOATATTR 	vfgetfloatattr_
#define	VF_GETSTRINGATTR 	vfgetstringattr_
#define	VF_INTVALUE 		vfintvalue_
#define	VF_FLOATVALUE 		vffloatvalue_
#define	VF_STRINGVALUE 		vfstringvalue_
#define	VF_GETDATASET 		vfgetdataset_
#define VF_SETINTATTR 		vfsetintattr_
#define VF_SETFLOATATTR 	vfsetfloatattr_
#define VF_SETSTRINGATTR 	vfsetstringattr_

#endif


/*  Function prototypes.
*/
void VF_CONECALLTOFILE (char *url, double *ra, double *dec, double *sr, 
	int *otype, char *file, int *ier, int ulen, int flen);
void VF_SIAPCALLTOFILE (char *url, double *ra, double *dec, double *rsize, 
	double *dsize, char *fmt, int *otype, char *file, int *ier,
	int ulen, int fmtlen, int fillen);
void VF_SSAPCALLTOFILE (char *url, double *ra, double *dec, double *size, 
	char *band, char *time, char *fmt, int *otype, char *file, int *ier,
	int ulen, int blen, int tlen, int fmtlen, int fillen);
void VF_GETRAWURL (char *url,  char *result, int *len, int *ier, 
	int ulen, int rlen);

void VF_OPENCONNECTION (char *service_url, int *type, DAL *dal, int *ier, 
	int len);
void VF_OPENCONECONNECTION (char *service_url, DAL *cone, int *ier, int len);
void VF_OPENSIAPCONNECTION (char *service_url, DAL *siap, int *ier, int len);
void VF_OPENSSAPCONNECTION (char *service_url, DAL *ssap, int *ier, int len);
void VF_CLOSECONNECTION (DAL *dal, int *ier);
void VF_GETSERVICECOUNT (DAL *dal, int *count);
void VF_ADDSERVICEURL (DAL *dal, char *service_url, int *ier, int len);
void VF_GETSERVICEURL (DAL *dal, int *index, char *url, int *len, int slen);
void VF_GETQUERY (DAL *dal, int *type, Query *query);
void VF_GETCONEQUERY (DAL *dal, double *ra, double *dec, double *sr, 
	Query *query);
void VF_GETSIAPQUERY (DAL *dal, double *ra, double *dec, double *ra_size, 
	double *dec_size, char *format, Query *query, int len);
void VF_GETSSAPQUERY (DAL *dal, double *ra, double *dec, double *size, 
	char *band, char *time, char *format, Query *query, 
	int blen, int tlen, int flen);
void VF_ADDINTPARAM (Query *query, char *name, int *ival, int *ier, int len);
void VF_ADDFLOATPARAM (Query *query, char *name, double *dval, int *ier, 
	int len);
void VF_ADDSTRINGPARAM (Query *query, char *name, char *str, int *ier, 
	int nlen, int slen);
void VF_GETQUERYSTRING (Query *query, char *type, int *index, char *qstring, 
	int *len, int tlen, int qlen);
void VF_EXECUTEQUERY (Query *query, QResponse *qr);
void VF_GETQUERYRESPONSE (Query *query, QResponse *qr);
void VF_EXECUTECSV (Query *query, char *csv, int *len, int clen);
void VF_EXECUTETSV (Query *query, char *tsv, int *len, int tlen);
void VF_EXECUTEASCII (Query *query, char *ascii, int *len, int alen);
void VF_EXECUTEVOTABLE (Query *query, char *votable, int *len, int vlen);
void VF_EXECUTEQUERYAS (Query *query, char *fname, int *type, int *ier, 
	int len);
void VF_GETRECORDCOUNT (QResponse *qr, int *count);
void VF_GETRECORD (QResponse *qr, int *recnum, QRecord *rec);
void VF_GETFIELDATTR (QResponse *qr, int *fieldnum, char *attr, char *aval,
			int *len, int alen);
void VF_GETATTRCOUNT (QRecord *rec, int *count);
void VF_GETATTRLIST (QRecord *rec, char *alist, int *len, int alen);
void VF_GETATTRIBUTE (QRecord *rec, char *attrname, QRAttribute *attr, int len);
void VF_GETINTATTR (QRecord *rec, char *attrname, int *ival, int len);
void VF_GETFLOATATTR (QRecord *rec, char *attrname, double *dval, int len);
void VF_GETSTRINGATTR (QRecord *rec, char *attrname, char *str, int *len, 
	int alen, int slen);
void VF_INTVALUE (QRAttribute *v, int *ival);
void VF_FLOATVALUE (QRAttribute *v, double *dval);
void VF_STRINGVALUE (QRAttribute *v, char *str, int *len, int slen);
void VF_GETDATASET (QRecord *rec, char *acref, char *fname, int *ier, 
	int alen, int flen);
void VF_SETINTATTR (QRecord *rec, char *attrname, int *ival, int alen);
void VF_SETFLOATATTR (QRecord *rec, char *attrname, double *dval, int alen);
void VF_SETSTRINGATTR (QRecord *rec, char *attrname, char *str, 
	int alen, int slen);


/** Private interface declarations.
**/
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);
extern int   typecode (char *typestr);

extern void voc_debugLevel ();



/******************************************************************************
**  CONECALLTOFILE -- Simple all-in-one interface to call a Cone service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
void
VF_CONECALLTOFILE (char *url, double *ra, double *dec, double *sr, int *otype, 
	char *file, int *ier, int ulen, int flen)
{
    char *_url  = sstrip (url, ulen);
    char *_file = sstrip (file, flen);

    *ier = voc_coneCallerToFile (_url, *ra, *dec, *sr, *otype, _file);

    free ((char *) _url);
    free ((char *) _file);
}



/******************************************************************************
**  SIAPCALLTOFILE -- Simple all-in-one interface to call a Siap service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
void
VF_SIAPCALLTOFILE (char *url, double *ra, double *dec, double *rsize, 
	double *dsize, char *fmt, int *otype, char *file, int *ier,
	int ulen, int fmtlen, int fillen)
{
    char *_url  = sstrip (url, ulen);
    char *_fmt  = sstrip (fmt, fmtlen);
    char *_file = sstrip (file, fillen);

    *ier = voc_siapCallerToFile (_url, *ra, *dec, *rsize, *dsize, _fmt, 
	*otype, _file);

    free ((char *) _url);
    free ((char *) _fmt);
    free ((char *) _file);
}


/******************************************************************************
**  SSAPCALLTOFILE -- Simple all-in-one interface to call a SSAP service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
void
VF_SSAPCALLTOFILE (char *url, double *ra, double *dec, double *size, 
	char *band, char *tim, char *fmt, int *otype, char *file, int *ier,
	int ulen, int blen, int tlen, int fmtlen, int fillen)
{
    char *_url  = sstrip (url, ulen);
    char *_band = sstrip (band, blen);
    char *_tim  = sstrip (tim, tlen);
    char *_fmt  = sstrip (fmt, fmtlen);
    char *_file = sstrip (file, fillen);

    *ier = voc_ssapCallerToFile (_url, *ra, *dec, *size, _band, _tim, _fmt, 
	*otype, _file);

    free ((char *) _url);
    free ((char *) _band);
    free ((char *) _tim);
    free ((char *) _fmt);
    free ((char *) _file);
}


/******************************************************************************
**  GETRAWURL -- Get the raw contents of a URL to a string.
*/
void
VF_GETRAWURL (char *url, char *result, int *len, int *ier, int ulen, int rlen)
{
    char *_url  = sstrip (url, ulen);
    int  nbytes;
    char *res  = voc_getRawURL (_url, &nbytes);

    memset (result, 0, rlen);
    if ((*len = strlen(res)) > rlen)
	fprintf (stderr, "Warning: truncating query string: len=%d maxch=%d\n",
            *len, rlen);
    spad (strncpy (result, res, *len), rlen);

    free ((char *) res);
    free ((char *) _url);
}




/******************************************************************************
**  OPENCONNECTION --  Open a new DAL context connection.
*/
void
VF_OPENCONNECTION (char *service_url, int *type, DAL *dal, int *ier, int len)
{
    char *_service_url = sstrip (service_url, len);

    *dal = voc_openConnection (_service_url, *type);
    *ier = (*dal ? OK : ERR);

    free ((char *) _service_url);
}


/******************************************************************************
**  Utility aliases for code readability.
*/
void
VF_OPENCONECONNECTION (char *service_url, DAL *cone, int *ier, int len)
{
    char *_service_url = sstrip (service_url, len);

    *cone = voc_openConeConnection (_service_url);
    *ier = (*cone ? OK : ERR);

    free ((char *) _service_url);
}

void
VF_OPENSIAPCONNECTION (char *service_url, DAL *siap, int *ier, int len)
{
    char *_service_url = sstrip (service_url, len);

    *siap = voc_openSiapConnection (_service_url);
    *ier = (*siap ? OK : ERR);

    free ((char *) _service_url);
}

void
VF_OPENSSAPCONNECTION (char *service_url, DAL *ssap, int *ier, int len)
{
    char *_service_url = sstrip (service_url, len);

    *ssap = voc_openSsapConnection (_service_url);
    *ier = (*ssap ? OK : ERR);

    free ((char *) _service_url);
}




/******************************************************************************
**  CLOSECONNETION --  Close the requested connection, i.e. tell the server
**  we no longer need this handle.
*/
void
VF_CLOSECONNECTION (DAL *dal, int *ier)
{
    voc_closeConnection (*dal);
    *ier = OK;
}


/******************************************************************************
**  GETSERVICECOUNT --  Get a count of the number of services associated 
**  with the given DAL connection context.
*/
void
VF_GETSERVICECOUNT (DAL *dal, int *count)
{
    *count = voc_getServiceCount (*dal);
}


/******************************************************************************
**  ADDSERVICEURL --  Add a service URL to the specified connection.
*/
void
VF_ADDSERVICEURL (DAL *dal, char *service_url, int *ier, int len)
{
    char *_service_url = sstrip (service_url, len);

    voc_addServiceURL (*dal, _service_url);
    *ier = OK;

    free ((char *) _service_url);
}


/******************************************************************************
**  GETSERVICEURL --  Get the requested service URL for the connection.
**  For Fortran we adjust the 'index' so the call can be 1-indexed.
*/
void
VF_GETSERVICEURL (DAL *dal, int *index, char *url, int *len, int ulen)
{
    char *res = voc_getServiceURL (*dal, (*index - 1));

    memset (url, 0, ulen);
    if ((*len = strlen(res)) > ulen)
	fprintf (stderr, "Warning: truncating URL string: len=%d maxch=%d\n",
            *len, ulen);
    spad (strncpy (url, res, *len), ulen);

    free ((char *) res);
}




/******************************************************************************
**  GETQUERY --  Get a generic Query context from the server.
*/
void
VF_GETQUERY (DAL *dal, int *type, Query *query)
{
    *query = voc_getQuery (*dal, *type);
}


/******************************************************************************
**  GETCONEQUERY --  Get a query for a Cone service.  We take the typical
**  Cone arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
void
VF_GETCONEQUERY (DAL *dal, double *ra, double *dec, double *sr, Query *query)
{
    *query = voc_getConeQuery (*dal, *ra, *dec, *sr);
}


/******************************************************************************
**  GETSIAPQUERY --  Get a query for a SIAP service.  We take the typical
**  SIAP arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
void
VF_GETSIAPQUERY (DAL *dal, double *ra, double *dec, double *ra_size, 
	double *dec_size, char *format, Query *query, int len)
{
    char *_format = sstrip (format,len);

    *query = voc_getSiapQuery (*dal, *ra, *dec, *ra_size, *dec_size, _format);

    free ((char *) _format);
}


/******************************************************************************
**  GETSSAPQUERY --  Get a query for a SSAP service.  We take the typical
**  SSAP arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
void
VF_GETSSAPQUERY (DAL *dal, double *ra, double *dec, double *size, 
	char *band, char *tim, char *format, Query *query, 
	int blen, int tlen, int flen)
{
    char *_band = sstrip (band,blen);
    char *_tim  = sstrip (tim,tlen);
    char *_format = sstrip (format,flen);

    *query = voc_getSsapQuery (*dal, *ra, *dec, *size, _band, _tim, _format);

    free ((char *) _format);
}


/******************************************************************************
** ADDPARAM --  Add a parameter to a Query string.
*/
void
VF_ADDINTPARAM (Query *query, char *name, int *ival, int *ier, int len)
{
    char *_name = sstrip (name, len);

    *ier = voc_addIntParam (*query, _name, *ival);

    free ((char *) _name);
}
              
void
VF_ADDFLOATPARAM (Query *query, char *name, double *dval, int *ier, int len)
{
    char *_name = sstrip (name, len);

    *ier = voc_addFloatParam (*query, _name, *dval);

    free ((char *) _name);
}
             
void
VF_ADDSTRINGPARAM (Query *query, char *name, char *str, int *ier, 
    int nlen, int slen)
{
    char *_name = sstrip (name, nlen);
    char *_str  = sstrip (str,  slen);

    *ier = voc_addStringParam (*query, _name, _str);

    free ((char *) _str);
    free ((char *) _name);
}



/******************************************************************************
**  GETQUERYSTRING -- Get the complete query string that will be executed.
**  For Fortran we adjust the 'index' so the call can be 1-indexed.
*/
void
VF_GETQUERYSTRING (Query *query, char *type, int *index, char *qstring, 
    int *len, int tlen, int qlen)
{
    char *_type  = sstrip (type, tlen);
    int   code = typecode (_type);

    char *res  = voc_getQueryString (*query, code, (*index - 1));

    memset (qstring, 0, qlen);
    if ((*len = strlen(res)) > qlen)
	fprintf (stderr, "Warning: truncating query string: len=%d maxch=%d\n",
            *len, qlen);
    spad (strncpy (qstring, res, *len), qlen);

    free ((char *) res);
    free ((char *) _type);
}


/******************************************************************************
**  EXECUTEQUERY --  Execute the specified query in the DAL server, return
**  the QResponse object handle.
*/
void
VF_EXECUTEQUERY (Query *query, QResponse *qr)
{
    *qr = voc_executeQuery (*query);
}


void
VF_GETQUERYRESPONSE (Query *query, QResponse *qr)
{
    *qr = voc_getQueryResponse (*query);
}


void
VF_EXECUTECSV (Query *query, char *csv, int *len, int clen)
{
    char *res = voc_executeCSV (*query);

    memset (csv, 0, clen);
    if ((*len = strlen(res)) > clen)
	fprintf (stderr, "Warning: truncating CSV: len=%d maxch=%d\n",
            *len, clen);
    spad (strncpy (csv, res, *len), clen);

    free ((char *) res);
}

void
VF_EXECUTETSV (Query *query, char *tsv, int *len, int tlen)
{
    char *res = voc_executeTSV (*query);

    memset (tsv, 0, tlen);
    if ((*len = strlen(res)) > tlen)
	fprintf (stderr, "Warning: truncating TSV: len=%d maxch=%d\n",
            *len, tlen);
    spad (strncpy (tsv, res, *len), tlen);

    free ((char *) res);
}

void
VF_EXECUTEASCII (Query *query, char *ascii, int *len, int alen)
{
    char *res = voc_executeASCII (*query);

    memset (ascii, 0, alen);
    if ((*len = strlen(res)) > alen)
	fprintf (stderr, "Warning: truncating CSV: len=%d maxch=%d\n",
            *len, alen);
    spad (strncpy (ascii, res, *len), alen);

    free ((char *) res);
}


void
VF_EXECUTEVOTABLE (Query *query, char *votable, int *len, int vlen)
{
    char *res = voc_executeVOTable (*query);

    memset (votable, 0, vlen);
    if ((*len = strlen(res)) > vlen)
	fprintf (stderr, "Warning: truncating VOTable: len=%d maxch=%d\n",
            *len, vlen);
    spad (strncpy (votable, res, *len), vlen);

    free ((char *) res);
}

             

/******************************************************************************
**  EXECUTEQUERYAS --  Execute the specified query in the DAL server,
**  saving the results in the specified format (e.g. CSV or VOTable) in
**  the specified file.
*/
void
VF_EXECUTEQUERYAS (Query *query, char *fname, int *type, int *ier, int len)
{
    char *_fname = sstrip (fname,len);

    *ier = voc_executeQueryAs (*query, _fname, *type);

    free ((char *) _fname);
}


/*  GETRECORDCOUNT --  Get a count of the records returned by the QResponse.
*/
void
VF_GETRECORDCOUNT (QResponse *qr, int *count)
{
    *count = voc_getRecordCount (*qr);
}


/***************************************************************************
** Access by dataset attribute:
**
**  For Fortran we adjust the 'recnum' so the call can be 1-indexed.
*/

void
VF_GETRECORD (QResponse *qr, int *recnum, QRecord *rec)
{
    *rec = voc_getRecord (*qr, (*recnum - 1));
}


void 
VF_GETFIELDATTR (QResponse *qr, int *fieldnum, char *attr, char *aval,
			int *len, int alen)
{
    char *_attrname = sstrip (attr, alen);
    char *res = voc_getFieldAttr (*qr, (*fieldnum - 1), _attrname);

    memset (aval, 0, alen);
    if ((*len = strlen(res)) > alen)
	fprintf (stderr, 
	    "Warning: truncating string attr '%s': len=%d maxch=%d\n",
	    _attrname, *len, alen);
    spad (strncpy (aval, res, *len), alen);

    free ((char *) res);
    free ((char *) _attrname);
}


void
VF_GETATTRCOUNT (QRecord *rec, int *count)
{
    *count = voc_getAttrCount (*rec);
}


void
VF_GETATTRLIST (QRecord *rec, char *alist, int *len, int alen)
{
    char *res = voc_getAttrList (*rec);

    memset (alist, 0, alen);
    if ((*len = strlen(res)) > alen)
	fprintf (stderr, "Warning: truncating attrList: len=%d maxch=%d\n",
            *len, alen);
    spad (strncpy (alist, res, *len), alen);

    free ((char *) res);
}



/***************************************************************************
**  Dataset Attribute Methods:
*/

void
VF_GETATTRIBUTE (QRecord *rec, char *attrname, QRAttribute *attr, int len)
{
    char *_attrname = sstrip (attrname,len);

    *attr = voc_getAttribute (*rec, _attrname);

    free ((char *) _attrname);
}


/*********************************************
****    NOT YET IMPLEMENTED IN DAL CLIENT
***/

void 
VF_SETINTATTR (QRecord *rec, char *attrname, int *ival, int alen) 
{
    char *_attrname = sstrip (attrname, alen);

    voc_setIntAttr (*rec, _attrname, *ival);

    free ((char *) _attrname);
}

void 
VF_SETFLOATATTR (QRecord *rec, char *attrname, double *dval, int alen) 
{
    char *_attrname = sstrip (attrname, alen);

    voc_setFloatAttr (*rec, _attrname, *dval) ;

    free ((char *) _attrname);
}

void 
VF_SETSTRINGATTR (QRecord *rec, char *attrname, char *str, int alen, int slen) 
{
    char *_attrname = sstrip (attrname, alen);
    char *s = sstrip (str, slen);

    voc_setStringAttr (*rec, _attrname, s); 

    free ((char *) _attrname);
    free ((char *) s);
}



/******************************************************************************
** Utility aliases for the messaging commands below.
*/
void
VF_GETINTATTR (QRecord *rec, char *attrname, int *ival, int len)
{
    char *_attrname = sstrip (attrname, len);

    *ival = voc_getIntAttr (*rec, _attrname);

    free ((char *) _attrname);
}

void
VF_GETFLOATATTR (QRecord *rec, char *attrname, double *dval, int len)
{
    char *_attrname = sstrip (attrname, len);

   *dval = voc_getFloatAttr (*rec, _attrname);

    free ((char *) _attrname);
}

void
VF_GETSTRINGATTR (QRecord *rec, char *attrname, char *str, int *len, 
    int alen, int slen)
{
    char *_attrname = sstrip (attrname, alen);
    char *res = voc_getStringAttr (*rec, _attrname);

    memset (str, 0, slen);
    if ((*len = strlen(res)) > slen)
	fprintf (stderr, 
	    "Warning: truncating string attr '%s': len=%d maxch=%d\n",
	    _attrname, *len, slen);
    spad (strncpy (str, res, *len), slen);

    free ((char *) res);
    free ((char *) _attrname);
}



/******************************************************************************
**  Value Conversion routines.
*/

void
VF_INTVALUE (QRAttribute *v, int *ival)
{
    *ival = voc_intValue (*v);
}

void
VF_FLOATVALUE (QRAttribute *v, double *dval)
{
    *dval = voc_floatValue (*v);
}

void
VF_STRINGVALUE (QRAttribute *v, char *str, int *len, int slen)
{
    char *res = voc_stringValue (*v);

    memset (str, 0, slen);
    if ((*len = strlen(res)) > slen)
	fprintf (stderr, "Warning: truncating string value: len=%d maxch=%d\n",
            *len, slen);
    spad (strncpy (str, res, *len), slen);

    free ((char *) res);
}


/******************************************************************************
**  GETDATASET -- Download the AccessReference dataset object to the named 
**  file.  If fname is NULL, a temp file will be created and the fname
**  pointer allocated with a string containing the name.
*/
void
VF_GETDATASET (QRecord *rec, char *acref, char *fname, int *ier, 
    int alen, int flen)
{
    char *_acref = sstrip (acref, alen);
    char *_fname = sstrip (fname, flen);

    *ier = voc_getDataset (*rec, _acref, _fname);

    free ((char *) _fname);
    free ((char *) _acref);
}
