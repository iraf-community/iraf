/***************************************************************************
**
**  SPP Language binding for the VOClient interface.
**
**  Michael Fitzpatrick, NOAO, Jul 2006
**
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/*  SPP Name mapping macros.  SPP procedure names are mappad as the first-5
**  plus the last character of a name minus any underscores.  This should
**  be done such that a unique 6-character name is produced for each SPP
**  symbol.  In these definitions the SPP code may use the long form of the
**  name in the code, the mapping is done automatically and so we need the
**  macros here so the symbol entered in the library is actually the short
**  name.
*/

#ifdef _NO_US_

#define	vx_conecaller 		vxconr
#define	vx_siapcaller 		vxsiar
#define	vx_ssapcaller 		vxssar
#define	vx_rawurl 		vxrawl
#define	vx_conetofile 		vxcone
#define	vx_siaptofile 		vxsiae
#define	vx_ssaptofile 		vxssae
#define	vx_dalconnection 	vxdaln
#define	vx_coneconnection 	vxconn
#define	vx_siapconnection 	vxsian
#define	vx_ssapconnection 	vxssan
#define	vx_closeconnection 	vxclon
#define	vx_servicecount 	vxsert
#define	vx_addserviceurl 	vxaddl
#define	vx_getserviceurl 	vxgetl
#define	vx_query 		vxquey
#define	vx_conequery 		vxcony
#define	vx_siapquery 		vxsiay
#define	vx_ssapquery 		vxssay
#define	vx_intparam 		vxintm
#define	vx_floatparam 		vxflom
#define	vx_stringparam 		vxstrm
#define	vx_querystring 		vxqueg
#define vx_executequery 	vxexey
#define vx_getqueryresp 	vxgetp
#define	vx_executecsv 		vxexev
#define	vx_executetab 		vxexeb
#define	vx_executeascii		vxexei
#define	vx_executevotable 	vxexee
#define	vx_executequeryas 	vxexes
#define	vx_recordcount 		vxrect
#define	vx_record 		vxrecd
#define	vx_fieldattr 		vxgfat
#define	vx_atcount 		vxatct
#define	vx_atlist 		vxatlt
#define vx_getattribute 	vxgete
#define	vx_sattri 		vxsati
#define	vx_sattrr  		vxsatr
#define	vx_sattrs 		vxsats
#define	vx_intattr 		vxintr
#define	vx_floatattr 		vxflor
#define	vx_stringattr 		vxstrr
#define	vx_intvalue 		vxinte
#define	vx_floatvalue 		vxfloe
#define	vx_stringvalue 		vxstre
#define	vx_getDataset 		vxgett

#else

#define	vx_conecaller 		vxconr_
#define	vx_siapcaller 		vxsiar_
#define	vx_ssapcaller 		vxssar_
#define	vx_rawurl 		vxrawl_
#define	vx_conetofile 		vxcone_
#define	vx_siaptofile 		vxsiae_
#define	vx_ssaptofile 		vxssae_
#define	vx_dalconnection 	vxdaln_
#define	vx_coneconnection 	vxconn_
#define	vx_siapconnection 	vxsian_
#define	vx_ssapconnection 	vxssan_
#define	vx_closeconnection 	vxclon_
#define	vx_servicecount 	vxsert_
#define	vx_addserviceurl 	vxaddl_
#define	vx_getserviceurl 	vxgetl_
#define	vx_query 		vxquey_
#define	vx_conequery 		vxcony_
#define	vx_siapquery 		vxsiay_
#define	vx_ssapquery 		vxssay_
#define	vx_intparam 		vxintm_
#define	vx_floatparam 		vxflom_
#define	vx_stringparam 		vxstrm_
#define	vx_querystring 		vxqueg_
#define vx_executequery 	vxexey_
#define vx_getqueryresp 	vxgetp_
#define	vx_executecsv 		vxexev_
#define	vx_executetab 		vxexeb_
#define	vx_executeascii		vxexei_
#define	vx_executevotable 	vxexee_
#define	vx_executequeryas 	vxexes_
#define	vx_recordcount 		vxrect_
#define	vx_record 		vxrecd_
#define	vx_fieldattr 		vxgfat_
#define	vx_atcount 		vxatct_
#define	vx_atlist 		vxatlt_
#define vx_getattribute 	vxgete_
#define	vx_sattri 		vxsati_
#define	vx_sattrr  		vxsatr_
#define	vx_sattrs 		vxsats_
#define	vx_intattr 		vxintr_
#define	vx_floatattr 		vxflor_
#define	vx_stringattr 		vxstrr_
#define	vx_intvalue 		vxinte_
#define	vx_floatvalue 		vxfloe_
#define	vx_stringvalue 		vxstre_
#define	vx_getDataset 		vxgett_


#endif



/* SPP Type definitions.
*/
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		NULL


/*  Public interface procedures.
**
*/
int	vx_conecaller (XCHAR *url, double *ra, double *dec, double *sr, 
	    XCHAR *otype, XCHAR *res, int *maxch);
int 	vx_siapcaller (XCHAR *url, double *ra, double *dec, double *rsize,
    	    double *dsize, XCHAR *fmt, XCHAR *type, XCHAR *result, int *maxch);
int	vx_siaptofile (XCHAR *url, double *ra, double *dec, double *rsize, 
	    double *dsize, XCHAR *fmt, int *otype, XCHAR *file);
int 	vx_ssapcaller (XCHAR *url, double *ra, double *dec, double *size,
    	    XCHAR *band, XCHAR *tim, XCHAR *fmt, XCHAR *type, XCHAR *result, 
	    int *maxch);
int	vx_ssaptofile (XCHAR *url, double *ra, double *dec, double *size, 
	    XCHAR *band, XCHAR *tim, XCHAR *fmt, int *otype, XCHAR *file);
int	vx_conetofile (XCHAR *url, double *ra, double *dec, double *sr, 
	    XCHAR *otype, XCHAR *file);
int	vx_rawurl (XCHAR *url, XCHAR *result, int *maxch);

DAL	vx_dalconnection (XCHAR *service_url, int *type);
DAL	vx_coneconnection (XCHAR *service_url);
DAL	vx_siapconnection (XCHAR *service_url);
DAL	vx_ssapconnection (XCHAR *service_url);
void	vx_closeconnection (DAL *dal);
int	vx_servicecount (DAL *dal);
void	vx_addserviceurl (DAL *dal, XCHAR *service_url);
void	vx_getserviceurl (DAL *dal, int *index, XCHAR *url, int *maxch, 
	    int *len);
Query	vx_query (DAL *dal, int *type);
Query	vx_conequery (DAL *dal, double *ra, double *dec, double *sr);
Query	vx_siapquery (DAL *dal, double *ra, double *dec, double *ra_size, 
	    double *dec_size, XCHAR *format);
Query	vx_ssapquery (DAL *dal, double *ra, double *dec, double *size, 
	    XCHAR *band, XCHAR *tim, XCHAR *format);
int	vx_intparam (Query *query, XCHAR *name, int *ival);
int	vx_floatparam (Query *query, XCHAR *name, double *dval);
int	vx_stringparam (Query *query, XCHAR *name, XCHAR *str);
int	vx_querystring (Query *query, XCHAR *type, int *index, XCHAR *qstring, 
    	    int *maxch);
QResponse vx_executequery (Query *query);
QResponse vx_getqueryresp (Query *query);
int	vx_executecsv (Query *query, XCHAR *csv, int *maxch);
int	vx_executetab (Query *query, XCHAR *tsv, int *maxch);
int	vx_executeascii (Query *query, XCHAR *ascii, int *maxch);
int	vx_executevotable (Query *query, XCHAR *votable, int *maxch);
int	vx_executequeryas (Query *query, XCHAR *fname, int *type);
int	vx_recordcount (QResponse *qr);
QRecord	vx_record (QResponse *qr, int *recnum);
int	vx_atcount (QRecord *rec);
int	vx_fieldattr (QResponse *qr, int *fieldnum, XCHAR *attr, 
		XCHAR *id, int *maxch);
int	vx_atlist (QRecord *rec, XCHAR *alist, int *maxch);

QRAttribute vx_getattribute (QRecord *rec, XCHAR *attrname);
void	vx_sattri (QRecord *rec, XCHAR *attrname, int *ival); 
void	vx_sattrr  (QRecord *rec, XCHAR *attrname, double *dval);
void	vx_sattrs (QRecord *rec, XCHAR *attrname, XCHAR *str);
int	vx_intattr (QRecord *rec, XCHAR *attrname);
double	vx_floatattr (QRecord *rec, XCHAR *attrname); 
int	vx_stringattr (QRecord *rec, XCHAR *attrname, XCHAR *attrval, 
	    int *maxch);

int	vx_intvalue (QRAttribute *v);
double	vx_floatvalue (QRAttribute *v);
int	vx_stringvalue (QRAttribute *v, XCHAR *val, int *maxch);

int	vx_getDataset (QRecord *rec, XCHAR *acref, XCHAR *fname); 



/*  Private interface procedures.
*/
extern PKCHAR *spp2c (XCHAR *instr,  int maxch);
extern int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
extern int    spplen (XCHAR *str);
extern int  dal_typecode (char *typestr);
extern int  out_typecode (char *typestr);





/******************************************************************************
***********         	    High Level Functions       		     **********
******************************************************************************/


/******************************************************************************
**  CONECALLER -- Simple all-in-one interface to call a Cone service and
**  return the result as a character string to the caller.
*/
int
vx_conecaller (XCHAR *url, double *ra, double *dec, double *sr, XCHAR *otype, 
    XCHAR *result, int *maxch)
{
    int  len = ERR;
    char *_otype = spp2c (otype, spplen (otype));
    char *_url = spp2c (url, spplen (url));

    char *_result = voc_coneCaller (_url, *ra, *dec, *sr, out_typecode(_otype));
    if (_result)
        len = c2spp (_result, result, *maxch);

    free ((char *) _otype);
    free ((char *) _url);
    if (_result) free ((char *) _result);

    return (len);
}


/******************************************************************************
**  SIAPCALLER -- Simple all-in-one interface to call a SIAP service and
**  return the result as a character string to the caller.
*/
int
vx_siapcaller (XCHAR *url, double *ra, double *dec, double *rsize,
    double *dsize, XCHAR *fmt, XCHAR *type, XCHAR *result, int *maxch)
{
    int  len = ERR;
    char *_url  = spp2c (url, spplen (url));
    char *_fmt  = spp2c (fmt, spplen (fmt));		/* image/fits, etc  */
    char *_type = spp2c (type, spplen (type));		/* csv or votable   */

    char *_result = voc_siapCaller (_url, *ra, *dec, *rsize, *dsize, _fmt, 
	out_typecode(_type));
    if (_result)
	 len = c2spp (_result, result, *maxch);

    free ((char *) _fmt);
    free ((char *) _type);
    free ((char *) _url);
    if (_result) free ((char *) _result);

    return (len);
}


/******************************************************************************
**  SSAPCALLER -- Simple all-in-one interface to call a SSAP service and
**  return the result as a character string to the caller.
*/
int
vx_ssapcaller (XCHAR *url, double *ra, double *dec, double *size,
    XCHAR *band, XCHAR *tim, XCHAR *fmt, XCHAR *type, XCHAR *result, int *maxch)
{
    int  len = ERR;
    char *_url  = spp2c (url, spplen (url));
    char *_band = spp2c (band, spplen (band));
    char *_tim  = spp2c (tim, spplen (tim));
    char *_fmt  = spp2c (fmt, spplen (fmt));		/* image/fits, etc  */
    char *_type = spp2c (type, spplen (type));		/* csv or votable   */

    char *_result = voc_ssapCaller (_url, *ra, *dec, *size, _band, _tim, _fmt, 
	out_typecode(_type));
    if (_result)
	 len = c2spp (_result, result, *maxch);

    free ((char *) _band);
    free ((char *) _tim);
    free ((char *) _fmt);
    free ((char *) _type);
    free ((char *) _url);
    if (_result) free ((char *) _result);

    return (len);
}


/******************************************************************************
**  RAWURL -- Get the contents of a raw URL to a string.
*/
int
vx_rawurl (XCHAR *url, XCHAR *result, int *maxch)
{
    int  nbytes, rlen;
    char *_url = spp2c (url, spplen (url));
    char *_result = voc_getRawURL (_url, &rlen);


    nbytes = c2spp (_result, result, *maxch);

    free ((char *) _url);
    free ((char *) _result);

    return (nbytes);
}



/******************************************************************************
**  CONETOFILE -- Simple all-in-one interface to call a Cone service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
int
vx_conetofile (XCHAR *url, double *ra, double *dec, double *sr, XCHAR *otype, 
    XCHAR *file)
{
    char *_otype = spp2c (otype, spplen (otype));
    char *_url = spp2c (url, spplen (url));
    char *_file = spp2c (file, spplen (file));

    int ier   = voc_coneCallerToFile (_url, *ra, *dec, *sr, 
	out_typecode(_otype), _file);

    free ((char *) _file);
    free ((char *) _url);
    free ((char *) _otype);

    return (ier);
}


/******************************************************************************
**  SIAPTOFILE -- Simple all-in-one interface to call a Siap service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
int
vx_siaptofile (XCHAR *url, double *ra, double *dec, double *rsize, 
	double *dsize, XCHAR *fmt, int *otype, XCHAR *file)
{
    char *_url  = spp2c (url, spplen (url));
    char *_file = spp2c (file, spplen (file));
    char *_fmt  = spp2c (fmt, spplen (fmt));

    int ier    = voc_siapCallerToFile (_url, *ra, *dec, *rsize, *dsize,
		    _fmt , *otype, _file);

    free ((char *) _file);
    free ((char *) _url);
    free ((char *) _fmt );
    return (ier);
}


/******************************************************************************
**  SSAPTOFILE -- Simple all-in-one interface to call a SSAP service and
**  save the resulting raw VOTable or a CSV to the named file.
*/
int
vx_ssaptofile (XCHAR *url, double *ra, double *dec, double *size, 
	XCHAR *band, XCHAR *tim, XCHAR *fmt, int *otype, XCHAR *file)
{
    char *_url  = spp2c (url, spplen (url));
    char *_band = spp2c (band, spplen (band));
    char *_tim  = spp2c (tim, spplen (tim));
    char *_file = spp2c (file, spplen (file));
    char *_fmt  = spp2c (fmt, spplen (fmt));

    int ier    = voc_ssapCallerToFile (_url, *ra, *dec, *size, _band, _tim,
		    _fmt , *otype, _file);

    free ((char *) _file);
    free ((char *) _url);
    free ((char *) _fmt );
    free ((char *) _tim );
    free ((char *) _band );
    return (ier);
}



/***************************************************************************
**  OPENCONNECTION --  Open a new DAL context connection.
*/
DAL
vx_dalconnection (XCHAR *service_url, int *type)
{
    char *_service_url = spp2c (service_url, spplen (service_url));
    int dal = voc_openConnection (_service_url, *type);

    free ((char *) _service_url);
    return (dal);
}


/*  Utility aliases for code readability.
*/
DAL 
vx_coneconnection (XCHAR *service_url) 
{
    char *_service_url = spp2c (service_url, spplen (service_url));
    int dal = voc_openConeConnection (_service_url); 

    free ((char *) _service_url);
    return (dal);
}

DAL 
vx_siapconnection (XCHAR *service_url) 
{
    char *_service_url = spp2c (service_url, spplen (service_url));
    int dal = voc_openSiapConnection (_service_url); 

    free ((char *) _service_url);
    return (dal);
}

DAL 
vx_ssapconnection (XCHAR *service_url) 
{
    char *_service_url = spp2c (service_url, spplen (service_url));
    int dal = voc_openSsapConnection (_service_url); 

    free ((char *) _service_url);
    return (dal);
}



/******************************************************************************
**  CLOSECONNETION --  Close the requested connection, i.e. tell the server
**  we no longer need this handle.
*/
void
vx_closeconnection (DAL *dal)
{
    voc_closeConnection (*dal);
}


/******************************************************************************
**  GETSERVICECOUNT --  Get a count of the number of services associated 
**  with the given DAL connection context.
*/
int
vx_servicecount (DAL *dal)
{
    return (voc_getServiceCount (*dal));
}


/******************************************************************************
**  ADDSERVICEURL --  Add a service URL to the specified connection.
*/
void
vx_addserviceurl (DAL *dal, XCHAR *service_url)
{
    char *_service_url = spp2c (service_url, spplen (service_url));

    voc_addServiceURL (*dal, _service_url);

    free ((char *) _service_url);
}


/******************************************************************************
**  GETSERVICEURL --  Get the requested service URL for the connection.
*/
void
vx_getserviceurl (DAL *dal, int *index, XCHAR *url, int *maxch, int *len)
{
    char *_url = voc_getServiceURL (*dal, *index);

    *len = c2spp (_url, url, *maxch);
    free ((char *) _url);
}


/******************************************************************************
**  GETQUERY --  Get a generic Query context from the server.
*/
Query
vx_query (DAL *dal, int *type)
{
    return ( voc_getQuery (*dal, *type) );
}


/******************************************************************************
**  GETCONEQUERY --  Get a query for a Cone service.  We take the typical
**  Cone arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
Query
vx_conequery (DAL *dal, double *ra, double *dec, double *sr)
{
    return ( voc_getConeQuery (*dal, *ra, *dec, *sr) );
}


/******************************************************************************
**  GETSIAPQUERY --  Get a query for a SIAP service.  We take the typical
**  SIAP arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
Query
vx_siapquery (DAL *dal, double *ra, double *dec, double *ra_size, 
	double *dec_size, XCHAR *format)
{
    char *_format = spp2c (format, spplen (format));
    Query siap = voc_getSiapQuery (*dal, *ra, *dec, *ra_size, *dec_size,
			_format);

    free ((char *) _format);
    return (siap);
}


/******************************************************************************
**  GETSIAPQUERY --  Get a query for a SIAP service.  We take the typical
**  SIAP arguments in the interface routine but break it down into a series
**  of messages to get a general Query object and then add parameters.
*/
Query
vx_ssapquery (DAL *dal, double *ra, double *dec, double *size, 
	XCHAR *band, XCHAR *tim, XCHAR *format)
{
    char *_band = spp2c (band, spplen (band));
    char *_tim = spp2c (tim, spplen (tim));
    char *_format = spp2c (format, spplen (format));

    Query ssap = voc_getSsapQuery(*dal, *ra, *dec, *size, _band, _tim, _format);

    free ((char *) _format);
    return (ssap);
}



/******************************************************************************
** ADDPARAM --  Add a parameter to a Query string.
*/
int
vx_intparam (Query *query, XCHAR *name, int *ival)
{
    char *_name = spp2c (name, spplen (name));

    int stat = voc_addIntParam (*query, _name, *ival);

    free ((char *) _name);
    return (stat);
}
              
int
vx_floatparam (Query *query, XCHAR *name, double *dval)
{
    char *_name = spp2c (name, spplen (name));

    int stat = voc_addFloatParam (*query, _name, *dval);

    free ((char *) _name);
    return (stat);
}
             
int
vx_stringparam (Query *query, XCHAR *name, XCHAR *str)
{
    char *_name = spp2c (name, spplen (name));
    char *_str = spp2c (str, spplen (str));

    int stat = voc_addStringParam (*query, _name, _str);

    free ((char *) _name);
    free ((char *) _str);
    return (stat);
}



       
/******************************************************************************
**  GETQUERYSTRING -- Get the complete query string that will be executed.
*/
int
vx_querystring (Query *query, XCHAR *type, int *index, XCHAR *qstring, 
    int *maxch)
{
    char *_type = spp2c (type, spplen (type));

    char *_qstring = voc_getQueryString (*query, dal_typecode(_type), *index);
    int len = c2spp (_qstring, qstring, *maxch);

    free ((char *) _type);
    free ((char *) _qstring);
    return (len);
}


/******************************************************************************
**  EXECUTEQUERY --  Execute the specified query in the DAL server, return
**  the QResponse object handle.
*/
QResponse
vx_executequery (Query *query)
{
    return ( voc_executeQuery (*query) );
}


QResponse
vx_getqueryresp (Query *query)
{
    return ( voc_getQueryResponse (*query) );
}


int
vx_executecsv (Query *query, XCHAR *csv, int *maxch)
{
    char *_result = voc_executeCSV (*query);
    int len = c2spp (_result, csv, *maxch);

    free ((char *) _result);
    return (len);
}


int
vx_executetab (Query *query, XCHAR *tsv, int *maxch)
{
    char *_result = voc_executeTSV (*query);
    int len = c2spp (_result, tsv, *maxch);

    free ((char *) _result);
    return (len);
}


int
vx_executeascii (Query *query, XCHAR *ascii, int *maxch)
{
    char *_result = voc_executeASCII (*query);
    int len = c2spp (_result, ascii, *maxch);

    free ((char *) _result);
    return (len);
}


int
vx_executevotable (Query *query, XCHAR *votable, int *maxch)
{
    char *_result = voc_executeVOTable (*query);
    int len = c2spp (_result, votable, *maxch);

    free ((char *) _result);
    return (len);
}

             
/******************************************************************************
**  EXECUTEQUERYAS --  Execute the specified query in the DAL server,
**  saving the results in the specified format (e.g. CSV or VOTable) in
**  the specified file.
*/
int
vx_executequeryas (Query *query, XCHAR *fname, int *type)
{
    char *_fname = spp2c (fname, spplen (fname));
    int stat = voc_executeQueryAs (*query, _fname, *type);

    free ((char *) _fname);
    return (stat);
}


/******************************************************************************
**  GETRECORDCOUNT --  Get a count of the records returned by the QResponse.
*/
int
vx_recordcount (QResponse *qr)
{
    return ( voc_getRecordCount (*qr) );
}


/***************************************************************************
** Access by dataset attribute:
*/

QRecord
vx_record (QResponse *qr, int *recnum)
{
    return ( voc_getRecord (*qr, *recnum) );
}


int
vx_atcount (QRecord *rec)
{
    return ( voc_getAttrCount (*rec) );
}


int
vx_fieldattr (QRecord *rec, int *fieldnum, XCHAR *attr, XCHAR *aval, int *maxch)
{
/*
    char *_attr = vot_getFieldID (*rec, *fieldnum, attr);
*/
    char *_attr = "";
    int len = c2spp (_attr, attr, *maxch);

    free ((char *) _attr);
    return (len);
}


int
vx_atlist (QRecord *rec, XCHAR *alist, int *maxch)
{
    char *_list = voc_getAttrList (*rec);
    int len = c2spp (_list, alist, *maxch);

    free ((char *) _list);
    return (len);
}



/***************************************************************************
**  Dataset Attribute Methods:
*/

QRAttribute
vx_getattribute (QRecord *rec, XCHAR *attrname)
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    QRAttribute v = voc_getAttribute (*rec, _attrname);

    free ((char *) _attrname);
    return (v);
}


/***
****    NOT YET IMPLEMENTED IN DAL CLIENT CLASSES.
***/

void 
vx_sattri (QRecord *rec, XCHAR *attrname, int *ival) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    voc_setIntAttr (*rec, _attrname, *ival);

    free ((char *) _attrname);
}

void 
vx_sattrr  (QRecord *rec, XCHAR *attrname, double *dval) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    voc_setFloatAttr (*rec, _attrname, *dval);

    free ((char *) _attrname);
}

void 
vx_sattrs (QRecord *rec, XCHAR *attrname, XCHAR *str) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));
    char *_str = spp2c (str, spplen (str));

    voc_setStringAttr (*rec, _attrname, _str);

    free ((char *) _attrname);
    free ((char *) _str);
}




/***************************************************************************
** Utility aliases for the messaging commands below.
*/
int 
vx_intattr (QRecord *rec, XCHAR *attrname) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    int ival = voc_getIntAttr (*rec, _attrname);

    free ((char *) _attrname);
    return (ival);
}


double 
vx_floatattr (QRecord *rec, XCHAR *attrname) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    double dval = voc_getFloatAttr (*rec, _attrname);

    free ((char *) _attrname);
    return (dval);
}


int
vx_stringattr (QRecord *rec, XCHAR *attrname, XCHAR *attrval, int *maxch) 
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    char *_result = voc_getStringAttr (*rec, _attrname);
    int len = c2spp (_result, attrval, *maxch);

    free ((char *) _result);
    free ((char *) _attrname);
    return (len);
}



/***************************************************************************
**  Return attribute as given type.
*/

int
vx_intvalue (QRAttribute *v)
{
    return ( voc_intValue (*v) );
}

double
vx_floatvalue (QRAttribute *v)
{
    return ( voc_floatValue (*v) );
}

int
vx_stringvalue (QRAttribute *v, XCHAR *val, int *maxch)
{
    char *_result = voc_stringValue (*v);
    int len = c2spp (_result, val, *maxch);

    free ((char *) _result);

    return (len);
}


/***************************************************************************
**  GETDATASET -- Download the AccessReference dataset object to the named 
**  file.  If fname is NULL, a temp file will be created and the fname
**  pointer allocated with a string containing the name.
*/
int 
vx_getDataset (QRecord *rec, XCHAR *acref, XCHAR *fname) 
{
    char *_acref = spp2c (acref, spplen (acref));
    char *_fname = spp2c (fname, spplen (fname));

    int stat = voc_getDataset (*rec, _acref, _fname); 

    free ((char *) _acref);
    free ((char *) _fname);

    return (stat);
}
