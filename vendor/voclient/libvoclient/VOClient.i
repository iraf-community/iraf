/* VOClient.i  -- SWIG Interface definition file.
 */
%module voclient
%{
typedef int   	   ObjectID;
typedef int   	   DAL;
typedef int   	   Query;
typedef int   	   QResponse;
typedef int   	   QRecord;
typedef int   	   QRAttribute;

extern char       *voc_coneCaller (char *url, double ra, double dec, double sr, 
			int otype);
extern int         voc_coneCallerToFile (char *url, double ra, double dec, 
			double sr, int otype, char *file);

extern char       *voc_siapCaller (char *url, double ra, double dec, 
			double rsize, double dsize, char *fmt, int otype);
extern int         voc_siapCallerToFile (char *url, double ra, double dec,
			double rsize, double dsize, char *fmt, int otype, 
			char *file);
extern char       *voc_ssapCaller (char *url, double ra, double dec, 
			double size, char *band, char *time, char *fmt);
extern int         voc_ssapCallerToFile (char *url, double ra, double dec,
			double size, char *band, char *time, char *fmt,
			char *file);
extern char       *voc_getRawURL (char *url, int *nbytes);

extern int         voc_validateObject (int hcode);



extern int         voc_initVOClient (char *opts);
extern void        voc_closeVOClient (int shutdown);
extern void        voc_abortVOClient (int code, char *msg);
extern DAL         voc_openConnection (char *service_url, int type);
extern DAL         voc_openConeConnection (char *service_url);
extern DAL         voc_openSiapConnection (char *service_url);
extern DAL         voc_openSsapConnection (char *service_url);
extern void        voc_closeConnection (DAL dal);
extern int         voc_getServiceCount (DAL dal);
extern void        voc_addServiceURL (DAL dal, char *service_url);
extern char       *voc_getServiceURL (DAL dal, int index);
extern Query       voc_getQuery (DAL dal, int type);
extern Query       voc_getConeQuery (DAL dal, double ra, double dec, double sr);
extern Query       voc_getSiapQuery (DAL dal, double ra, double dec,
                         double ra_size, double dec_size, char *format);
extern Query       voc_getSsapQuery (DAL dal, double ra, double dec,
                         double size, char *band, char *time, char *format);
extern int         voc_addIntParam (Query query, char *name, int value);
extern int         voc_addFloatParam (Query query, char *name, double value);
extern int         voc_addStringParam (Query query, char *name, char *value);
extern char       *voc_getQueryString (Query query, int type, int index);
extern QResponse   voc_executeQuery (Query query);
extern QResponse   voc_getQueryResponse (Query query);
extern char       *voc_executeCSV (Query query);
extern char       *voc_executeVOTable (Query query);
extern int         voc_executeQueryAs (Query query, char *fname, int type);
extern int         voc_getRecordCount (QResponse qr);
extern QRecord     voc_getRecord (QResponse qr, int recnum);
extern char       *voc_getFieldAttr (QResponse qr, int fieldnum, char *attr);
extern QRAttribute voc_getAttribute (QRecord rec, char *attrname);
extern int         voc_intValue (QRAttribute v);
extern double      voc_floatValue (QRAttribute v);
extern char       *voc_stringValue (QRAttribute v);
extern int         voc_getIntAttr (QRecord rec, char *attr_name);
extern double      voc_getFloatAttr (QRecord rec, char *attr_name);
extern char       *voc_getStringAttr (QRecord rec, char *attr_name);
extern char       *voc_getAttrList (QRecord rec);
extern int         voc_getAttrCount (QRecord rec);
extern int         voc_getDataset (QRecord rec, char *acref, char *fname);
extern int         voc_debugLevel (int level);



typedef int   	   RegResult;
typedef int   	   RegQuery;


extern RegResult   voc_regSearch (char *term1,  char *term2, int orValues);
extern RegResult   voc_regSearchByService (char *svc, char *term, int orValues);
extern RegQuery    voc_regQuery (char *term, int orValues);
extern void        voc_regConstSvcType (RegQuery query, char *svc);
extern void        voc_regConstWaveband (RegQuery query, char *waveband);
extern void        voc_regDALOnly (RegQuery query, int value);
extern void        voc_regSortRes (RegQuery query, int value);
extern void        voc_regAddSearchTerm (RegQuery query, char *term, 
			int orValue);
extern void        voc_removeSearchTerm (RegQuery query, char *term);
extern int         voc_regGetSTCount (RegQuery query);
extern char       *voc_regGetQueryString (RegQuery query);
extern RegResult   voc_regExecute (RegQuery query);
extern char       *voc_regExecuteRaw (RegQuery query);
extern int         voc_resGetCount (RegResult res);
extern char       *voc_resGetStr (RegResult res, char *attribute, int index);
extern double      voc_resGetFloat (RegResult res, char *attribute, int index);
extern int         voc_resGetInt (RegResult res, char *attribute, int index);


typedef int   	   Sesame;

extern Sesame      voc_nameResolver (char *target);
extern char       *voc_resolverPos (Sesame sr);
extern double      voc_resolverRA (Sesame sr);
extern double      voc_resolverDEC (Sesame sr);
extern double      voc_resolverRAErr (Sesame sr);
extern double      voc_resolverDECErr (Sesame sr);
extern char       *voc_resolverOtype (Sesame sr);


typedef int   	   Skybot;

extern Skybot      voc_skybot (double ra, double dec, double rsz, double dsz,
                	double epoch);
extern int         voc_skybotNObjs (Skybot sb);
extern char       *voc_skybotStrAttr (Skybot sb, char *attr, int index);
extern double      voc_skybotDblAttr (Skybot sb, char *attr, int index);
%}



extern char       *voc_coneCaller (char *url, double ra, double dec, double sr, 
			int otype);
extern int         voc_coneCallerToFile (char *url, double ra, double dec, 
			double sr, int otype, char *file);

extern char       *voc_siapCaller (char *url, double ra, double dec, 
			double rsize, double dsize, char *fmt, int otype);
extern int         voc_siapCallerToFile (char *url, double ra, double dec,
			double rsize, double dsize, char *fmt, int otype, 
			char *file);
extern char       *voc_ssapCaller (char *url, double ra, double dec, 
			double size, char *band, char *time, char *fmt);
extern int         voc_ssapCallerToFile (char *url, double ra, double dec,
			double size, char *band, char *time, char *fmt,
			char *file);

extern char       *voc_getRawURL (char *url, int *nbytes);
extern int         voc_validateObject (int hcode);


extern int         voc_initVOClient (char *opts);
extern void        voc_closeVOClient (int shutdown);
extern void        voc_abortVOClient (int code, char *msg);
extern DAL         voc_openConnection (char *service_url, int type);
extern DAL         voc_openConeConnection (char *service_url);
extern DAL         voc_openSiapConnection (char *service_url);
extern DAL         voc_openSsapConnection (char *service_url);
extern void        voc_closeConnection (DAL dal);
extern int         voc_getServiceCount (DAL dal);
extern void        voc_addServiceURL (DAL dal, char *service_url);
extern char       *voc_getServiceURL (DAL dal, int index);
extern Query       voc_getQuery (DAL dal, int type);
extern Query       voc_getConeQuery (DAL dal, double ra, double dec, double sr);
extern Query       voc_getSiapQuery (DAL dal, double ra, double dec,
                         double ra_size, double dec_size, char *format);
extern Query       voc_getSsapQuery (DAL dal, double ra, double dec,
                         double size, char *band, char *time, char *format);
extern int         voc_addIntParam (Query query, char *name, int value);
extern int         voc_addFloatParam (Query query, char *name, double value);
extern int         voc_addStringParam (Query query, char *name, char *value);
extern char       *voc_getQueryString (Query query, int type, int index);
extern QResponse   voc_executeQuery (Query query);
extern char       *voc_executeCSV (Query query);
extern char       *voc_executeVOTable (Query query);
extern int         voc_executeQueryAs (Query query, char *fname, int type);
extern int         voc_getRecordCount (QResponse qr);
extern QRecord     voc_getRecord (QResponse qr, int recnum);
extern char       *voc_getFieldAttr (QResponse qr, int fieldnum, char *attr);
extern QRAttribute voc_getAttribute (QRecord rec, char *attrname);
extern int         voc_intValue (QRAttribute v);
extern double      voc_floatValue (QRAttribute v);
extern char       *voc_stringValue (QRAttribute v);
extern int         voc_getIntAttr (QRecord rec, char *attr_name);
extern double      voc_getFloatAttr (QRecord rec, char *attr_name);
extern char       *voc_getStringAttr (QRecord rec, char *attr_name);
extern char       *voc_getAttrList (QRecord rec);
extern int         voc_getAttrCount (QRecord rec);
extern int         voc_getDataset (QRecord rec, char *acref, char *fname);
extern int         voc_debugLevel (int level);


extern RegResult   voc_regSearch (char *term1,  char *term2, int orValues);
extern RegResult   voc_regSearchByService (char *svc, char *term, int orValues);
extern RegQuery    voc_regQuery (char *term, int orValues);
extern void        voc_regAddSearchTerm (RegQuery query, char *term, 
			int orValue);
extern void        voc_regRemoveSearchTerm (RegQuery query, char *term);
extern int         voc_regGetSTCount (RegQuery query);
extern char       *voc_regGetQueryString (RegQuery query);
extern RegResult   voc_regExecute (RegQuery query);
extern char        *voc_regExecuteRaw (RegQuery query);
extern int         voc_resGetCount (RegResult res);
extern char       *voc_resGetStr (RegResult res, char *attribute, int index);
extern double      voc_resGetFloat (RegResult res, char *attribute, int index);
extern int         voc_resGetInt (RegResult res, char *attribute, int index);


extern Sesame      voc_nameResolver (char *target);
extern char       *voc_resolverPos (Sesame sr);
extern double      voc_resolverRA (Sesame sr);
extern double      voc_resolverDEC (Sesame sr);
extern double      voc_resolverRAErr (Sesame sr);
extern double      voc_resolverDECErr (Sesame sr);
extern char       *voc_resolverOtype (Sesame sr);


extern Skybot      voc_skybot (double ra, double dec, double rsz, double dsz,
                	double epoch);
extern int         voc_skybotNObjs (Skybot sb);
extern char       *voc_skybotStrAttr (Skybot sb, char *attr, int index);
extern double      voc_skybotDblAttr (Skybot sb, char *attr, int index);
