/**
 *  VOClient.i  -- SWIG Interface definition file.
 */


/*  vocDAL.c
 */
typedef int   	   ObjectID;
typedef int   	   DAL;
typedef int   	   Query;
typedef int   	   QResponse;
typedef int   	   QRecord;
typedef int   	   QRAttribute;

char       *voc_coneCaller (char *url, double ra, double dec, double sr, 
			int otype);
int         voc_coneCallerToFile (char *url, double ra, double dec, 
			double sr, int otype, char *file);

char       *voc_siapCaller (char *url, double ra, double dec, 
			double rsize, double dsize, char *fmt, int otype);
int         voc_siapCallerToFile (char *url, double ra, double dec,
			double rsize, double dsize, char *fmt, int otype, 
			char *file);
char       *voc_ssapCaller (char *url, double ra, double dec, 
			double size, char *band, char *time, char *fmt);
int         voc_ssapCallerToFile (char *url, double ra, double dec,
			double size, char *band, char *time, char *fmt,
			char *file);
char       *voc_getRawURL (char *url, int *nbytes);

int         voc_validateObject (int hcode);



/*  vocLIB.c
 */
int         voc_initVOClient (char *opts);
void        voc_closeVOClient (int shutdown);
void        voc_abortVOClient (int code, char *msg);
DAL         voc_openConnection (char *service_url, int type);
DAL         voc_openConeConnection (char *service_url);
DAL         voc_openSiapConnection (char *service_url);
DAL         voc_openSsapConnection (char *service_url);
void        voc_closeConnection (DAL dal);
int         voc_getServiceCount (DAL dal);
void        voc_addServiceURL (DAL dal, char *service_url);
char       *voc_getServiceURL (DAL dal, int index);
Query       voc_getQuery (DAL dal, int type);
Query       voc_getConeQuery (DAL dal, double ra, double dec, double sr);
Query       voc_getSiapQuery (DAL dal, double ra, double dec,
                         double ra_size, double dec_size, char *format);
Query       voc_getSsapQuery (DAL dal, double ra, double dec,
                         double size, char *band, char *time, char *format);
int         voc_addIntParam (Query query, char *name, int value);
int         voc_addFloatParam (Query query, char *name, double value);
int         voc_addStringParam (Query query, char *name, char *value);
char       *voc_getQueryString (Query query, int type, int index);
QResponse   voc_executeQuery (Query query);
QResponse   voc_getQueryResponse (Query query);
char       *voc_executeCSV (Query query);
char       *voc_executeVOTable (Query query);
int         voc_executeQueryAs (Query query, char *fname, int type);
int         voc_getRecordCount (QResponse qr);
QRecord     voc_getRecord (QResponse qr, int recnum);
char       *voc_getFieldAttr (QResponse qr, int fieldnum, char *attr);
QRAttribute voc_getAttribute (QRecord rec, char *attrname);
int         voc_intValue (QRAttribute v);
double      voc_floatValue (QRAttribute v);
char       *voc_stringValue (QRAttribute v);
int         voc_getIntAttr (QRecord rec, char *attr_name);
double      voc_getFloatAttr (QRecord rec, char *attr_name);
char       *voc_getStringAttr (QRecord rec, char *attr_name);
char       *voc_getAttrList (QRecord rec);
int         voc_getAttrCount (QRecord rec);
int         voc_getDataset (QRecord rec, char *acref, char *fname);
int         voc_debugLevel (int level);



/*  vocRegistry.c
 */
typedef int   	   RegResult;
typedef int   	   RegQuery;


RegResult   voc_regSearch (char *term1,  char *term2, int orValues);
RegResult   voc_regSearchByService (char *svc, char *term, int orValues);
RegQuery    voc_regQuery (char *term, int orValues);
void        voc_regConstSvcType (RegQuery query, char *svc);
void        voc_regConstWaveband (RegQuery query, char *waveband);
void        voc_regDALOnly (RegQuery query, int value);
void        voc_regSortRes (RegQuery query, int value);
void        voc_regAddSearchTerm (RegQuery query, char *term, 
			int orValue);
void        voc_removeSearchTerm (RegQuery query, char *term);
int         voc_regGetSTCount (RegQuery query);
char       *voc_regGetQueryString (RegQuery query);
RegResult   voc_regExecute (RegQuery query);
char       *voc_regExecuteRaw (RegQuery query);
int         voc_resGetCount (RegResult res);
char       *voc_resGetStr (RegResult res, char *attribute, int index);
double      voc_resGetFloat (RegResult res, char *attribute, int index);
int         voc_resGetInt (RegResult res, char *attribute, int index);


/*  vocSesame.c
 */
typedef int   	   Sesame;

Sesame      voc_nameResolver (char *target);
char       *voc_resolverPos (Sesame sr);
double      voc_resolverRA (Sesame sr);
double      voc_resolverDEC (Sesame sr);
double      voc_resolverRAErr (Sesame sr);
double      voc_resolverDECErr (Sesame sr);
char       *voc_resolverOtype (Sesame sr);


/*  vocSkybot.c
 */
typedef int   	   Skybot;

Skybot      voc_skybot (double ra, double dec, double rsz, double dsz,
                	double epoch);
int         voc_skybotNObjs (Skybot sb);
char       *voc_skybotStrAttr (Skybot sb, char *attr, int index);
double      voc_skybotDblAttr (Skybot sb, char *attr, int index);



