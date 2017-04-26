/*  
**  XRPC.h -- Public include file for the XRPC interface.
*/


#ifdef  OK
#undef  OK
#endif
#define	OK		0

#ifdef  ERR
#undef  ERR
#endif
#define	ERR		1


#ifdef  TRUE
#undef  TRUE
#endif
#define TRUE            1

#ifdef  FALSE
#undef  FALSE
#endif
#define FALSE           0



/*****************************************************************************/
/****                       Function Prototypes                           ****/
/*****************************************************************************/

/*  xrArray.c
*/
int    xr_newArray (void);
void   xr_freeArray (int anum);
int    xr_arrayLen (int anum);

void   xr_setIntInArray (int anum, int value);
void   xr_setDoubleInArray (int anum, double value);
void   xr_setBoolInArray (int anum, int value);
void   xr_setStringInArray (int anum, char *value);
void   xr_setDatetimeInArray (int anum, char *value);
void   xr_setStructInArray (int anum, int value);
void   xr_setArrayInArray (int anum, int value);

void   xr_getIntFromArray (int anum, int index, int *ival);
void   xr_getDoubleFromArray (int anum, int index, double *dval);
void   xr_getBoolFromArray (int anum, int index, int *bval);
void   xr_getStringFromArray (int anum, int index, char **value);
void   xr_getDatetimeFromArray (int anum, int index, char **value);
void   xr_getStructFromArray (int anum, int index, int *value);
void   xr_getArrayFromArray (int anum, int index, int *value);

xmlrpc_value *xr_getAElement (int anum);
void   xr_setAElement (int anum, xmlrpc_value *v);


/*  xrClient.c
*/
int    xr_newASync (int cnum);
int    xr_initClient (char *url, char *name, char *version);
int    xr_closeClient (int cnum);
int    xr_setClient (int cnum, char *url);
int    xr_callSync (int cnum, char *name);

int    xr_callASync (int cnum, char *name, void *ret_handler);
int    xr_asyncWait (void);

void   xr_initParam (int cnum);
void   xr_setVerbose (int verbose);
void   xr_setDebug   (int debug);

void   xr_setIntInParam (int cnum, int value);
void   xr_setDoubleInParam (int cnum, double value);
void   xr_setBoolInParam (int cnum, int value);
void   xr_setStringInParam (int cnum, char *str);
void   xr_setDatetimeInParam (int cnum, char *str);
void   xr_setStructInParam (int cnum, int snum);
void   xr_setArrayInParam (int cnum, int anum);

int    xr_getIntFromResult (int cnum, int *value);
int    xr_getDoubleFromResult (int cnum, double *value);
int    xr_getBoolFromResult (int cnum, int *value);
int    xr_getStringFromResult (int cnum, char **value);
int    xr_getDatetimeFromResult (int cnum, char **date);
int    xr_getStructFromResult (int cnum, int *snum);
int    xr_getArrayFromResult (int cnum, int *anum);

char  *xr_getErrMsg (int cnum);
int    xr_getErrCode (int cnum);

void   xr_envClean (int cnum);
void   xr_freeParam (int cnum);
void   xr_freeResult (int cnum);
void   xr_clientCleanup (int cnum);
void   xr_printClient (int cnum);


/*  xrMethod.c
*/
int    xr_getIntFromParam (void *data, int index);
double xr_getDoubleFromParam (void *data, int index);
char  *xr_getStringFromParam (void *data, int index);
int    xr_getBoolFromParam (void *data, int index);
char  *xr_getDatetimeFromParam (void *data, int index);
int    xr_getStructFromParam (void *data, int index);
int    xr_getArrayFromParam (void *data, int index);

void   xr_setIntInResult (void *data, int val);
void   xr_setDoubleInResult (void *data, double val);
void   xr_setBoolInResult (void *data, int val);
void   xr_setStringInResult (void *data, char *val);
void   xr_setDatetimeInResult (void *data, char *val);
void   xr_setStructInResult (void *data, int snum);
void   xr_setArrayInResult (void *data, int anum);

void   xr_setShutdown (void *data, int val);


/*  xrServer.c
*/
typedef void (*sighandler_t)(int);

int    xr_createServer (char *path, int port, char *logfile);
int    xr_addServerMethod (char *name, void *method, void *userData);
int    xr_removeServerMethod (char *name);
void   xr_setServerParam (char *param, void *value);
pthread_t xr_startServerThread (void);
void   xr_startServer (void);
int    xr_shutdownServer (void);
int    xr_requestAbort (void *data);
void   xr_setShutdownLevel (int level);
void   xr_setSigHandler (int sig, sighandler_t handler);


/*  xrStruct.c
*/
int    xr_newStruct (void);
void   xr_freeStruct (int snum);

void   xr_printJSONStruct (int snum);
int    xr_structSize (int snum);
char  *xr_getStructKey (int snum, int index);
char  *xr_getStructVal (int snum, int index);

void   xr_setIntInStruct (int snum, char *key, int value);
void   xr_setDoubleInStruct (int snum, char *key, double value);
void   xr_setBoolInStruct (int snum, char *key, int value);
void   xr_setStringInStruct (int snum, char *key, char *value);
void   xr_setDatetimeInStruct (int snum, char *key, char *value);
void   xr_setStructInStruct (int snum, char *key, int value);
void   xr_setArrayInStruct (int snum, char *key, int value);

void   xr_getIntFromStruct (int snum, char *key, int *value);
void   xr_getDoubleFromStruct (int snum, char *key, double *value);
void   xr_getBoolFromStruct (int snum, char *key, int *value);
void   xr_getStringFromStruct (int snum, char *key, char **value);
void   xr_getDatetimeFromStruct (int snum, char *key, char **value);
void   xr_getStructFromStruct (int snum, char *key, int *value);
void   xr_getArrayFromStruct (int snum, char *key, int *value);

xmlrpc_value *xr_getSParam (int snum);
void   xr_setSParam (int snum, xmlrpc_value *v);


/*  xrUtil.c
*/
void   xr_setupSigtermHandler (xmlrpc_server_abyss_t *serverP);
void   xr_svrSigtermHandler (int signalClass);
void   xr_restoreSigtermHandler (void);

void   xr_setupSigpipeHandlers (void);

char  *xr_getPeerIpAddr (TSession * const abyssSessionP);
void   xr_dieIfFailed (char *description, xmlrpc_env env);
void   xr_dbgPrintParams (xmlrpc_server_abyss_parms s);
void   die_on_error (xmlrpc_env *env);
void   warn_on_error (xmlrpc_env *env);



/*  xrValues.c  
*/
void   xr_initValues (void);
int    xr_newValue (int type, void *v);
int    xr_tmpValue (int type, void *v);
void   xr_freeValue (int index);

int    xr_appendStruct (int snum, char *key, int value);
int    xr_appendArray (int anum, char *key, int value);

void   xr_getStructValue (int snum, char *key, void *value);
void   xr_getArrayValue (int anum, int index, void *value);
