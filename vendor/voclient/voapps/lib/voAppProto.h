/**
 *  VOAPPPROTO.H -- VOApps prototype headers.
 */


/**
 *  VOACLIST.C -- Procedures for handling the AccessList of images/data
 */
void vot_addToAclist (char *url, char *outfile);
void vot_freeAclist (void);
void vot_procAclist (void);


/**
 *  VODALUTIL.C  -- Utility procedures for the DAL interface worker procedures.
*/
int   vot_extractResults (char *result, char delim, svcParams *pars);
char *vot_openExFile (svcParams *pars, int nrows, char *extn, FILE **ofd);
char *vot_getOFName (svcParams *pars, char *extn, int pid);
char *vot_getOFIndex (svcParams *pars, char *extn, int pid);
int   vot_countResults (char *result);
void  vot_dalExit (int code, int count);
void  vot_printHdr (int fd, svcParams *pars);
void  vot_printCountHdr (void);
int   vot_printCount (Query query, svcParams *pars, int *res_count);
void  vot_printCountLine (int nrec, svcParams *pars);

char  vot_svcTypeCode (int type);
char *vot_getExtn (void);

char *vot_procTimestamp (void);
void  vot_concat (void);


/**
 *  VODALUTIL.C  -- Utility procedures for the DAL interface worker procedures.
 */
void  vot_initHTML (FILE *fd, svcParams *pars);
void  vot_printHTMLRow (FILE *fd, char *line, int isHdr, int rownum);
void  vot_closeHTML (FILE *fd);


/**
 *  VOINV.C -- VOInventory service routines.
 */
char *vot_doInventory (void);
char *vot_execInv (double ra, double dec, double radius, char *sources,
    		    char *resources, char *id, char *rettype, FILE *outfile);


/**
 *  VOKML.C  -- Utility procedures for writing Google KML files.
 */
void  vot_initKML (FILE *fd, svcParams *pars);
void  vot_printKMLPlacemark (FILE *fd, char *id, double ra, double dec, 
	char *line, char *acref, svcParams *pars);
void  vot_mkPlaceDescr (FILE *fd, char *line, char *acref, svcParams *pars);
void  vot_closeKML (FILE *fd);
void  vot_concatKML (char *fname);
void  vot_concatKMLByObject (FILE *fd);
void  vot_concatKMLByService (FILE *fd);
char *vot_getSName (char *root);
char *vot_getOName (char *root);
int   vot_copyKMLFile (char *root, char *name, FILE *fd);
void  vot_cleanKML (void);


/**
 *  VOLOG.C -- VOApps logging interface.  
 */
void  vo_appLog (FILE *fd, char *format, ...);
void  vo_encodeString (char *buf, char *format, va_list *argp);
char *vo_doarg (va_list **argp, int dtype);
char *vo_logtime (void);


/**
 *  VOARGS.C -- Procedures for commandline argument handling.  We also do
 */
int   vot_parseObjectList (char *list, int isCmdLine);
void  vot_freeObjectList (void);
int   vot_countObjectList (void);
int   vot_printObjectList (FILE *fd);
void  vot_readObjFile (char *fname);


/**
 *  VOPARAMS.C -- Interface to manage cmdline options or library parameters.
 */
char **vo_paramInit (int argc, char *argv[]);
int    vo_paramNext (char *opts, struct option long_opts[], int argc, 
		char *argv[], char *optval, int *posindex);
void   vo_paramFree (int argc, char *argv[]);

 
/**
 *  VORANGES -- Simple range-specification package to decode lists of numbers
 *  or ranges of the form:
 */
int vot_decodeRanges (range_string, ranges, max_ranges, nvalues);
int get_next_number (int ranges[], int number);
int is_in_range (int ranges[], int number);


/**
 *  VOSCS.C -- Worker procedure to query a Simple Cone Search service.
 */
int vot_callConeSvc (svcParams *pars);


/**
 *  VOSIAP.C -- Worker procedure to make a query to an SIAP service.
 */
int vot_callSiapSvc (svcParams *pars);
char *vot_validateFile (char *fname);


/**
 *  VOSSAP.C -- Worker procedure to make a query to an SSAP service.
 */
int vot_callSsapSvc (svcParams *pars);


/**
 *  VOSVC.C -- Procedures for commandline argument and DAL service handling.
 */
int   vot_parseServiceList (char *list, int dalOnly);
void  vot_freeServiceList (void);
void  vot_resetServiceCounters (void);
void  vot_addToSvcList (char *name, char *ident, char *url, char *type, 
				char *title);
int   vot_countServiceList (void);
int   vot_printServiceList (FILE *fd);
int   vot_printServiceVOTable (FILE *fd);
void  vot_readSvcFile (char *fname, int dalOnly);


/**
 *  VOTASK.C -- Utilities to run a VOApps task as a connected subprocess.
 */
int vo_runTask (char *method, Task *apps, int argc, char **argv, size_t *len, 
		void **result);
int  vo_taskTest (Task self, char *arg, ...);

int vo_setResultFromFile (char *fname, size_t *len, void **data);
int vo_setResultFromInt (int value, size_t *len, void **data);
int vo_setResultFromReal (float value, size_t *len, void **data);
int vo_setResultFromString (char *str, size_t *len, void **data);


/**
 *  VOUTIL.C -- Utility procedures for the VO-CLI tasks.
 */
int   vot_regResolver (char *term, char *svctype, char *bpass, char *subject, 
		 char *fields, int index, int exact, int dalOnly, char **res);
int   vot_regSearch (char **ids, int nids, char *svctype, char *bpass, 
		   char *subject, int orValues, int votable, FILE *vot_fd,
		   int dalOnly, int sortRes, int terse);
void  pretty_print (char *result, int nresults);

void  ppResSummary (char *result, int nresults);
void  ppMultiLine (char *result, int poffset, int pwidth, int maxchars);

void  pretty_print_table (char *result, int nresults, char *fields);
char *vot_parseSvcType (char *svctype, int exact);

char *vot_parseBandpass (char *bpass);
char *vot_parseSubject (char *subject);

char *vot_urlFname (char *url);
void  vot_printAttrs (char *fname, Query query, char *ident);
void  vot_printRegVOTableHdr (FILE *fd);

void  vot_printRegVOTableRec (FILE *fd, RegResult resource, int recnum);
void  vot_printRegVOTableTail (FILE *fd);

char *xmlEncode (char *in);
char *vot_getline (FILE *fd);
char *vot_normalizeCoord (char *coord);
char *vot_normalize (char *str);
char *vot_toURL (char *arg);
void  vot_setArg (char **argv, int *argc, char *value);

int   isVOTable (char *fname); 			/* utility functions  */
int   isSexagesimal (char *str);
int   isDecimal (char *str);
float sexa (char *s);
char *toSexa (double pos);
char *toSexaTime (int nsec);
char *vot_mktemp (char *root);
char *vot_copyStdin (void);
void  vot_skipHdr (FILE *fd);

char *vot_getTableCol (char *line, int col, int span);
int   vot_isNumericField (handle_t field);
int   vot_fileType (char *fname);
int   vot_sum32 (char *str);
int   strdic (char *in_str, char *out_str, int maxchars, char *dict);


/**
**  VOXML.C  -- Utility procedures for writing XML files, i.e. the raw
*/

void  vot_concatXML (char *fname);
int   vot_copyXMLFile (char *root, char *name, FILE *fd);
void  vot_cleanXML (void);


/**
 *  VOSUTIL.C - Utility routines for the VOSAMP tools.
 */
int   vos_urlType (char *url);
char *vos_getFName (char *path);
char *vos_typeName (int type);
int   vos_getURL (char *url, char *fname);
char *vos_optArg (char *arg);
char *vos_toURL (char *arg);
int  *vos_toIntArray (char *arg, int *nrows);

int   vos_openServerSocket (int port);
int   vos_openClientSocket (char *host, int port, int retry);
int   vos_testClientSocket (char *host, int port);
int   vos_sockReadHdr (int fd, int *len, char *name, int *type, int *mode);
int   vos_sockWriteHdr (int fd, int len, char *name, int type, int mode, 
		char *to);
void  vos_sockPrintHdr (char *msg, int fd);
int   vos_sockRead (int fd, void *vptr, int nbytes);
int   vos_sockWrite (int fd, void *vptr, int nbytes);
int   vos_fileRead (int fd, void *vptr, int nbytes);
int   vos_fileWrite (int fd, void *vptr, int nbytes);
void  vos_setNonBlock (int sock);
char *vos_getLocalIP (void);

struct hostent *vos_getHostByName (char *name);
struct hostent *vos_dupHostent (struct hostent *hentry);

int   vos_strsub (char *in, char *from, char *to, char *outstr, int maxch);
