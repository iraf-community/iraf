/*
 *  VOCLIENT.H == Global include file for the VOClient Interface.
 *
 *  M. Fitzpatrick, NOAO, June 2006
 */

#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <sys/un.h>
#include <netdb.h>
#include <fcntl.h>


/* Function prototypes */
#ifdef __STDC__
#include <stddef.h>
#include <stdlib.h>
#else
char    *getenv();
#endif


#ifdef __cplusplus
extern "C" {
#endif

/* For the signal handling. */
typedef void  (*SIGFUNC)();


/* Interface Variables.
 */



#ifdef OK
#undef OK
#endif
#define OK        	0

#ifdef ERR
#undef ERR
#endif
#define ERR        	1

#ifdef TRUE
#undef TRUE
#endif
#define TRUE        	1

#ifdef FALSE
#undef FALSE
#endif
#define FALSE        	0

#define	DEF_SERVER		"6200:localhost"
#define	DEF_NET_SERVER		"9000:iraf-nvo.noao.edu"

#define	DEF_RUNID		"VOClient"

#define MSG_CALL        	1
#define MSG_RESULT      	2
#define MSG_MESSAGE     	3

#define	TY_INT			1	/* result data types		*/
#define	TY_FLOAT		2
#define	TY_STRING		3
#define	TY_BULK			4

#define SZ_MSGBUF       	102400
#define SZ_METHOD       	64
#define SZ_CLASS        	64
#define SZ_MSGSTR       	65535
#define SZ_PBUF         	128
#define SZ_FNAME        	256
#define MAX_VALUES      	64

#define DAL_CONN   		1	/* DAL Connection Types		*/
#define CONE_CONN  		2	/* Simple Cone Search		*/
#define SIAP_CONN  		3	/* Simple Image Access		*/
#define SSAP_CONN  		4	/* Simple Spectral Access	*/
#define SLAP_CONN  		5	/* Simple Line Access		*/
#define STAP_CONN  		6	/* Synch TAP Access		*/

#define CONE_SERVICE  		1
#define SIAP_SERVICE  		2
#define SSAP_SERVICE  		3
#define TAP_SERVICE  		4
#define REG_SERVICE  		5
#define SKYNODE_SERVICE  	6
#define WEB_SERVICE  		7
#define REST_SERVICE  		8

#define VOC_RAW			0
#define VOC_CSV			1
#define VOC_TSV			2
#define VOC_ASCII		3
#define VOC_VOTABLE		4

#define VOC_NULL		0	/* integer NULL			*/


typedef int   ObjectID;			/* DAL type aliases		*/
typedef int   DAL;
typedef int   Query;
typedef int   QResponse;
typedef int   QRecord;
typedef int   QRAttribute;

typedef int   Sesame;			/* Name Resolver aliases	*/

typedef int   Skybot;			/* SkyBoT Service aliases	*/

typedef int   RegQuery;			/* Registry Query object	*/
typedef int   RegResult;		/* Query Reuslt object		*/

#ifdef _VOCLIENT_LIB_

typedef struct vocMsg {
    int         type;                   /* message type                 */

    ObjectID    objId;                  /* for CALL messages            */
    char        method[SZ_METHOD];
    int         nparams;                

    int         status;                 /* for RESULT messages          */
    int         restype;                
    int         nitems;         

    char        msgclass[SZ_CLASS];        /* for MESSAGE messages         */
    char        msgstr[SZ_MSGSTR];

    char        message[SZ_MSGBUF];     /* fully formed message         */
} vocMsg_t;



typedef struct vocRes {
    int         status;                 /* result status                */
    int         type;                   /* type of result value         */
    int         nitems;                 /* no. of returned items        */      
    char        value[MAX_VALUES][SZ_MSGSTR];   /* value strings        */

    void	*buf;			/* bulk data buffer		*/
    int		buflen;			/* length of buffer		*/
} vocRes_t;


typedef struct VOClient {
    char   *server_host;                /* socket to DALServer          */
    char   *runid;                      /* RUNID logging string	        */
    int     server_port;
    int     io_chan;

    int     msg_port;                   /* asynch message socket        */
    int     msg_chan;

    int     onetrip;			/* private invocation flag	*/
    int     debug;			/* debug flag			*/
    int     quiet;			/* suppress output?		*/
    int     use_cache;			/* use cached results?		*/
    int     use_runid;			/* use RUNID parameter?		*/

} VOClient, *VOClientPtr;


#define	VOC_DEBUG	(vo->debug > 0)
#define MSG_DEBUG	(vo->debug > 1)

#endif





/* Prototype Declarations.
 */

/*  DAL Interface procedures.
 */
char       *voc_coneCaller (char *url, double ra, double dec, double sr, 
		int otype);
int         voc_coneCallerToFile (char *url, double ra, double dec, double sr, 
            	int otype, char *file);

char       *voc_siapCaller (char *url, double ra, double dec, double rsize, 
            	double dsize, char *fmt, int otype);
int         voc_siapCallerToFile (char *url, double ra, double dec, 
            	double rsize, double dsize, char *fmt, int otype, char *file);
char       *voc_ssapCaller (char *url, double ra, double dec,
            	double size, char *band, char *time, char *fmt, int otype);
int         voc_ssapCallerToFile (char *url, double ra, double dec, 
            	double size, char *band, char *time, char *fmt, int otype, 
		char *file);
char       *voc_getRawURL (char *url, int *nbytes);
int         voc_validateObject (int hcode);
void        voc_freePointer (char *ptr);


int 	    voc_initVOClient (char *opts);
void	    voc_closeVOClient (int shutdown);
void	    voc_abortVOClient (int code, char *msg);

DAL         voc_openConnection (char *service_url, int type);
DAL         voc_openConeConnection (char *service_url);
DAL         voc_openSiapConnection (char *service_url);
DAL         voc_openSsapConnection (char *service_url);
void	    voc_closeConnection (DAL dal);

int	    voc_getServiceCount (DAL dal);
void	    voc_addServiceURL (DAL dal, char *service_url);
char 	   *voc_getServiceURL (DAL dal, int index);

Query	    voc_getQuery (DAL dal, int type);
Query	    voc_getConeQuery (DAL dal, double ra, double dec, double sr);
Query	    voc_getSiapQuery (DAL dal, double ra, double dec, 
			double ra_size, double dec_size, char *format);
Query	    voc_getSsapQuery (DAL dal, double ra, double dec, 
			double size, char *band, char *time, char *format);

int         voc_addIntParam (Query query, char *name, int value);
int         voc_addFloatParam (Query query, char *name, double value);
int         voc_addStringParam (Query query, char *name, char *value);

char	   *voc_getQueryString (Query query, int type, int index);

QResponse   voc_executeQuery (Query query);
QResponse   voc_getQueryResponse (Query query);
char       *voc_executeCSV (Query query);
char       *voc_executeTSV (Query query);
char       *voc_executeASCII (Query query);
char       *voc_executeVOTable (Query query);
int	    voc_executeQueryAs (Query query, char *fname, int type);
int	    voc_getRecordCount (QResponse qr);

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
int 	    voc_getAttrCount (QRecord rec);

void	    voc_setIntAttr (QRecord rec, char *attrname, int ival);
void	    voc_setFloatAttr (QRecord rec, char *attrname, double dval) ;
void	    voc_setStringAttr (QRecord rec, char *attrname, char *str);

int 	    voc_getDataset (QRecord rec, char *acref, char *fname);



/*  Registry Interface procedures.
 */

RegResult   voc_regSearch (char *term1,  char *term2, int orValues);
RegResult   voc_regSearchByService (char *svc,  char *term, int orValues);
RegQuery    voc_regQuery (char *term, int orValues);
void	    voc_regConstSvcType (RegQuery query, char *svc);
void	    voc_regConstWaveband (RegQuery query, char *bpass);
void	    voc_regDALOnly (RegQuery query, int value);
void	    voc_regSortRes (RegQuery query, int value);
void	    voc_regAddSearchTerm (RegQuery query, char *term, int orValue);
void	    voc_regRemoveSearchTerm (RegQuery query, char *term);
int	    voc_regGetSTCount (RegQuery query);
char	   *voc_regGetQueryString (RegQuery query);
RegResult   voc_regExecute (RegQuery query);
char	   *voc_regExecuteRaw (RegQuery query);

int	    voc_resGetCount (RegResult res);
char	   *voc_resGetStr (RegResult res, char *attribute, int index);
double	    voc_resGetFloat (RegResult res, char *attribute, int index);
int	    voc_resGetInt (RegResult res, char *attribute, int index);



/*  SESAME interface procedures.
 */
Sesame      voc_nameResolver (char *target);
char       *voc_resolverPos (Sesame sr);
double      voc_resolverRA (Sesame sr);
double      voc_resolverDEC (Sesame sr);
double      voc_resolverRAErr (Sesame sr);
double      voc_resolverDECErr (Sesame sr);
char       *voc_resolverOtype (Sesame sr);



/*  SkyBoT interface procedures.
 */
Skybot      voc_skybot (double ra, double dec, double rsz, double dsz, 
		double epoch);
int         voc_skybotNObjs (Skybot sb);
char       *voc_skybotStrAttr (Skybot sb, char *attr, int index);
double      voc_skybotDblAttr (Skybot sb, char *attr, int index);



/***************************************************************************
 *   VOCMSG.C Prototypes
 */
#ifdef _VOCLIENT_LIB_

vocMsg_t *msg_newCallMsg (ObjectID objid, char *method, int nparams);
vocMsg_t *msg_newResultMsg (int status, int type, int nitems);
vocMsg_t *msg_newMsg (char *msgclass, char *str);

vocMsg_t *msg_shutdownMsg ();
vocMsg_t *msg_quitMsg ();
vocMsg_t *msg_ackMsg ();

vocRes_t *msg_sendMsg (int fd, vocMsg_t *msg);
int       msg_sendRawMsg (int fd, vocMsg_t *msg);

vocRes_t *msg_getResult (int fd);
vocRes_t *msg_getResultToFile (int fd, char *fname, int overwrite);

void      msg_addIntParam (vocMsg_t *msg, int ival);
void      msg_addFloatParam (vocMsg_t *msg, double dval);
void      msg_addStringParam (vocMsg_t *msg, char *str);
void      msg_addIntResult (vocMsg_t *msg, int ival);
void      msg_addFloatResult (vocMsg_t *msg, double dval);
void      msg_addStringResult (vocMsg_t *msg, char *str);

int       msg_resultStatus (vocRes_t *res);
int       msg_resultType (vocRes_t *res);
int       msg_resultLength (vocRes_t *res);

int       msg_getIntResult (vocRes_t *res, int index);
double    msg_getFloatResult (vocRes_t *res, int index);
char     *msg_getStringResult (vocRes_t *res, int index);
void     *msg_getBuffer (vocRes_t *res);
char     *msg_getFilename (vocRes_t *res);

#ifdef __cplusplus
}
#endif

#endif
