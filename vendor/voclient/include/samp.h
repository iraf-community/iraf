/**
 *  SAMP.H -- SAMP interface include file.
 *
 *  @brief      SAMP interface include file.
 *
 *  @file       samp.h
 *  @author     Mike FItzpatrick
 *  @date       7/10/09
 */

#include <xmlrpc-c/base.h>              /* XMLRPC-C interface               */
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>
#include <sys/types.h>                  /* for struct stat                  */
#include <sys/stat.h>
#include <unistd.h>

#include "xrpc.h"                       /* XRPC Wrapper Definitions         */

#ifndef SZ_NAME
#define	SZ_NAME	    	    256		/** size of a file path       	    */
#endif
#ifndef SZ_LINE
#define	SZ_LINE		    256 	/** size of a text line       	    */
#endif

#define	SZ_SECRET	    64		/** size of secret string     	    */
#define	SZ_DESC		    8192	/** len of a description      	    */
#define	SZ_URL		    1024	/** len of a URL	      	    */
#define	SZ_CMD		    1024	/** len of a command	      	    */
#define SZ_SBUF             65536	/** big string buffer		    */
#define	SZ_RESSTR	    1024	/** len of result string      	    */

#define	DEF_PORT	    9876	/** server port			    */
#define	DEF_TIMEOUT	    "15"	/** sync message timeout	    */

#define MAX_SAMPS	    16		/** max clients allowed	      	    */
#define	MAX_HUBS	    16		/** max hubs allowed	      	    */
#define	MAX_MDATTRS	    32		/** max metadata attrs	      	    */
#define	MAX_SUBS	    256		/** max subscriptions allowed  	    */
#define	MAX_CLIENTS	    32		/** max number of clients      	    */
#define	MAX_ROWS	    256		/** max rows to highlight  	    */

#define SAMP_ERR	    -1		/** error return		    */
#define SAMP_PENDING	    0		/** pending operation		    */
#define SAMP_OK		    1		/** ok return			    */

#define SAMP_CBR	    0		/** use call-by-reference	    */
#define SAMP_CBV	    1		/** use call-by-value		    */

#define	SAMP_SYNCH	    0		/** synchronous pattern		    */
#define	SAMP_ASYNCH	    1		/** asynchronous pattern	    */
#define	SAMP_NOTIFY	    2		/** notification pattern	    */

#define SAMP_INT            TY_INT	/* values from xrpcP.h 	      	    */
#define SAMP_DOUBLE         TY_DOUBLE
#define SAMP_BOOL           TY_BOOL
#define SAMP_STRING         TY_STRING
#define SAMP_DATETIME       TY_DATETIME
#define SAMP_SAMPRUCT       TY_STRUCT
#define SAMP_ARRAY          TY_ARRAY

#define	SAMP_TRACE	    0		/** debug trace               	    */


/** 
 * Special Hub events
 */
#define HUB_SHUTDOWN	    0		/** Hub is shutting down	    */
#define HUB_REGISTER	    1		/** An app has registered	    */
#define HUB_UNREGISTER	    2		/** An app has unregistered	    */
#define HUB_SUBSCRIPTIONS   3		/** An app declared subscritions    */
#define HUB_METADATA	    4		/** An app declared metadata	    */
#define HUB_DISCONNECT	    5		/** An app is forcibly disconnected */


#define	LEN_DESC	    32768	/** max len of description    	    */

#ifdef min
#undef min
#endif
#ifdef max
#undef max
#endif

#define  min(a,b)		(a<b?a:b)
#define  max(a,b)		(a>b?a:b)


typedef  long   handle_t; 		/** generic object handle	    */
typedef  int    Map; 			/** SAMP Map datatype  		    */
typedef  int    List; 			/** SAMP List datatype  	    */
typedef  int    Msg; 			/** SAMP Msg datatype  		    */
typedef  int    Param; 			/** SAMP Param datatype		    */
typedef  char  *String;			/** SAMP String datatype  	    */


/**
 *  Application (and Hub) metadata.
 */
typedef struct {
    char      name[SZ_LINE];		/** name			    */
    char      desc[SZ_DESC];		/** descriptive text		    */
    char      iconURL[SZ_URL];		/** icon URL			    */
    char      docURL[SZ_URL];		/** documentation URL		    */

    char     *descHTML;			/** descriptive text (HTML)	    */

    int	      nkeys;			/** number of meta keys		    */
    char     *aKey[MAX_MDATTRS];	/** attr keyword		    */
    char     *aVal[MAX_MDATTRS];	/** attr value			    */
} appMD, *appMDP;



/**
 *  Message subscription.
 */
typedef struct {
    char  mtype[SZ_LINE];		/** mtype string		    */
    int   (*userFunc)(void *p); 	/** user handler function           */
					/** samp handler function           */
    int   (*sampFunc)(char *sid, char *sender, char *msgid, Map map);       	
} Subs, *SubsP;



/**
 *  Registered Client name mappings.
 */
typedef struct {
    char  pubId[SZ_NAME];		/** public name			    */
    char  name[SZ_NAME];		/** app name			    */
} Client, *ClientP;

 
/**
 *  Hub description.  Our application connects to this hub by default, but
 *  the structure will be valid for any Hub.
 */
typedef struct {
    char      appName[SZ_LINE];		/** application name		    */
    char      appVer[SZ_LINE];		/** application version		    */
    char      description[SZ_LINE];	/** descriptive text		    */

    appMD     meta;			/** metadata			    */

    char      secret[SZ_SECRET];	/** registration string		    */
    char      url[SZ_URL];		/** Hub service endpoint	    */
    char      version[SZ_NAME];		/** Hub version string		    */

    int	      id;			/** Hub XML-RPC connection	    */

    char      appId[SZ_NAME];		/** client key			    */
    char      privateKey[SZ_LINE];	/** client key value		    */
    char      hubId[SZ_LINE];		/** Hub id value		    */
    char      selfId[SZ_LINE];		/** Client id value		    */
    char      timeout[SZ_NAME];		/** Sync msg timeout (str)	    */

    void     *samp;			/** back pointer		    */
} Hub, *HubP;


/**
 *  SAMP application description.  By default this describes our app by
 *  may be used to store information about other apps in the network as
 *  well.
 */
typedef struct {
    char      appName[SZ_NAME];		/** application name		    */
    char      appVer[SZ_LINE];		/** application version		    */
    char      description[SZ_NAME];	/** application description	    */

    char      errortxt[SZ_LINE];	/** last msh error string	    */

    appMD     meta;			/** metadata			    */

    pthread_t svrThread;		/** server thread number	    */

					/** default user handler	    */
    int   (*defaultUserFunc)(char *sender, char *msgid, Map map);       	

    Subs      subs[MAX_SUBS];		/** message subscriptions	    */
    int	      nsubs;			/** number of subscriptions	    */

    Client    clients[MAX_CLIENTS];	/** samp clients		    */
    int	      nclients;			/** number of samp clients	    */

    int	      serverTid;		/** samp server threadId	    */
    int	      serverPort;		/** samp server port		    */

    Hub      *hub;			/** Hub connection 		    */
    handle_t  hubHandle;		/** Hub handle alias 		    */
    int       hubThreadID;		/** Hub thread id 		    */

    int	      active;			/** is interface active		    */
    int	      mapClients;		/** map other clients		    */
    int	      msgMode;			/** (a)synch message mode	    */
    int	      handlerMode;		/** CBR / CBV for user handlers     */

    FILE     *logfd;			/** log file descriptor		    */
    int	      debug;			/** debug flag			    */
    int	      verbose;			/** verbose flag		    */
    int	      trace;			/** trace flag			    */
} Samp, *SampP;


#define MSG_SYNC	0
#define MSG_ASYNC	1
#define MSG_NOTIFY	2
#define DEF_CALLMODE	MSG_ASYNC



/**
 *   Prototype declarations.
 */

/******************************************************************************
 **  Public Interface Methods
 *****************************************************************************/

/*  samp.c -- Methods called by user apps to initialize the interface.
 */
handle_t  sampInit (String appName, String description);
void 	  samp_Metadata (handle_t handle, String field, String value);
void 	  samp_Subscribe (handle_t handle, String mtype, void *func);
void 	  samp_Unsubscribe (handle_t handle, String mtype);
int  	  sampStartup (handle_t handle);
int  	  sampShutdown (handle_t handle);
int  	  sampDebug (handle_t handle, int value);
int  	  sampVerbose (handle_t handle, int value);
void 	  sampClose (handle_t handle);
int 	  samp_hubActive (handle_t handle);
int 	  samp_setOpt (handle_t handle, char *opt, int value);

void 	  samp_setSyncMode (handle_t handle);
void 	  samp_setASyncMode (handle_t handle);
void 	  samp_setNotifyMode (handle_t handle);
void 	  samp_setMsgMode (handle_t handle, int mode);
void 	  samp_setCallByRef (handle_t handle);
void 	  samp_setCallMode (handle_t handle, int mode);

void 	  samp_setReplyCallback (handle_t handle, int *func);
void 	  samp_setResponseCallback (handle_t handle, int *func);
void      samp_setTimeout (handle_t handle, int timeout);
void      samp_setAppName (handle_t handle, String name);
void      samp_setAppVersion (handle_t handle, String version);

void 	  samp_defaultReplyHandler (handle_t handle);
void 	  samp_deaultfResponseHandler (handle_t handle);
int 	  samp_replyStatus (handle_t handle);

int  	  samp_mapClients (handle_t handle);
int  	  samp_listClients (handle_t handle);
char  	 *samp_getClients (handle_t handle);
int  	  samp_addClient (handle_t handle, String name, String id);
int  	  samp_removeClient (handle_t handle, String id);

Map	  samp_getOKMap (void);
Map	  samp_getNullMap (void);


/*  sampCommands.c -- Methods called to send messages to the Hub.
 */
int 	samp_Register (handle_t handle);
int 	samp_UnRegister (handle_t handle);
int 	samp_DeclareMetadata (handle_t handle);
int 	samp_Ping (handle_t handle, String appName);
Map 	samp_GetMetadata (handle_t handle, String pubId);
int 	samp_DeclareSubscriptions (handle_t handle);
Map     samp_GetSubscriptions (handle_t handle, String pubId);
List 	samp_GetRegisteredClients (handle_t handle);
List 	samp_GetSubscribedClients (handle_t handle, String mtype);


/*  sampMType.c -- Methods called to send messages to other apps.
 */
int 	samp_tableLoadVOTable (handle_t handle, String recip, String url, 
		String tableId, String name);
int 	samp_tableLoadFITS (handle_t handle, String recip, String url, 
		String tableId, String name);
int 	samp_imageLoadFITS (handle_t handle, String recip, String url, 
		String imageId, String name);

int 	samp_tableHighlightRow (handle_t handle, String recip, String tableId, 
		String url, int row);
int 	samp_tableSelectRowList (handle_t handle, String recip, String tableId, 
		String url, int rows[], int nrows);
int 	samp_coordPointAtSky (handle_t handle, String recip, 
		float ra, float dec);
int 	samp_specLoadSSAGeneric (handle_t handle, String recip, String url, 
		Map meta, String spectrumId, String name);

int     samp_cmdExec (handle_t handle, String recip, String cmd);
char   *samp_envGet (handle_t handle, String recip, String name);
int 	samp_envSet (handle_t handle, String recip, String name, String value);
char   *samp_paramGet (handle_t handle, String recip, String name);
int 	samp_paramSet(handle_t handle, String recip, String name, String value);
int 	samp_bibLoad (handle_t handle, String recip, String bibcode);
int 	samp_resourceLoad (handle_t handle, String recip, String type, 
		String name, Map resMap);

int 	samp_sendGeneric (handle_t handle, String recip, String mtype, 
		String args[]);
int 	samp_sendMsg (handle_t handle, String recip, Map msg);



/*  sampClient.c -- Low-level methods to send messages.
 */
void    samp_notify (handle_t handle, String recipId, Map msg);
List    samp_notifyAll (handle_t handle, Map msg);
String  samp_call (handle_t handle, String recipId, String tag, Map msg);
int     samp_callAll (handle_t handle, String msg_tag, Map msg);
int     samp_callAndWait (handle_t handle, String recipId, String msg_tag, 
				Map msg);
int  	samp_Reply (handle_t handle, String msg_id, Map resp);

String 	samp_clientName (handle_t handle, String pubId);
int	samp_setErr (handle_t handle, Map resp);
String  samp_getErr (handle_t handle);


/*  sampMethods.c -- SAMP methods implemented by a callable client.
 */
int 	samp_receiveCall (void *data);
int 	samp_receiveNotification (void *data);
int 	samp_receiveResponse (void *data);

void	samp_setHandlerReply (Map resp);
Map	samp_getHandlerReply (void);


/*  sampHandlers.c -- Handlers to responses from the message.
 */
void 	samp_setUserHandler (handle_t handle, String mtype, void *func);
void 	samp_setSampHandler (handle_t handle, String mtype, void *func);
void   *samp_getUserHandler (String mtype);
void   *samp_getSampHandler (String mtype);
void    samp_execUserHandler (String mtype, String sender, 
		String msg_id, Map params);

int     samp_genericMsgHandler (String sender, String mtype, String msg_id, 
		Map msg_map);

int 	samp_PingHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_StatusHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_imLoadHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_tbLoadHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_tbLoadFITSHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_tbLoadVOTHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_tbHighlightHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_tbSelectHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_pointAtHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_specLoadHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_specSSAHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_cmdExecHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_envGetHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_envSetHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_paramGetHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_paramSetHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_bibcodeHandler (String sender, String mtype, String msg_id,
		Map msg_map);

int 	samp_resLoadHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_resConeHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_resSiapHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_resSsapHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_resTapHandler (String sender, String mtype, String msg_id,
		Map msg_map);
int 	samp_resVOSpaceHandler (String sender, String mtype, String msg_id,
		Map msg_map);

void    samp_printMessage (String mtype, String sender, String msg_id, 
		Map params);

void    samp_printMap (String name, Map map);
int 	samp_nullResponse (void *data);
int 	samp_testEcho (void *data);



/******************************************************************************
 **  Internal Interface Methods
 *****************************************************************************/

/*  sampHub.c
 */
handle_t  samp_hubOpen (Samp *samp);
int 	  samp_hubClose (handle_t handle);
List 	  samp_getAvailableHubs (handle_t handle);
char     *samp_getActiveHubName (handle_t handle);
int 	  samp_getActiveHub (handle_t handle);
int 	  samp_hubRunning (void);
int 	  samp_hubInit (handle_t samp, char *appName, char *descr);

int	  samp_processHubEvent (String mtype, Map params);
int	  samp_hubEvent (String mtype);

int 	  samp_hubRegister (Hub *hub);
int 	  samp_hubUnRegister (Hub *hub);
int 	  samp_hubSendShutdown (Hub *hub);
int	  samp_hubSetXmlrpcCallback (Hub *hub);
int 	  samp_hubPing (Hub *hub);
int 	  samp_hubDeclareMetadata (Hub *hub);
int 	  samp_hubDeclareSubscriptions (Hub *hub);


/* sampList.c
*/
handle_t  samp_newList ();
void 	  samp_freeList (List list);
int 	  samp_listLen (List list);

void 	  samp_setStringInList (List list, char *value);
void 	  samp_setMapInList (List list, Map map);
void 	  samp_setListInList (List list1, List list2);
void 	  samp_setIntInList (List list, int value);
void 	  samp_setFloatInList (List list, float value);

char     *samp_getStringFromList (List list, int index);
Map 	  samp_getMapFromList (List list, int index);
List 	  samp_getListFromList (List list, int index);
int 	  samp_getIntFromList (List list, int index);
float 	  samp_getFloatFromList (List list, int index);


/* sampMap.c
*/
handle_t  samp_newMap (void);
void 	  samp_freeMap (Map map);

int	  samp_getMapSize (Map map);
char 	 *samp_getMapKey (Map map, int index);
char 	 *samp_getMapVal (Map map, int index);

void 	  samp_setStringInMap (Map map, char *key, char *value);
void 	  samp_setMapInMap (Map map1, char *key, Map map2);
void 	  samp_setListInMap (Map map, char *key, List list);
void 	  samp_setIntInMap (Map map, char *key, int value);
void 	  samp_setFloatInMap (Map map, char *key, float value);

char     *samp_getStringFromMap (Map map, char *key);
Map 	  samp_getMapFromMap (Map map, char *key);
List 	  samp_getListFromMap (Map map, char *key);
int 	  samp_getIntFromMap (Map map, char *key);
float 	  samp_getFloatFromMap (Map map, char *key);


/* sampMsg.c
 */
Msg 	  samp_newMsg (void);
void 	  samp_freeMsg (Msg msg);
void 	  samp_msgMType (Msg msg, String mtype);
void 	  samp_msgParam (Msg msg, Param param);
char     *samp_msgTag (void);


/* sampParam.c
 */
Param 	  samp_newParam (void);
void 	  samp_freeParam (Param param);
Param 	  samp_paramInit (Msg msg);
void 	  samp_addStringParam (Msg msg, char *keyw, String val);
void 	  samp_addMapParam (Msg msg, char *keyw, Map val);
void 	  samp_addListParam (Msg msg, char *keyw, List val);
void 	  samp_addIntParam (Msg msg, char *keyw, int val);
void 	  samp_addFloatParam (Msg msg, char *keyw, float val);
int 	  samp_paramLen (Msg msg);


/* sampLog.c
*/
void 	  sampLog (handle_t handle, char *format, ...);
void 	  sampTrace (handle_t handle, char *format, ...);


/*  sampUtil.c
 */
handle_t  samp_newHandle (void *ptr);
void 	  samp_freeHandle (handle_t handle);

handle_t  samp_P2H (void *ptr);
void     *samp_H2P (handle_t handle); 

char     *samp_app2id (handle_t handle, char *appName);
char     *samp_id2app (handle_t handle, char *pubId);

int	  samp_serverPort (void);
void 	  samp_printMetadata (handle_t handle, String name);
char 	 *samp_getMetadata (handle_t handle, String name);

