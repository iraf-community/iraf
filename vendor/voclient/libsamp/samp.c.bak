/**
 *  SAMP.C --  Top-level interface to the SAMP library.
 *
 *                  samp = sampInit  (appName, descr)
 *                        sampClose  (samp)

 *               stat = sampStartup  (samp)
 *                     sampShutdown  (samp)
 *
 *                    samp_Metadata  (samp, field, value)
 *                   samp_Subscribe  (samp, mtype, handler)
 *                 samp_Unsubscribe  (samp, mtype)
 *
 *            samp_setReplyCallback  (samp, func)
 *         samp_setResponseCallback  (samp, func)
 *	    stat = samp_replyStatus  (samp)
 *
 *  Utility Methods:
 *                 samp_setSyncMode  (samp)			// Default
 *                samp_setASyncMode  (samp)
 *               samp_setNotifyMode  (samp)
 *                samp_setCallByRef  (samp)
 *
 *                  samp_setTimeout  (samp, timeout)
 *                  samp_setAppNmae  (samp, version)
 *               samp_setAppVersion  (samp, name)
 *
 *                  samp_mapClients  (handle_t handle)
 *                   samp_addClient  (handle_t handle, String name, String id)
 *                samp_removeClient  (handle_t handle, String id)
 *
 *  @brief      Top-level interface to the SAMP library.
 *
 *  @file       samp.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/09
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <signal.h>
#include <ctype.h>

#include "samp.h"


Samp        *sampP;			/** SAMP struct pointer		*/
handle_t     sampH;			/** SAMP handle			*/

Map	     nullMap;			/** empty Map struct	        */
Map	     OK_Map;			/** SAMP_OK Map struct	        */
List	     nullList;			/** empty List struct	        */

extern Hub  *hub;			/** HUB connection		*/
extern int   numHubs;			/** No. of available hubs	*/
extern long  hubHandles[MAX_HUBS];	/** Handles for hubs		*/


static int  samp_initServer (Samp *sampP);
static void samp_printSubs (Samp *sampP);




/**
 *  SAMP_INIT -- Initialize the SAMP interface.
 *
 *  @brief  	Initialize the SAMP interface.
 *  @fn  	handle = sampInit (String name, String descr)
 *
 *  @param appName	application name
 *  @param description	description of application
 *  @return		samp handle
 */
handle_t
sampInit (String appName, String description)
{
    handle_t handle = -1;


    /*  Allocate the SAMP structure.
     */
    sampP = calloc (1, sizeof(Samp));
    if ((sampH = handle = samp_newHandle ((void *) sampP))  < 0) {
	fprintf (stderr, "Error allocating Samp struct.\n");
	return (-1);
    }


    /* Initialize our connection to the Hub and register as a client.
    */
    strcpy (sampP->appName, appName);
    strcpy (sampP->appVer, "1.0");
    strcpy (sampP->description, description);

    /*  Open a connection to the Hub.
    */
    if ((sampP->hubHandle = samp_hubOpen (sampP)) < 0)
        sampP->hub = (Hub *) NULL;
    else
    	sampP->hub = (Hub *) samp_H2P (sampP->hubHandle);

    sampP->msgMode         = DEF_CALLMODE;	/* set some defaults	    */
    sampP->handlerMode     = SAMP_CBV;
    sampP->trace           = SAMP_TRACE;
    sampP->verbose         = TRUE;
    sampP->serverPort      = samp_serverPort ();
    sampP->svrThread       = (pthread_t) 0;
    sampP->defaultUserFunc = NULL;

    sampTrace (handle, "%d = sampInit(%s,'%s')\n", handle,appName,description);
    
    /*  Create a NULL map for messages.
     */
    nullMap  = samp_newMap ();
    nullList = samp_newList ();

    /*  Create a OK map for messages when there is no real value to be 
     * returned.
     */
    OK_Map = samp_newMap ();
        samp_setStringInMap (OK_Map, "samp.status", "samp.ok");
        samp_setMapInMap (OK_Map, "samp.result", nullMap);

    /* Return the handle to the structure.
     */
    return ( handle );
}


/**
 *  SAMP_CLOSE -- Close the SAMP interface.
 *
 *  @brief  	Close the SAMP interface and free resources.
 *  @fn		sampClose (handle_t handle)
 * 
 *  @param handle	user handle to samp struct
 *  @return		nothing
 */
void 
sampClose (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);	/* get struct pointer	*/


    sampTrace (handle, "sampClose (%d)\n", handle);

    /* Close existing Hub handle.
     */
    if (sampP->hubHandle > 0 && samp_hubClose (sampP->hubHandle) != SAMP_OK)
	fprintf (stderr, "Error closing Hub connection.\n");

    if (sampP->hubHandle > 0)
        samp_freeHandle (sampP->hubHandle);	/* release handles	*/
    samp_freeHandle (handle);

    sampP->hub = (Hub *) NULL;
    sampP->hubHandle = -1;

    free ((void *) sampP);			/* free SAMP structure	*/
}


/**
 *  SAMPDEBUG -- Set interface debug level.
 *
 *  @brief  	Set interface debug level.
 *  @fn		sampDebug (handle_t handle, int value)
 * 
 *  @param handle	user handle to samp struct
 *  @param value	debug value
 *  @return		nothing
 */
int 
sampDebug (handle_t handle, int value)
{
    Samp *sampP = samp_H2P (handle);	/* get struct pointer	*/
    if (value >= 0)
        sampP->debug = value;

    return (sampP->verbose);
}


/**
 *  SAMPVERBOSE -- Set interface verbose level.
 *
 *  @brief  	Set interface debug level.
 *  @fn		sampVerbose (handle_t handle, int value)
 * 
 *  @param handle	user handle to samp struct
 *  @param value	verbose value
 *  @return		nothing
 */
int 
sampVerbose (handle_t handle, int value)
{
    Samp *sampP = samp_H2P (handle);	/* get struct pointer	*/
    if (value >= 0)
        sampP->verbose = value;

    return (sampP->verbose);
}


/**
 *  SAMP_STARTUP -- Startup the SAMP interface to send/receive messages.
 *
 *  @brief  	Startup the SAMP interface to send/receive messages.
 *  @fn		sampStartup (handle_t handle)
 * 
 *  @param handle	user handle to samp struct
 *  @return		nothing
 */
int 
sampStartup (handle_t handle)
{
    Samp  *sampP = samp_H2P (handle);	/* get struct pointer	*/
    int    stat;


    stat = SAMP_OK;
    if (sampP->active == 0) {
	/*  Startup the application server so we can handle the responses.
	 */
        samp_initServer (sampP);

	if (sampP->trace)
	    samp_printSubs (sampP);
	    
	/*  Open the Hub connection.
	 */
	if (sampP->hubHandle < 0)
            sampP->hubHandle = samp_hubOpen (sampP);

	/*  We *still* can't open the Hub, a first attempt failed in sampInit()
  	 */
        if (sampP->hubHandle < 0) {
            sampP->hub = (Hub *) NULL;
	    if (sampP->verbose)
	        fprintf (stderr, "Error: No Hub available on startup\n");
    	    sampP->active = 0;
	} else {
            sampP->hub = (Hub *) samp_H2P (sampP->hubHandle);
    	    sampP->active = 1;
	}
    }


    /*  Do the client startup declarations to the hub.
     */
    if (sampP->hub) {

	int   i, len, clients = 0, appnum = 0, done = 0;
	char *app = NULL, *pubId = NULL;

	/* First get the list of currently registered clients.  If we find
 	 * one already registered using our name, add a number to the name
	 * and try agin.
	 */
        pubId =  samp_id2app (handle, sampP->appName);
        clients = samp_GetRegisteredClients (handle);
	while (!done) {
	    done = 1;
            for (i=0; i < samp_listLen (clients); i++) {
                app = samp_getStringFromList (clients, i);
                len = min (strlen (pubId), strlen (app));
fprintf (stderr, "app = '%s'  pubId = '%s'\n", app, pubId);
                if (strncasecmp (pubId, app, len) == 0) {
		    sprintf (sampP->appName, "%s%d", sampP->appName, appnum++);
    		    strcpy (hub->appName, sampP->appName);
fprintf (stderr, "changed name to '%s'\n", sampP->appName);
		    done = 0;
                    break;
                }
            }
	}
        samp_freeList (clients);
fprintf (stderr, "Registering with appName  '%s'\n", sampP->appName);



        if (samp_hubDeclareMetadata (hub) != SAMP_OK) {
            fprintf (stderr, "Error: Metadata declaration fails: '%s'\n", 
                xr_getErrMsg (hub->id));
            return ((int) 0);
        }

        if (samp_hubDeclareSubscriptions (hub) != SAMP_OK) {
            fprintf (stderr, "Error: Subscription declaration fails: '%s'\n", 
                xr_getErrMsg (hub->id));
            return ((int) 0);
        }

        /*  Final step is to gather the metadata from the other attached
         *  clients.  This allows us to send messages to the appName and not
         *  just the public ID.
         */
        samp_mapClients (handle);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_SHUTDOWN -- Shut down the active SAMP interface.
 *
 *  @brief  	Shut down the active SAMP interface.
 *  @fn		sampShutdown (handle_t handle)
 * 
 *  @param handle	user handle to samp struct
 *  @return		nothing
 */
int 
sampShutdown (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);


    /* Close existing Hub handle.
     */
    if (sampP->hubHandle > 0 && samp_hubClose (sampP->hubHandle) != SAMP_OK)
        fprintf (stderr, "Error closing Hub connection.\n");

    /*  Stop XML-RPC server thread
     */
#ifdef KILL_SAMP_SERVER
    if (pthread_kill (sampP->svrThread, SIGUSR1) != 0)
	fprintf (stderr, "Error killing server thread %ld\n", 
	    (long) sampP->svrThread);
#endif

    sampP->hubHandle = -1;
    sampP->svrThread =  0;
    sampP->active    =  0;

    return (SAMP_OK);
}


/**
 *  SAMP_HUBACTIVE -- Determine if the Hub is active (i.e. connected).
 *
 *  @brief  	Determine if the Hub is active (i.e. connected).
 *  @fn		samp_hubActive (handle_t handle)
 * 
 *  @param handle	user handle to samp struct
 *  @return		nothing
 */
int 
samp_hubActive (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);
    return (sampP->active);
}



/*****************************************************************************/

/**
 *  SAMP_METADATA -- Set a metadata field for the application.  This will
 *  	be sent to the Hub later in a declareMetadata() call.
 *
 *  @brief 	Set a metadata field value for the application.
 *  @fn		samp_Metadata (handle_t handle, String field, String value)
 * 
 *  @param  handle	samp handle
 *  @param  field	metadata field to set (name, author, etc)
 *  @param  samp	value of field
 *  @return		nothing
 */
void
samp_Metadata (handle_t handle, String field, String value)
{
    Samp *sampP = samp_H2P (handle);
    Hub  *hub = sampP->hub;


    sampTrace (handle, "sampSetMetadata (%d) %s = %s\n", handle, field, value);

    if (strcmp (field, "samp.name") == 0) { 	/* Known metadata values      */
        strcpy (sampP->meta.name, value);
	strcpy (sampP->appName, value);
	if (hub)
	    strcpy (hub->appName, value);
    } else if (strcmp (field, "samp.description.text") == 0) {
        strcpy (sampP->meta.desc, value);
	strcpy (sampP->description, value);
	if (hub)
	    strcpy (hub->description, value);
    } else if (strcmp (field, "samp.icon.url") == 0) {
        strcpy (sampP->meta.iconURL, value);
	if (hub)
            strcpy (hub->meta.iconURL, value);
    } else if (strcmp (field, "samp.documentation.url") == 0) {
        strcpy (sampP->meta.docURL, value);
	if (hub)
            strcpy (hub->meta.docURL, value);
    } else {
	sampP->meta.aKey[sampP->meta.nkeys] = calloc (1, strlen (field) + 1);
	sampP->meta.aVal[sampP->meta.nkeys] = calloc (1, strlen (value) + 1);

        strcpy (sampP->meta.aKey[sampP->meta.nkeys], field);
        strcpy (sampP->meta.aVal[sampP->meta.nkeys], value);
        sampP->meta.nkeys++;

	if (hub) {
	    int n = hub->meta.nkeys;
	    hub->meta.aKey[n] = calloc (1, strlen (field) + 1);
	    hub->meta.aVal[n] = calloc (1, strlen (value) + 1);

            strcpy (hub->meta.aKey[n], field); 
            strcpy (hub->meta.aVal[n], value);
            hub->meta.nkeys++;
	}
    }

    if (hub) {
    	strcpy (hub->meta.name, sampP->appName);
    	strcpy (hub->appName, sampP->appName);
    	strcpy (hub->appVer, sampP->appVer);
    	strcpy (hub->meta.desc, sampP->description);
    	strcpy (hub->description, sampP->description);
    }
}


/**
 *  SAMP_SUBSCRIBE -- Subscribe to a given mtype.
 *
 *  @brief  	Subscribe to a given mtype.
 *  @fn		samp_Subscribe (handle_t handle, String mtype, void *func)
 * 
 *  @param handle	user handle to samp struct
 *  @param mtype	mtype name
 *  @param func		callback function
 *  @return		nothing
 */
void 
samp_Subscribe (handle_t handle, String mtype, void *userFunc)
{
    Samp *sampP = samp_H2P (handle);


    if (sampP->nsubs == MAX_SUBS) {
	fprintf (stderr, "Error: Too many subscriptions\n");
	exit (1);
    }

    if (samp_getSampHandler (mtype) == (void *) NULL) {
	/* If there is no Samp Handler installed, assumed it's a generic
	 * message so we can be sure a reply is sent in our handler.
	 */
        samp_setSampHandler (handle, mtype, samp_genericMsgHandler);
    }

    samp_setUserHandler (handle, mtype, userFunc);
}


/**
 *  SAMP_UNSUBSCRIBE -- Unsubscribe to a given mtype.
 *
 *  @brief  	Unsubscribe to a given mtype.
 *  @fn		samp_Unsubscribe (handle_t handle, String mtype)
 * 
 *  @param handle	user handle to samp struct
 *  @param mtype	mtype name
 *  @return		nothing
 */
void 
samp_Unsubscribe (handle_t handle, String mtype)
{
    Samp *sampP = samp_H2P (handle);
    register int i, j, found = 0;


    /* Find the mtype in question.
     */
    for (i=0; i < sampP->nsubs; i++) {
	if (strcasecmp (sampP->subs[i].mtype, mtype) == 0) {
	    found = 1;
	    break;
	}
    }

    if (found) {
	/*  Shift the remainder of the list. 		
	 */
	for (j=i; j < (sampP->nsubs - 1); j++) {
	    memset (sampP->subs[j].mtype, 0, SZ_LINE);
            strcpy (sampP->subs[j].mtype, sampP->subs[j+1].mtype);
            sampP->subs[j].userFunc = sampP->subs[j+1].userFunc;
            sampP->subs[j].sampFunc = sampP->subs[j+1].sampFunc;
	}
	memset (sampP->subs[j].mtype, 0, SZ_LINE);
        sampP->nsubs--;

	/*  Send unsubscribe msg to Hub.
	 */
        samp_DeclareSubscriptions (handle);
    }
}



/*****************************************************************************/

/**
 *  SAMP_SETCALLMODE -- Set the default calling mode (synch or asynch)
 *
 *  @brief	Set the default calling mode (synch or asynch)
 *  @fn 	samp_setCallMode (handle_t handle, int mode);
 * 
 *  @param  handle	samp handle
 *  @param  mode	call mode (synch or asynch)
 *  @return		nothing
 */
void
samp_setCallMode (handle_t handle, int mode)
{
    Samp *sampP = samp_H2P (handle);
    sampP->msgMode = mode;
}


/**
 *  SAMP_SETSYNCMODE -- Set the calling mode to use synchronous messaging.
 *
 *  @brief	Set the calling mode to use synchronous messaging.
 *  @fn 	samp_setSyncMode (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_setSyncMode (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);
    sampP->msgMode = MSG_SYNC;
}


/**
 *  SAMP_SETASYNCMODE -- Set the calling mode to use asynchronous messaging.
 *
 *  @brief	Set the calling mode to use asynchronous messaging.
 *  @fn 	samp_setASyncMode (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_setASyncMode (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);
    sampP->msgMode = MSG_ASYNC;
}


/**
 *  SAMP_SETNOTIFYMODE -- Set the calling mode to use notification messaging.
 *
 *  @brief	Set the calling mode to use notification messaging.
 *  @fn 	samp_setNotifyMode (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_setNotifyMode (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);
    sampP->msgMode = MSG_NOTIFY;
}


/**
 *  SAMP_SETCALLBYREF -- Have interface call user handlers by reference.
 *
 *  @brief	Have interface call user handlers by reference.
 *  @fn 	samp_setCallByRef (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_setCallByRef (handle_t handle)
{
    Samp *sampP = samp_H2P (handle);
    sampP->handlerMode = SAMP_CBR;
}


/**
 *  SAMP_SETREPLYCALLBACK -- Set the Reply callback.
 *
 *  @brief	Set the Reply callback.
 *  @dn		samp_setReplyCallback (handle_t handle, int *func)
 * 
 *  @param  handle	samp handle
 *  @param  func	callback for Reply message
 *  @return		nothing
 */
void
samp_setReplyCallback (handle_t handle, int *func)
{
}


/**
 *  SAMP_SETRESPONSECALLBACK -- Set the Response callback.
 *
 *  @brief	Set the Response callback.
 *  @dn		samp_setResponseCallback (handle_t handle, int *func)
 * 
 *  @param  handle	samp handle
 *  @param  func	callback for message response
 *  @return		nothing
 */
void
samp_setResponseCallback (handle_t handle, int *func)
{
}


/**
 *  SAMP_DEFAULTREPLYHANDLER -- The interface's default Reply handler.
 *
 *  @brief	The interface's default Reply handler.
 *  @fn		samp_defaultReplyHandler (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_defaultReplyHandler (handle_t handle)
{
}


/**
 *  SAMP_DEFAULTRESPONSEHANDLER -- The interface's default Response handler.
 *
 *  @brief	The interface's default Response handler.
 *  @fn		samp_defaultResponseHandler (handle_t handle)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_deaultfResponseHandler (handle_t handle)
{
}


/**
 *  SAMP_REPLYSTATUS -- Reply with the status of the last message sent.
 *
 *  May be used to 'poll' for a reponse from the caller in cases where
 *  use of a callback is a problem.  Codes are: <0==ERR, 0==PENDING, 1==OK    
 * 
 *  @brief	Reply with the status of the last message sent.
 *  @fn		status = samp_replyStatus (handle_t handle)
 *
 *  @param	samp	samp struct ptr 
 *  @return		message status
 */	    
int
samp_replyStatus  (handle_t handle)
{
    return (SAMP_OK);
}


/**
 *  SAMP_SETTIMEOUT -- Set the message timeout value (in seconds).
 *
 *  @brief	Set the message timeout value (in seconds).
 *  @fn		samp_setTimeout (handle_t handle, int timeout)
 * 
 *  @param  handle	samp handle
 *  @param  name	application name
 *  @return		nothing
 */
void
samp_setTimeout (handle_t handle, int timeout)
{
    Samp *sampP = samp_H2P (handle);
    char  buf[32];

    memset (buf, 0, 32);
    sprintf (buf, "%d", timeout);

    strcpy (sampP->hub->timeout, buf);
}


/**
 *  SAMP_SETAPPNAME -- Set the application name string.
 *
 *  @brief	Set the application name string.
 *  @fn		samp_setAppVersion (handle_t handle, String name)
 * 
 *  @param  handle	samp handle
 *  @param  name	application name
 *  @return		nothing
 */
void
samp_setAppName (handle_t handle, String name)
{
    Samp *sampP = samp_H2P (handle);

    strcpy (sampP->appName, name);
    strcpy (sampP->hub->appName, name);
}


/**
 *  SAMP_SETAPPVERSION -- Set the application version string.
 *
 *  @brief	Set the application version string.
 *  @fn		samp_setAppVersion (handle_t handle, String version)
 * 
 *  @param  handle	samp handle
 *  @param  name	application version
 *  @return		nothing
 */
void
samp_setAppVersion (handle_t handle, String version)
{
    Samp *sampP = samp_H2P (handle);

    strcpy (sampP->appVer, version);
    strcpy (sampP->hub->appVer, version);
}


/**
 *  SAMP_MAPCLIENTS -- Map the public-ids of registered clients to the 
 *  appName.
 */
int
samp_mapClients (handle_t handle)
{
    Samp  *sampP = samp_H2P (handle);	/* get struct pointer	*/
    register int  i = 0;
    char  *pub, rstr[SZ_RESSTR], *sres = rstr;
    List clients;
    Map  resp;


    /*  Get the list of registered clients and their metadata.
     */        
    sampP->nclients = 0;
    clients = samp_GetRegisteredClients (handle);
    for (i=0; i < samp_listLen (clients); i++) {
	pub = samp_getStringFromList (clients, i);
        resp = samp_GetMetadata (handle, pub);
        xr_getStringFromStruct (resp, "samp.name", &sres);

	memset (sampP->clients[i].name, 0, SZ_NAME);
	memset (sampP->clients[i].pubId, 0, SZ_NAME);

	sampP->nclients++;
	strcpy (sampP->clients[i].name, sres);
	strcpy (sampP->clients[i].pubId, pub);
	xr_freeStruct (resp);
    }
    samp_freeList (clients);

    return (SAMP_OK);
}


/**
 *  SAMP_ADDCLIENT -- Add a newly registered client to the list of known
 *  apps so we can do the public-private name translation.
 */
int
samp_addClient (handle_t handle, String name, String id)
{
    Samp  *sampP = samp_H2P (handle);	/* get struct pointer	*/
    register int  i = 0;


    /*  Scan the list of registered clients in case this is a name change.
     */        
    for (i=0; i < sampP->nclients; i++) {
	if (strcasecmp (sampP->clients[i].pubId, id) == 0) 
	    break;
    }

    /*  Otherwise, add new client.
     */
    memset (sampP->clients[i].name, 0, SZ_NAME);
    memset (sampP->clients[i].pubId, 0, SZ_NAME);

    strcpy (sampP->clients[i].name, name);
    strcpy (sampP->clients[i].pubId, id);
    sampP->nclients++;

    return (SAMP_OK);
}


/**
 *  SAMP_ADDCLIENT -- Add a newly registered client to the list of known
 *  apps so we can do the public-private name translation.
 */
int
samp_listClients (handle_t handle)
{
    Samp  *sampP = samp_H2P (handle);	/* get struct pointer	*/
    register int  i = 0;


    for (i=0; i < sampP->nclients; i++)
	printf ("%2d:  PubID = '%s'  Name = '%s'\n",
	    i, sampP->clients[i].pubId, sampP->clients[i].name);

    return (SAMP_OK);
}


/**
 *  SAMP_REMOVECLIENT -- Remove a registered client from the list of known
 *  apps that do the public-private name translation.
 */
int
samp_removeClient (handle_t handle, String id)
{
    Samp  *sampP = samp_H2P (handle);	/* get struct pointer	*/
    register int  i = 0;


    /*  Find the client.
     */        
    for (i=0; i < sampP->nclients; i++) {
	if (strcasecmp (sampP->clients[i].pubId, id) == 0) {
	    if (i == (sampP->nclients - 1)) {	/* last entry, ignore */
	        ;
	    } else {
		/* Shift the list. */
    		for (i=0; i < (sampP->nclients-1); i++) {
	            strcpy (sampP->clients[i].name,sampP->clients[i+1].name);
	            strcpy (sampP->clients[i].pubId,sampP->clients[i+1].pubId);
    		}
	    }
	    memset (sampP->clients[i].name, 0, SZ_NAME);
	    memset (sampP->clients[i].pubId, 0, SZ_NAME);
	    sampP->nclients--;
	    break;
	}
    }

    return (SAMP_OK);
}


/**
 *  SAMP_GETOKMAP -- Generate an 'OK' map we can return to the Hub.
 */
Map
samp_getOKMap ()
{
    Map map;
    map = samp_newMap ();
        samp_setStringInMap (map, "samp.status", "samp.ok");
        samp_setMapInMap (map, "samp.result", nullMap);

    return (map);
}


/**
 *  SAMP_GETNULLMAP -- Generate a 'Null' map we can return to the Hub.
 */
Map
samp_getNullMap ()
{
    return ( samp_newMap() );
}





/******************************************************************************
 *  Internal Interface procedures.
 *****************************************************************************/

/**
 *  SAMP_INITSERVER -- Initialize the server-side of the SAMP.  Create
 *  an XML-RPC server instance, define the commands and start the thread.
 */
static int
samp_initServer (Samp *sampP)
{
    int  handle = samp_P2H(sampP);
    static int initialized = 0;


    if (initialized++)
	return (SAMP_OK);

    if (sampP->trace)
	fprintf (stderr, "Initializing Server Methods:\n");

    /*  Create the server and launch.  Note we never return from this
    **  and make no calls to other servers.
    */
    xr_createServer ("/RPC2", sampP->serverPort, NULL);

    /*  Define the required SAMP client methods.
     *
     *	receiveNotification (string sender-id, map message)
     *	receiveCall (string sender-id, string msg-id, map message)
     *	receiveResponse (string responder-id, string msg-tag, map response)
     */
    xr_addServerMethod ("test.echo", samp_testEcho,   		NULL);

    xr_addServerMethod ("samp.client.receiveCall",
 				samp_receiveCall,   		NULL);
    xr_addServerMethod ("samp.client.receiveNotification",
				samp_receiveNotification,   	NULL);
    xr_addServerMethod ("samp.client.receiveResponse",
 				samp_receiveResponse,   	NULL);

    /*  Define the SAMP message handlers.
    */
    samp_setSampHandler(handle, "samp.app.ping",       samp_PingHandler);
    samp_setSampHandler(handle, "samp.app.status",     samp_StatusHandler);
    samp_setSampHandler(handle, "image.load.fits",     samp_imLoadHandler);
    samp_setSampHandler(handle, "table.load.fits",     samp_tbLoadFITSHandler);
    samp_setSampHandler(handle, "table.load.votable",  samp_tbLoadVOTHandler);
    samp_setSampHandler(handle, "table.highlight.row", samp_tbHighlightHandler);
    samp_setSampHandler(handle, "table.select.rowList",samp_tbSelectHandler);
    samp_setSampHandler(handle, "coord.pointAt.sky",   samp_pointAtHandler);
    samp_setSampHandler(handle, "client.cmd.exec",     samp_cmdExecHandler);
    samp_setSampHandler(handle, "client.env.get",      samp_envGetHandler);
    samp_setSampHandler(handle, "client.env.set",      samp_envSetHandler);
    samp_setSampHandler(handle, "client.param.get",    samp_paramGetHandler);
    samp_setSampHandler(handle, "client.param.set",    samp_paramSetHandler);

    samp_setSampHandler(handle, "spectrum.load",       samp_specLoadHandler);
    samp_setSampHandler(handle, "bibcode.load",        samp_bibcodeHandler);
    samp_setSampHandler(handle, "voresource.loadlist", samp_resLoadHandler);


    /*  Start the server thread.
     */
    sampP->svrThread = (pthread_t) xr_startServerThread ();

    return (SAMP_OK);
}


/**
 *  SAMP_PRINTSUBS -- Print the application subscriptions.
 */
static void 
samp_printSubs (Samp *sampP)
{
    if (!sampP->trace) {
        int i = 0;

        for (i=0; i < sampP->nsubs; i++) {
          fprintf (stderr, "sampStartup: sub[%d] =  %12ld %12ld '%s'\n", i,
	        (long)sampP->subs[i].userFunc, (long)sampP->subs[i].sampFunc, 
	        sampP->subs[i].mtype);
        }
    }
}
