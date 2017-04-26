/**
 *  SAMPHUB.C  -- Methods related to the SAMP Hub interface.
 *
 *  @brief      Methods related to the SAMP Hub interface.
 *
 *  @file       sampHub.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/09
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <pthread.h>
#include <ctype.h>

#include "samp.h"

#define  HUB_DBG   (getenv("HUB_DBG")!=NULL||access("/tmp/HUB_DBG",F_OK)==0)




Hub     *hub		= (Hub *) NULL;
int      numHubs        = 0;
handle_t hubHandles[MAX_HUBS];

                                        /** lock so we can update struct  */
pthread_mutex_t samp_mutex = PTHREAD_MUTEX_INITIALIZER;


extern Samp     *sampP;
extern int       numSamps;
extern handle_t  sampHandles[MAX_SAMPS];

extern Map	 nullMap;



/**
 *  Private declarations.
 */
static Hub  *samp_readLockfile ();



/**
 *  SAMP_HUBOPEN -- Discover and open a connection to the SAMP Hub.
 *
 *  @brief	Discover and open a connection to the SAMP Hub.
 *  @fn		handle = samp_hubOpen (Samp *sampP)
 *
 *  @param  samp	Same structure
 *  @return		hub handle
 */
handle_t
samp_hubOpen (Samp *sampP)
{
    /*  Open and read the lockfile.
    */
    if ((hub = samp_readLockfile ()) == (HubP) NULL) 
	return (SAMP_ERR);

    /*  Create a new user handle for the pointer.  Ignore the return, we
     *  just want to create a slot for it.
     */
    (void) samp_newHandle ((void *) hub);

    /*  Fill in known metadata.
    */
    strcpy (hub->meta.name, sampP->appName);
    strcpy (hub->appName, sampP->appName);
    strcpy (hub->appVer, sampP->appVer);
    strcpy (hub->meta.desc, sampP->description);
    strcpy (hub->description, sampP->description);
    strcpy (hub->timeout, DEF_TIMEOUT);
    hub->samp = sampP;				/* set back pointer	*/


    /*  Open the Hub client connection, register and declare metadata. 
    */
    hub->id = xr_initClient (hub->url, sampP->appName, sampP->appVer);

    if (samp_hubRegister (hub) != SAMP_OK) {
        if (HUB_DBG)
	    fprintf (stderr, "Error: hubOpen registration fails: '%s'\n", 
	        xr_getErrMsg (hub->id));
	return (SAMP_ERR);

    } else if (HUB_DBG)
	fprintf (stderr, "hubOpen: registration OK....hub->id = %d\n", hub->id);


    /*  Make the application 'callable'.
     */
    if (samp_hubSetXmlrpcCallback (hub) != SAMP_OK) {
        if (HUB_DBG)
	    fprintf (stderr, "Error: client callability fails: '%s'\n", 
	        xr_getErrMsg (hub->id));
	return (SAMP_ERR);
    } else if (HUB_DBG)
	fprintf (stderr, "hubOpen: Client made callable OK ....\n");


    if (HUB_DBG)
	fprintf (stderr, "hubOpen:  url='%s'  id=%d '%s'\n", 
	    hub->url, hub->id, hub->appId);

    /*  Finally, return the handle to the hub pointer.
    */
    return ( (handle_t) samp_P2H ((void *) hub));
}


/**
 *  SAMP_HUBCLOSE -- Close a connection to the Hub.
 *
 *  @brief	Close a connection to the Hub.
 *  @fn		status = samp_hubClose (handle_t handle)
 *
 *  @param  handle	hub handle
 *  @return		unregister status
 */
int
samp_hubClose (handle_t handle)
{
    Hub *hub = (Hub *) samp_H2P (handle);
    int  status = SAMP_OK;

    status = samp_hubUnRegister (hub);

#ifdef HUB_FREE_META
    if (hub) {
        int n=hub->meta.nkeys;

        for (n=hub->meta.nkeys; n; n--) {
            if (hub->meta.aKey[n]) {
		free ((void *) hub->meta.aKey[n]);
            	hub->meta.aKey[n] = NULL;
	    }
            if (hub->meta.aVal[n]) {
		free ((void *) hub->meta.aVal[n]);
            	hub->meta.aVal[n] = NULL;
	    }
        }
    }
#endif

#ifdef SINGLE_HUB
    if (hub) 
        free ((void *) hub);
    samp_freeHandle (handle);
    hub = NULL;
#endif

    return (status);
}


/**
 *  SAMP_HUBRUNNING -- See whether a Hub is running.  The test simply checks
 *  the existence of a Hub lockfile.
 */
int
samp_hubRunning (void)
{
    char   lockfile[SZ_NAME];

    memset (lockfile, 0, SZ_NAME);
    sprintf (lockfile, "%s/.samp", getenv ("HOME"));	/* MACHDEP	*/

    return ( ((access (lockfile, R_OK) == 0) ? 1 : 0) );
}


/**
 *  SAMP_GETAVAILABLEHUBS -- Get a list of available Hubs
 *
 *  @brief	Get a list of available Hubs
 *  @fn		status = samp_getAvailableHubs (handle_t handle)
 *
 *  @param  handle	hub handle
 *  @return		List of hub handles
 */
List
samp_getAvailableHubs (handle_t handle)
{
    int  status = SAMP_OK;

    /*  NYI */

    return (status);
}


/**
 *  SAMP_GETACTIVEHUB -- Get name of active Hub.
 *
 *  @brief	Get name of active Hub.
 *  @fn		name = samp_hubActiveHub (handle_t handle)
 *
 *  @param  handle	hub handle
 *  @return		handle of currently active Hub
 */
char *
samp_getActiveHubName (handle_t handle)
{
    return ( ((Hub *) samp_H2P (handle))->appId );
}

int
samp_getActiveHub (handle_t handle)
{
    return ( ((Hub *) samp_H2P (handle))->id );
}

/**
 *  SAMP_HUBINIT -- Initialize a hub client connection.
 *
 *  @brief	Initialize a hub client connection.
 *  @fn		status = samp_hubInit (handle_t handle)
 *
 *  @param  handle	samp handle
 *  @param  appName	application name
 *  @param  descr	application description
 *  @return		status
 */
int
samp_hubInit (handle_t handle, char *appName, char *descr)
{
    /*  NYI */

    return (SAMP_OK);
}


/**
 *  SAMP_HUBEVENT -- Determine the type of Hub event mtype.
 *
 *  @brief	Determine the type of Hub event mtype.
 *  @fn		valid = samp_processHubEvent (String mtype, Map params)
 *
 *  @param  mtype	mtype string
 *  @param  params	message parameter Map
 *  @return		non-zero value if a Hub event was processed
 */
int
samp_processHubEvent (String mtype, Map params)
{
    extern Samp *sampP;
    int   sampH = samp_P2H (sampP);
    int   event, lock, isHubEvent = 1;
    static char *id   = (char *) NULL;
    static char *name = (char *) NULL;
    Map   meta;


    if (id == NULL) 			/* initialize 		*/
	id = calloc (1, SZ_LINE);
    if (name == NULL) 
	name = calloc (1, SZ_LINE);


    switch ((event = samp_hubEvent (mtype))) {
    case HUB_SHUTDOWN:
	sampShutdown (sampH);
	break;

    case HUB_REGISTER:
    case HUB_UNREGISTER:
	xr_getStringFromStruct (params, "id", &id);
	if (event == HUB_UNREGISTER) {
            lock = pthread_mutex_lock (&samp_mutex);
	    samp_removeClient (sampH, id);
            lock = pthread_mutex_unlock (&samp_mutex);
	}
	break;

    case HUB_METADATA:
	xr_getStringFromStruct (params, "id", &id);
	xr_getStructFromStruct (params, "metadata", &meta);
	    xr_getStringFromStruct (meta, "samp.name", &name);

        lock = pthread_mutex_lock (&samp_mutex);
	samp_addClient (sampH, name, id);
        lock = pthread_mutex_unlock (&samp_mutex);

	xr_freeStruct (meta);
	break;

    case HUB_DISCONNECT:
	xr_getStringFromStruct (params, "id", &id);
	samp_removeClient (sampH, id);
	break;

    case HUB_SUBSCRIPTIONS:
	/*   nothing to do   */
	break;

    default:
        isHubEvent = 0;
    }

    return (isHubEvent);
}


/**
 *  SAMP_HUBEVENT -- Determine the type of Hub event mtype.
 *
 *  @brief	Determine the type of Hub event mtype.
 *  @fn		code = samp_hubEvent (String mtype)
 *
 *  @param  mtype	mtype string
 *  @return		Hub event code or -1 if not found
 */
int
samp_hubEvent (String mtype)
{
    if (strcasecmp (mtype, "samp.hub.event.shutdown") == 0)
	return (HUB_SHUTDOWN);
    if (strcasecmp (mtype, "samp.hub.event.register") == 0)
	return (HUB_REGISTER);
    if (strcasecmp (mtype, "samp.hub.event.unregister") == 0)
	return (HUB_UNREGISTER);
    if (strcasecmp (mtype, "samp.hub.event.subscriptions") == 0)
	return (HUB_SUBSCRIPTIONS);
    if (strcasecmp (mtype, "samp.hub.event.metadata") == 0)
	return (HUB_METADATA);
    if (strcasecmp (mtype, "samp.hub.event.disconnect") == 0)
	return (HUB_DISCONNECT);

    return (-1);
}



/******************************************************************************
 *  Hub client-side commands 
 *****************************************************************************/

/**
 *  SAMP_HUB_REGISTER -- Send a Register message to the Hub.
 *
 *  @brief	Send a Register message to the Hub.
 *  @fn		status = samp_hubRegister (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubRegister (Hub *hub)
{
    int   reg;
    char *key = hub->privateKey,
	 *hid = hub->hubId,
	 *sid = hub->selfId;


    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, hub->secret);

    if (xr_callSync (hub->id, "samp.hub.register") == OK) {

    	xr_getStructFromResult (hub->id, &reg);
	    xr_getStringFromStruct (reg, "samp.private-key", &key);
	    xr_getStringFromStruct (reg, "samp.hub-id", &hid);
	    xr_getStringFromStruct (reg, "samp.self-id", &sid);

    	if (HUB_DBG) {
	    fprintf (stderr, "hubRegister:  key = '%s'\n", hub->privateKey);
	    fprintf (stderr, "hubRegister:  hubId = '%s'\n", hub->hubId);
	    fprintf (stderr, "hubRegister:  selfId = '%s'\n", hub->selfId);
    	}

	return (SAMP_OK);
    }

    return (SAMP_ERR);
}


/**
 *  SAMP_HUBUNREGISTER -- Send a UnRegister message to the Hub.
 *
 *  @brief	Send a UnRegister message to the Hub.
 *  @fn		status = samp_hubUnRegister (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubUnRegister (Hub *hub)
{
    int status = SAMP_ERR;

    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, hub->privateKey);

    status = xr_callSync (hub->id, "samp.hub.unregister");

    if (HUB_DBG)
	fprintf (stderr, "Closing Hub client....id = %d\n", hub->id);
    xr_closeClient (hub->id);

    return ( (status == OK) ? SAMP_OK : SAMP_ERR );
}


/**
 *  SAMP_HUBSENDSHUTDOWN -- Send a samp.app.event.shutdown message to the Hub.
 *
 *  @brief	Send a samp.app.event.shutdown message to the Hub.
 *  @fn		status = samp_hubSendShutdown (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubSendShutdown (Hub *hub)
{
    int status = SAMP_ERR;
    int async = 0;

    async = xr_newASync (hub->id);	/* set calling parameters	*/
    xr_setStringInParam (async, hub->privateKey);

    status = xr_callASync (async, "samp.app.event.shutdown", samp_nullResponse);


    return ( (status == OK) ? SAMP_OK : SAMP_ERR );
}


/**
 *  SAMP_HUBSETXMLRPCCALLBACK -- Set the client callback and send to Hub
 *
 *  @brief	Set the client callback and send to Hub
 *  @fn		status = samp_hubSetXmlrpcCallback (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */

int    
samp_hubSetXmlrpcCallback (Hub *hub)
{
    int   port;
    char  CB_url[SZ_URL];


    memset (CB_url, 0, SZ_URL);
    port = samp_serverPort ();

    /* Create a server instance so we can reply to messages.
    xr_createServer ("/RPC2", port, NULL);
    */
    sprintf (CB_url, "http://127.0.0.1:%d/RPC2", port);

    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, CB_url);

    if (xr_callSync (hub->id, "samp.hub.setXmlrpcCallback") == OK)
	return (SAMP_OK);

    return (SAMP_ERR);
}


/**
 *  SAMP_HUBPING -- Ping the Hub to see if it is alive.
 *
 *  @brief	Ping the Hub to see if it is alive.
 *  @fn		status = samp_hubPing (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubPing (Hub *hub)
{
    if (!hub)
	return (SAMP_ERR);

    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, hub->privateKey);

    if (xr_callSync (hub->id, "samp.hub.ping") == OK)
	return (SAMP_OK);
    else 
        return (SAMP_ERR);
}


/**
 *  SAMP_DECLAREMETADATA -- Declare "standard" metadata to the Hub.
 *
 *  @brief	Declare "standard" metadata to the Hub.
 *  @fn		status = samp_hubDeclareMetadata (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubDeclareMetadata (Hub *hub)
{
    int    i, stat, map;


    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, &hub->privateKey[0]);
    map = xr_newStruct ();
	xr_setStringInStruct (map, "samp.name", hub->appName);
	xr_setStringInStruct (map, "samp.description.text", hub->description);

	if (hub->meta.iconURL[0])
	    xr_setStringInStruct (map, "samp.icon.url", hub->meta.iconURL);

	if (hub->meta.docURL[0])
	    xr_setStringInStruct (map, "samp.documentation.url", 
		hub->meta.docURL);

	/*  Send remainder of user-defined keys.
	*/
	for (i=0; i < hub->meta.nkeys; i++)	
	    xr_setStringInStruct (map, hub->meta.aKey[i], hub->meta.aVal[i]);

    xr_setStructInParam (hub->id, map);
    stat = xr_callSync (hub->id, "samp.hub.declareMetadata");

    xr_freeStruct (map);

    return ( (stat == OK) ? SAMP_OK : SAMP_ERR);
}


/**
 *  SAMP_HUBDECLARESUBSCRIPTIONS -- Declare mtype subscriptions to the Hub.
 *
 *  @brief	Declare mtype subscriptions to the Hub.
 *  @fn		status = samp_hubDeclareSubscriptions (Hub *hub)
 *
 *  @param  hub		hub struct pointer
 *  @return		status
 */
int
samp_hubDeclareSubscriptions (Hub *hub)
{
    Samp *sampP = (Samp *) hub->samp;
    Map   subs;
    int   i, status;
    char *mtype;


    /*  Create the subscription map, values are the nullMap.
     */
    subs = xr_newStruct ();
    for (i=0; i < sampP->nsubs; i++) {
	mtype = sampP->subs[i].mtype;
	if (sampP->subs[i].userFunc || strncasecmp ("samp.hub",mtype,8) == 0)
	    xr_setStructInStruct (subs, mtype, nullMap);
    }
    xr_setStructInStruct (subs, "samp.app.ping", nullMap);


    xr_initParam (hub->id);		/* set calling parameters	*/
    xr_setStringInParam (hub->id, &hub->privateKey[0]);
    xr_setStructInParam (hub->id, subs);

    status = xr_callSync (hub->id, "samp.hub.declareSubscriptions");

    samp_freeMap (subs);		/* clean up			*/
	
    return ( (status == OK) ? SAMP_OK : SAMP_ERR );
}



/****************************************************************************
**  Private Procedures.
*/

/**
 *  SAMP_READLOCKFILE -- Read the lockfile for a Standard Profile.
 */
static Hub *
samp_readLockfile ()
{
    char   lockfile[SZ_NAME], line[SZ_LINE], *ip=NULL, *key=NULL, *val=NULL;
    FILE  *lck = (FILE *) NULL;


    memset (lockfile, 0, SZ_NAME);
    sprintf (lockfile, "%s/.samp", getenv ("HOME"));	/* MACHDEP	*/

    if (access (lockfile, R_OK) < 0) {
	if (HUB_DBG)
	    fprintf (stderr, "Error: No Hub available\n");
	return ((Hub *) NULL);

    } else if ((lck = fopen (lockfile, "r")) == (FILE *) NULL) {
	fprintf (stderr, "Error: Cannot read SAMP lock file '%s'\n", lockfile);
	return ((Hub *) NULL);
    }

    /* Allocate the Hub structure we'll return.
    */
    if (hub == (HubP) NULL)
	hub = calloc (1, sizeof(Hub));

    while (fgets (line, SZ_LINE, lck)) {
	line[strlen (line)-1] = '\0';		/* kill newline		*/

	/* Skip blank lines and comments.
	*/
	if (line[0] == '\n' || line[0] == '#')
	    continue;
	else {
	    ip = strchr (line, (int)'=');
	    *ip = '\0';
	    key = line;
	    val = ip + 1;

	    if (strcmp (key, "samp.secret") == 0)
		strcpy (hub->secret, val);
	    if (strcmp (key, "samp.hub.xmlrpc.url") == 0)
		strcpy (hub->url, val);
	    if (strcmp (key, "samp.profile.version") == 0)
		strcpy (hub->version, val);

	    if (strcmp (key, "samp.name") == 0) /* Metadata values	*/
		strcpy (hub->meta.name, val);
	    else if (strcmp (key, "samp.description.text") == 0)
		strcpy (hub->meta.desc, val);
	    else if (strcmp (key, "samp.icon.url") == 0)
		strcpy (hub->meta.iconURL, val);
	    else if (strcmp (key, "samp.documentation.url") == 0)
		strcpy (hub->meta.docURL, val);

	    else {
		hub->meta.aKey[hub->meta.nkeys] = calloc (1, strlen(key)+1);
		strcpy (hub->meta.aKey[hub->meta.nkeys], key);

		hub->meta.aVal[hub->meta.nkeys] = calloc (1, strlen(val)+1);
		strcpy (hub->meta.aVal[hub->meta.nkeys], val);

		hub->meta.nkeys++;
	    }
	}
    }

    /* Clean up.
    */
    fclose (lck);

    if (SAMP_TRACE > 1) {
	fprintf (stderr, "readLockFile:  secret = '%s'\n", hub->secret);
	fprintf (stderr, "readLockFile:     url = '%s'\n", hub->url);
	fprintf (stderr, "readLockFile: version = '%s'\n", hub->version);
    }

    return (hub);
}
