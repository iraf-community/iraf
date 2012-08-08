/**
 *  SAMPCOMMANDS.C -- SAMP commands used by app to send administrative messages.
 *
 *
 *               stat = samp_Register  (samp)
 *             stat = samp_UnRegister  (samp)
 *        stat = samp_DeclareMetadata  (samp)
 *                   stat = samp_Ping  (samp, appName)
 *             map = samp_GetMetadata  (samp, pubId)
 *          samp_DeclareSubscriptions  (samp, subscripMap)
 *        map = samp_GetSubscriptions  (samp)
 *   list = samp_GetRegisteredClients  (samp, mtype)
 *
 *
 *  @brief      SAMP commands used by app to send administrative messages.
 *
 *  @file       sampCommands.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/11
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "samp.h"



/*****************************************************************************
*/

/**
 *  SAMP_REGISTER -- Register with the Hub.  We use the currently stored
 *  metadata.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp)
 *
 *  @param  samp        ...
 *  @return             ...
 */
int
samp_Register (handle_t samp)
{
    char *privateKey, *hubId, *selfId;
    Samp *sampP = samp_H2P(samp);
    Hub  *hub 	= sampP->hub;
    int   reg;


    if (!hub)
	return (SAMP_ERR);

    privateKey = hub->privateKey;
    hubId      = hub->hubId;
    selfId     = hub->selfId;

    xr_initParam (hub->id);
    xr_setStringInParam (hub->id, hub->secret);

    xr_callSync (hub->id, "samp.hub.register");

    /* Check for an error response.
    */
    if (samp_replyStatus (hub->id) != SAMP_OK) {
	fprintf (stderr, "Hub registration failed.\n");
	return (SAMP_ERR);
    }
	

    xr_getStructFromResult (hub->id, &reg);
        xr_getStringFromStruct (reg, "samp.private-key", &privateKey);
        xr_getStringFromStruct (reg, "samp.hub-id", &hubId);
        xr_getStringFromStruct (reg, "samp.self-id", &selfId);

    /* Save the private key we get back.  Note there is a difference in
    ** behavior between the hubs as to whether we strip any prefix on the
    ** key returned in the response.
    */
    strcpy (hub->privateKey, &hub->privateKey[7]);

    return (SAMP_OK);
}


/**
 *  SAMP_UNREGISTER -- Un-Register with the hub.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp)
 *
 *  @param  samp        ...
 *  @return             ...
 */
int
samp_UnRegister (handle_t samp)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;


    if (!hub)
	return (SAMP_ERR);

    xr_initParam (hub->id);

    xr_setStringInParam (hub->id, hub->privateKey);
    xr_callSync (hub->id, "samp.hub.unregister");

    /* Check for error result.
    */
	;

    return (SAMP_OK);
}


/**
 *  SAMP_DECLAREMETATA -- (Re)Declare all of our metadata.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp)
 *
 *  @param  samp        ...
 *  @return             ...
 */
int
samp_DeclareMetadata (handle_t samp)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;
    int   map;


    if (!hub)
	return (SAMP_ERR);

    xr_initParam (hub->id);

    xr_setStringInParam (hub->id, hub->appId);
    map = xr_newStruct ();
        xr_setStringInStruct (map, "samp.name", hub->appName);
        xr_setStringInStruct (map, "samp.description.text", hub->description);
    xr_setStructInParam (hub->id, map);

    xr_callSync (hub->id, "samp.hub.declareMetadata");

    /* ignore result */

    return (SAMP_OK);
}


/**
 *  SAMP_PING --  Ping the hub/app to see if it is alive (returns >0).
 *
 *  @brief      Ping the hub/app to see if it is alive (returns >0).
 *  @fn         handle = samp_Ping (handle_t samp, String appName)
 *
 *  @param  samp        samp struct handle
 *  @param  appName     application name
 *  @return             OK or ERR if no response
 */
int
samp_Ping (handle_t samp, String appName)
{
    Samp *sampP = samp_H2P(samp);
    char  *tag = (char *) NULL;
    int   res = -1;
    Map   resp = (Map) 0;


    if (strncasecmp (appName, "hub", 3) == 0) {
        res = samp_hubPing (sampP->hub);
        return ( samp_hubPing (sampP->hub) );
    } else {
        Msg   msg   = samp_newMsg ();
        Param param = samp_newParam ();

	samp_msgMType (msg, "samp.app.ping");
	samp_msgParam (msg, param);

	tag = samp_msgTag();
        if (strncasecmp (appName, "all", 3) == 0) {
	    /* callAll */
	    resp = samp_callAll (samp, tag, msg);

        } else {
	    /* call    */
	    resp = samp_callAndWait (samp, appName, tag, msg);
	    samp_freeMap (resp);
	}

	samp_freeMsg (msg);
    }

    return (SAMP_ERR);
}


/**
 *  SAMP_GETMETADATA -- Get the metadata for a specified app.
 *
 *  @brief      Get the metadata for a specified app.
 *  @fn         map = samp_GetMetadata (Samp *samp, String pubId)
 *
 *  @param  samp        samp struct handle
 *  @param  pubId       App public-id
 *  @return             Map to message response
 */
Map
samp_GetMetadata (handle_t samp, String pubId)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;
    Map   resp  = (Map) 0;
/*
    Map   err = (Map) 0, snum = (Map) 0, res = (Map) 0;
*/


    if (!hub)
	return ((Map) 0);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);

    if (xr_callSync (hub->id, "samp.hub.getMetadata") == OK) {
        xr_getStructFromResult (hub->id, &resp);
/*
        char  rstr[SZ_RESSTR], *sres = rstr;
        resp = samp_newMap ();
            xr_getStringFromStruct (snum, "samp.status", &sres);
	    if (strcasecmp (sres, "samp.ok") != 0) {
        	xr_freeStruct (snum);
    		return ( (Map) 0 );
	    } else {
                xr_getStructFromStruct (snum, "samp.error", &err);
                xr_getStringFromStruct (snum, "samp.errortxt", &sres);
		memset (sampP->errortxt, 0, SZ_LINE);
		strcpy (sampP->errortxt, sres);

                xr_getStructFromStruct (snum, "samp.result", &res);
	    }
        xr_freeStruct (snum);
*/
        return (resp);
    } else {
	memset (sampP->errortxt, 0, SZ_LINE);
	strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
 
	fprintf (stderr, "Error: '%s'\n", sampP->errortxt);
    }

    return ( (Map) 0 );
}


/**
 *  SAMP_DECLARESUBSCRIPIONS -- Declare the messages we're interested in.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp)
 *
 *  @param  samp        ...
 *  @return             ...
 */
void
samp_DeclareSubscriptions (handle_t samp, Map subscriptions)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;

    if (!hub)
	return;

    xr_initParam (hub->id);

    /*  NYI  */
}


/**
 *  SAMP_GETSUBSCRIPTIONS -- Get the message subscriptions for a specific app.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp, String pubId)
 *
 *  @param  samp        ...
 *  @return             ...
 */
Map
samp_GetSubscriptions (handle_t samp, String pubId)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;
    Map   resp  = (Map) 0;

    if (!hub)
	return ((Map) 0);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);
    if (xr_callSync (hub->id, "samp.hub.getSubscriptions") == OK) {
        xr_getStructFromResult (hub->id, &resp);
        return (resp);
    }

    return ( (Map) 0 );
}


/**
 *  SAMP_GETREGISTEREDCLIENTS -- Get the list of public-ids of the registered
 *  clients.
 *
 *  @brief      ...
 *  @fn         handle = samp_hubOpen (Samp *samp)
 *
 *  @param  samp        ...
 *  @return             ...
 */
List
samp_GetRegisteredClients (handle_t samp)
{
    Samp *sampP = samp_H2P(samp);
    Hub *hub = sampP->hub;
    int res;
    List lres = (List) 0;


    if (!hub)
	return ((List) 0);

    xr_initParam (hub->id);
    xr_setStringInParam (hub->id, hub->privateKey);

    xr_callSync (hub->id, "samp.hub.getRegisteredClients");

    xr_getArrayFromResult (hub->id, &res);

    /*
    lres = samp_newList ();
    for (i=0; i < samp_listLen (res); i++) {
	samp_setStringInList (lres, xr_getStringFromList (list, i);
    }
    */
    lres = res;

    return (lres);
}
