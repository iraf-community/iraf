/**
 *  SAMPCOMMANDS.C -- SAMP commands used by app to send administrative messages.
 *
 *
 *               stat = samp_Register  (handle)
 *             stat = samp_UnRegister  (handle)
 *        stat = samp_DeclareMetadata  (handle)
 *                   stat = samp_Ping  (handle, appName)
 *             map = samp_GetMetadata  (handle, pubId)
 *          samp_DeclareSubscriptions  (handle)
 *        map = samp_GetSubscriptions  (handle)
 *   list = samp_GetRegisteredClients  (handle)
 *   list = samp_GetSubscribedClients  (handle, mtype)
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
 *  SAMP_REGISTER -- Register with the Hub using the currently stored metadata.
 *
 *  @brief      Register with the Hub using the currently stored metadata.
 *  @fn         handle = samp_Register (handle_t handle)
 *
 *  @param  handle      samp struct handle
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_Register (handle_t handle)
{
    char *privateKey, *hubId, *selfId;
    Samp *sampP = samp_H2P(handle);
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

    /*  Save the private key we get back.  Note there is a difference in
     *  behavior between the hubs as to whether we strip any prefix on the
     *  key returned in the response.
     * 
     *  FIXME ?? 
     */
    strcpy (hub->privateKey, &hub->privateKey[7]);

    return (SAMP_OK);
}


/**
 *  SAMP_UNREGISTER -- Un-Register from the hub.
 *
 *  @brief      Un-Register from the hub.
 *  @fn         stat = samp_UnRegister (handle_t handle)
 *
 *  @param  handle      samp struct handle
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_UnRegister (handle_t handle)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;

    return ( hub ? samp_hubUnRegister (hub)  : SAMP_ERR );
}


/**
 *  SAMP_DECLAREMETATA -- (Re)Declare all of our metadata.
 *
 *  @brief      (Re)Declare all of our metadata.
 *  @fn         stat = samp_DeclareMetadata (handle_t handle)
 *
 *  @param  handle      samp struct handle
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_DeclareMetadata (handle_t handle)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;

    return ( hub ? samp_hubDeclareMetadata (hub)  : SAMP_ERR );
}


/**
 *  SAMP_PING --  Ping the hub/app to see if it is alive (returns >0).
 *
 *  @brief      Ping the hub/app to see if it is alive (returns >0).
 *  @fn         stat = samp_Ping (handle_t handle, String appName)
 *
 *  @param  handle      samp struct handle
 *  @param  appName     application name
 *  @return             OK or ERR if no response
 */
int
samp_Ping (handle_t handle, String appName)
{
    int   res = -1, status = SAMP_OK;
    Samp *sampP = samp_H2P(handle);
    Map   resp  = (Map) 0;
    char  *tag  = samp_msgTag();
    char  *pubId = samp_app2id (handle, appName);


    if (strcasecmp (appName, "hub") == 0) {
        return ( res = samp_hubPing (sampP->hub) );

    } else {
        Msg   msg   = samp_newMsg ();
        Param param = samp_newParam ();

	samp_msgMType (msg, "samp.app.ping");
	samp_msgParam (msg, param);

        if (strcasecmp (appName, "all") == 0) {
	    resp = samp_callAll (handle, tag, msg);
        } else {
	    resp = samp_callAndWait (handle, pubId, tag, msg);
	    status = samp_setErr (handle, resp);
	}

	samp_freeMap (resp);
	samp_freeMsg (msg);

	return (status);
    }

    return (SAMP_ERR);
}


/**
 *  SAMP_GETMETADATA -- Get the metadata for a specified app.
 *
 *  @brief      Get the metadata for a specified app.
 *  @fn         map = samp_GetMetadata (handle_t handle, String pubId)
 *
 *  @param  handle      samp struct handle
 *  @param  pubId       App public-id
 *  @return             Map to message response
 */
Map
samp_GetMetadata (handle_t handle, String pubId)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub = sampP->hub;
    Map   resp  = (Map) 0;


    if (!hub)
	return ((Map) 0);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);

    if (xr_callSync (hub->id, "samp.hub.getMetadata") == OK) {
        xr_getStructFromResult (hub->id, &resp);
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
 *  @brief      Declare the messages we're interested in.
 *  @fn         stat = samp_DeclareSubscriptions (handle_t handle)
 *
 *  @param  handle      samp struct handle
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_DeclareSubscriptions (handle_t handle)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;

    return ( hub ? samp_hubDeclareSubscriptions (hub)  : SAMP_ERR );
}


/**
 *  SAMP_GETSUBSCRIPTIONS -- Get the message subscriptions for a specific app.
 *
 *  @brief      Get the message subscriptions for a specific app.
 *  @fn         subs[] = samp_GetSubscriptions (handle_t handle, String pubId)
 *
 *  @param  handle      samp struct handle
 *  @param  pubId       public ID of target app
 *  @return             Array of target subscriptions
 */
Map
samp_GetSubscriptions (handle_t handle, String pubId)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub = sampP->hub;
    Map   resp  = (Map) 0;

    if (!hub)
        return ( (Map) 0);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);
    if (xr_callSync (hub->id, "samp.hub.getSubscriptions") == OK) {
        xr_getStructFromResult (hub->id, &resp);

        return ( (Map) 0);
    }

    return ( (Map) 0 );
}


/**
 *  SAMP_GETREGISTEREDCLIENTS -- Get public-ids of the registered clients.
 *
 *  @brief      Get public-ids of the registered clients.
 *  @fn         handle = samp_GetRegisteredClients (handle_t handle)
 *
 *  @param  handle      samp struct handle
 *  @return             handle to list of registered clients
 */
List
samp_GetRegisteredClients (handle_t handle)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;
    int  i, res;
    List lres = (List) 0;
    char  name[64], *str = name;


    if (!hub)
	return ((List) 0);

    xr_initParam (hub->id);
    xr_setStringInParam (hub->id, hub->privateKey);
    if (xr_callSync (hub->id, "samp.hub.getRegisteredClients") == OK) {
        xr_getArrayFromResult (hub->id, &res);

        lres = samp_newList ();
        for (i=0; i < samp_listLen (res); i++) {
	    xr_getStringFromArray (res, i, &str);
	    samp_setStringInList (lres, str);

	    //free ((void *) str);
        }
    } else {
        lres = samp_newList ();
	samp_setStringInList (lres, "");
    }

    return (lres);
}


/**
 *  SAMP_GETSUBSCRIBEDCLIENTS -- Get clients matching the mtype subscription.
 *
 *  @brief      Get clients matching the mtype subscription.
 *  @fn         list = samp_GetSubscribedClients (handle_t handle, String mtype)
 *
 *  @param  handle      samp struct handle
 *  @param  mtype       mtype string
 *  @return             handle to list of clients having mtype subscription
 */
List
samp_GetSubscribedClients (handle_t handle, String mtype)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;
    int  i, res;
    List lres = (List) 0;
    char  name[64], *str = name;


    if (!hub)
	return ((List) 0);

    xr_initParam (hub->id);
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, mtype);

    xr_callSync (hub->id, "samp.hub.getSubscribedClients");

    xr_getArrayFromResult (hub->id, &res);

    lres = samp_newList ();
    for (i=0; i < samp_listLen (res); i++) {
	xr_getStringFromArray (res, i, &str);
	samp_setStringInList (lres, str);
    }

    return (lres);
}
