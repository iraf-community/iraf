/**
 *  SAMPCLIENT.C -- Client callable methods.
 *
 *                 samp_notify  (handle, recipId, msg_map)
 *       list = samp_notifyAll  (handle, msg_map)
 *            str =  samp_call  (handle, recipId, tag, msg_map)
 *         map =  samp_callAll  (handle, msg_tag, msg_map)
 *     map =  samp_callAndWait  (handle, recipId, msg_tag, msg_map)
 *           stat = samp_Reply  (handle, msg_id, resp_map)
 *
 *       str = samp_clientName  (handle, pubId)
 *          stat = samp_setErr  (handle, resp_map)
 *           str = samp_getErr  (handle)
 *  
 *  
 *  @brief      Client callable methods.
 *  
 *  @file       sampClient.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/11
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include "samp.h"



/**
 *  SAMP_NOTIFY -- Make a notify() call to the Hub
 *
 *  @brief	Make a notify() call to the Hub
 *  @fn		void samp_notify (handle_t handle, String recipId, Map msg)
 *
 *  @param   handle	samp struct handle
 *  @param   recipId    recipient ID
 *  @param   msg        message map
 *  @return 		nothing
 */
void
samp_notify (handle_t handle, String recipId, Map msg)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;
#ifdef USE_ASYNC
    int   async = 0;
#endif
    char *pubId = samp_app2id (handle, recipId);

    if (!hub)
	return;

    /*  Format the message.
     */
#ifdef USE_ASYNC
    async = xr_newASync (hub->id);
        xr_setStringInParam (async, hub->privateKey);
        xr_setStringInParam (async, pubId);
        xr_setStructInParam (async, msg);

    /*  Send it to the Hub, ignore any reply.
     */
    xr_callASync (async, "samp.hub.notify", samp_nullResponse);
    xr_closeClient (async);

#else
    xr_initParam (hub->id);
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);
    xr_setStructInParam (hub->id, msg);

    /*  Send it to the Hub, ignore any reply.
     */
    if (xr_callSync (hub->id, "samp.hub.notify") == OK )
        return;
#endif

    return;
}


/**
 *  SAMP_NOTIFYALL -- Make a notifyAll() call to the Hub
 *
 *  @brief	Make a notifyAll() call to the Hub
 *  @fn		void samp_notifyAll (handle_t handle, Map msg)
 *
 *  @param   handle	samp struct handle
 *  @param   msg        message map
 *  @return 		nothing
 */
List
samp_notifyAll (handle_t handle, Map msg)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;

    if (!hub)
	return ( (List) 0);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStructInParam (hub->id, msg);

    if (xr_callSync (hub->id, "samp.hub.notifyAll") == OK ) {
	int res = 0;

	/* Get list of recipient ids  */
        return (res);
    } else { 
	strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
	if (sampP->debug || sampP->trace)
	    fprintf (stderr, "%s\n", sampP->errortxt);
	return ((List) 0);
    }

    return ( (List) 0);
}


/**
 *  SAMP_CALL -- Make a call() call to the Hub
 *
 *  @brief	Make a call() call to the Hub
 *  @fn		String msg-id = samp_call (handle_t handle, String recipId, 
 *				    String msg_tag, Map msg)
 *
 *  @param   handle	samp struct handle
 *  @param   recipId    recipient ID
 *  @param   msg_tag    message tag
 *  @param   msg        message map
 *  @return 		message ID
 */
String
samp_call (handle_t handle, String recipId, String msg_tag, Map msg)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;
    char *pubId = samp_app2id (handle, recipId);


    if (!hub)
	return (NULL);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);
    xr_setStringInParam (hub->id, msg_tag);
    xr_setStructInParam (hub->id, msg);

    memset (sampP->errortxt, 0, SZ_LINE);
    if (xr_callSync (hub->id, "samp.hub.call") == OK ) {
        char  rstr[SZ_RESSTR], *sres = rstr;

        xr_getStringFromResult (hub->id, &sres);
        return (sres);
    } else { 
	strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
	if (sampP->debug || sampP->trace)
	    fprintf (stderr, "%s\n", sampP->errortxt);
    }

    return (NULL);
}


/**
 *  SAMP_CALLALL -- Make a callAll() call to the Hub
 *
 *  @brief	Make a callAll() call to the Hub
 *  @fn		Map calls = samp_callAll (handle_t handle, String msg_tag, 
 *				Map msg)
 *
 *  @param   handle	samp struct handle
 *  @param   msg_tag    message tag
 *  @param   msg        message map
 *  @return 		SAMP_OK or SAMP_ERR
 */
int 
samp_callAll (handle_t handle, String msg_tag, Map msg)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;
    Map   resp  = (Map) 0;
    int   status = SAMP_OK;


    if (!hub)
        return (status);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, msg_tag);
    xr_setStructInParam (hub->id, msg);

    if (xr_callSync (hub->id, "samp.hub.callAll") == OK ) {
	/*  Save the response to a new map so we can free the msg result.
	 */
        xr_getStructFromResult (hub->id, &resp);
        status = samp_setErr (handle, resp);          /* save the result   */
    } else { 
	strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
	if (sampP->debug || sampP->trace)
	    fprintf (stderr, "%s\n", sampP->errortxt);
	return (SAMP_ERR);
    }

    return (status);
}


/**
 *  SAMP_CALLANDWAIT -- Make a callAndWait() call to the Hub
 *
 *  @brief	Make a callAndWait() call to the Hub
 *  @fn		status = samp_callAndWait (handle_t handle, String recipId, 
 *			String msg_tag, Map msg)
 *
 *  @param   handle	samp struct handle
 *  @param   recipId    recipient ID
 *  @param   msg_tag    message tag
 *  @param   msg        message map
 *  @return 		SAMP_OK or SAMP_ERR
 */
int
samp_callAndWait (handle_t handle, String recipId, String msg_tag, Map msg)
{
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;
    char *pubId = samp_app2id (handle, recipId);
    Map   resp  = (Map) 0;
    int   status = SAMP_OK;


    if (!hub)
        return (status);

    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, hub->privateKey);
    xr_setStringInParam (hub->id, pubId);
    xr_setStructInParam (hub->id, msg);
    xr_setStringInParam (hub->id, hub->timeout);

    if (xr_callSync (hub->id, "samp.hub.callAndWait") == OK ) {
	/*  Save the response to a new map so we can free the msg result.
	 */
        xr_getStructFromResult (hub->id, &resp);
        status = samp_setErr (handle, resp);          /* save the result   */
    } else { 
	strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
	if (sampP->debug || sampP->trace)
	    fprintf (stderr, "%s\n", sampP->errortxt);
	return (SAMP_ERR);
    }

    return (status);
}


/**
 *  SAMP_REPLY -- Reply to a message.  All we do here is send the reply
 *  message, we require that the response Map already be defined.
 *
 *  @brief	Reply to a message.
 *  @fn		status = samp_Reply (handle_t handle, String msg_id, 
 *  			Map response), 
 *
 *  @param   handle	samp struct handle
 *  @param   msg_id     message ID
 *  @param   response   response map
 *  @return 		SAMP_OK or SAMP_ERR
 */
int
samp_Reply (handle_t handle, String msg_id, Map response)
{
    Samp *sampP = samp_H2P(handle);
    Hub *hub = sampP->hub;
#ifdef USE_ASYNC
    int  async;
#endif


    if (!hub)
        return (SAMP_ERR);

    /*  Format the message.
     */
#ifdef USE_ASYNC
    async = xr_newASync (hub->id);
        xr_setStringInParam (async, sampP->hub->privateKey);
        xr_setStringInParam (async, msg_id);
        xr_setStructInParam (async, response);

    /*  Send it to the Hub.
     */
    xr_callASync (async, "samp.hub.reply", samp_nullResponse);

    xr_closeClient (async);

#else
    xr_initParam (hub->id);             /* set calling parameters       */
    xr_setStringInParam (hub->id, sampP->hub->privateKey);
    xr_setStringInParam (hub->id, msg_id);
    xr_setStructInParam (hub->id, response);

    /*  Send it to the Hub.
     */
    xr_callSync (hub->id, "samp.hub.reply");
#endif
    return (SAMP_OK);
}




/******************************************************************************
 *  Utility Client Functions
 *****************************************************************************/

/**
 *  SAMP_CLIENTNAME -- Get the Client name from a pubic-id.
 *
 *  @brief	Get the Client name from a pubic-id.
 *  @fn		name = samp_clientName (handle_t handle, String pubId)
 *
 *  @param   handle	samp struct handle
 *  @param   pubId      public ID
 *  @return 		declared application name
 */
String  
samp_clientName (handle_t handle, String pubId)
{
/*
    Samp *sampP = samp_H2P(handle);
    Hub  *hub   = sampP->hub;

    if (!hub)
        return ((String) NULL);
*/

    return ((String) NULL);
}


/**
 *  SAMP_SETERR -- Set the error response string/code.
 *
 *  @brief	Set the error response string.
 *  @fn		stat = samp_setErr (handle_t handle, Map resp)
 *
 *  @param   handle	samp struct handle
 *  @param   resp       Response map
 *  @return 		SAMP_OK or SAMP_ERR
 */
int  
samp_setErr (handle_t handle, Map resp)
{
    char   rstr[SZ_RESSTR], *sres = rstr;
    Samp *sampP = samp_H2P(handle);
    Map   err = (Map) 0;


    if (resp < 0)
	return (SAMP_ERR);

    memset (sampP->errortxt, 0, SZ_LINE);

    xr_getStringFromStruct (resp, "samp.status", &sres);
    if (!sres[0])  {
	xr_getStringFromStruct (resp, "faultString", &sres);
        strcpy (sampP->errortxt, sres);
        return (SAMP_ERR);

    } else if (strcasecmp (sres, "samp.ok") == 0) {
	return (SAMP_OK);

    } else if (strcasecmp (sres, "samp.warning") == 0) {
	;	/* NYI */

    } else if (strcasecmp (sres, "samp.error") == 0) {
        xr_getStructFromStruct (resp, "samp.error", &err);
	if (err)
            xr_getStringFromStruct (err, "samp.errortxt", &sres);
	else {
	    memset (sres, 0, SZ_RESSTR);
	    strcpy (sres, "unknown error");
	}

        strcpy (sampP->errortxt, sres);

	if (err)
            xr_freeStruct (err);
        return (SAMP_ERR);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_GETERR -- Get the error response string.
 *
 *  @brief	Get the error response string.
 *  @fn		samp_getErr (handle_t handle)
 *
 *  @param   handle	samp struct handle
 *  @return 		Error text string
 */
String
samp_getErr (handle_t handle)
{
    Samp *sampP = samp_H2P(handle);

    /*  Remove any pre-amble text from the error string.
     */
    return ( strrchr (sampP->errortxt, (int)':') + 1);
}
