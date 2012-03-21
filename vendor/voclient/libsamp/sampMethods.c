/**
 *  SAMPMETHODS.C -- SAMP methods implemented by a callable client.
 *
 *  @brief      SAMP methods implemented by a callable client.
 *
 *  @file       sampMethods.c
 *  @author     Mike Fitzpatrick
 *  @date       7/10/09
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "samp.h"

#define	METH_DBG   (getenv("METH_DBG")!=NULL||access("/tmp/METH_DBG",F_OK)==0)


extern	Map	   nullMap;
extern	List	   nullList;

extern	Samp	  *sampP;
extern  handle_t   sampH;

#define	OK_Map	   samp_getOKMap()

static	Map msg_reply = (Map) 0;



/**
 *  SAMP_RECEIVECALL -- receiveCall() client method.
 *
 *  @brief      receiveCall () client method.
 *  @fn         int samp_receiveCall (void *data)
 *
 *  @param  data        caller param data
 *  @return             status code or errno
 */
int
samp_receiveCall (void *data)
{
    char  *priv_key = xr_getStringFromParam (data, 0);
    char  *sender   = xr_getStringFromParam (data, 1);
    char  *msg_id   = xr_getStringFromParam (data, 2);
    int    msg_map  = xr_getStructFromParam (data, 3);
    int    params   = -1;
    char   buf[SZ_LINE], *mtype = buf;
    Map    resp;

    void  (*func)(String sender, String mtype, String msgid, Map map);


    xr_getStringFromStruct (msg_map, "samp.mtype", &mtype);
    xr_getStructFromStruct (msg_map, "samp.params", &params);

    if (sampP->trace) {
	fprintf (stderr, "rcvCall(%s) mid='%s' mtype='%s'\n", 
    	    sender, msg_id, mtype);
    }


    /*  Call the default user handler for all messages.  This is in addition
     *  to the handler installed for a particular mtype or Hub event.
     */
    if (sampP && sampP->defaultUserFunc)
        samp_execUserHandler (sender, mtype, 0, params);

    /*  Call the interface's special handler for the mtype.  This allows us
     *  to parse the message map ourselves and present the user with a 
     *  signature for their method that makes more sense.  We assume the
     *  sampHandler calls the userHandler(), if there is no handler defined
     *  then call the userHandler directly and reply() on the behalf of
     *  the user.
     */
    if ( (func = samp_getSampHandler (mtype)) )
	(*func) (sender, mtype, msg_id, params);
    else { 
        if ( (func = samp_getUserHandler (mtype)) )
	    (*func) (sender, mtype, msg_id, params);
	else
	    samp_genericMsgHandler (sender, mtype, msg_id, params);
    }

    resp = OK_Map;
resp = samp_getHandlerReply ();

    samp_Reply (sampH, msg_id, resp);	      /* send message reply   */

    xr_setStructInResult (data, resp);

    xr_freeStruct (msg_map);
    if (priv_key) free ((void *) priv_key);
    if (sender)  free ((void *) sender);
    if (msg_id)  free ((void *) msg_id);

    return (0);
}


/**
 *  SAMP_RECEIVENOTIFICATION -- receiveNotification() client method.
 *
 *  @brief      receiveNotification () client method.
 *  @fn         int samp_receiveNotification (void *data)
 *
 *  @param  data        caller param data
 *  @return             status code or errno
 */
int
samp_receiveNotification (void *data)
{
    char  *priv_key = xr_getStringFromParam (data, 0);
    char  *sender   = xr_getStringFromParam (data, 1);
    int    msg_map  = xr_getStructFromParam (data, 2);
    int    params   = -1;
    char   buf[SZ_LINE], *mtype = buf;
    Map    resp;

    void  (*func)(String sender, String mtype, String msgid, Map map);


    xr_getStringFromStruct (msg_map, "samp.mtype", &mtype);
    xr_getStructFromStruct (msg_map, "samp.params", &params);

    if (sampP->trace)
	fprintf (stderr, "rcvNotify(%s) mtype='%s'\n", sender, mtype);


    /*  Call the default user handler for all messages.  This is in addition
     *  to the handler installe for a particular mtype or Hub event.
     */
    if (sampP && sampP->defaultUserFunc)
        samp_execUserHandler (sender, mtype, 0, params);

    /*  Process any special Hub events as a special case.
     */
    if (samp_processHubEvent (mtype, params))
	goto done_;


    /*  Call the interface's special handler for the mtype.  This allows us
     *  to parse the message map ourselves and present the user with a 
     *  signature for their method that makes more sense.  We assume the
     *  sampHandler calls the userHandler(), if there is no handler defined
     *  then call the userHandler directly, no reply() to the sender is 
     *  required.
     */
    if ( (func = samp_getSampHandler (mtype)) )
	(*func) (sender, mtype, NULL, params);
    else {
	/*
	samp_execUserHandler (sender, mtype, 0, params);
	*/
        if ( (func = samp_getUserHandler (mtype)) )
	    (*func) (sender, mtype, 0, params);
	else
	    samp_genericMsgHandler (sender, mtype, 0, params);
    }

done_:
    resp = OK_Map;
resp = samp_getHandlerReply ();

    xr_setStructInResult (data, resp);

    xr_freeStruct (msg_map);
    if (priv_key) free ((void *) priv_key);
    if (sender)  free ((void *) sender);

    return (0);
}


/**
 *  SAMP_RECEIVERESPONSE -- receiveResponse() client method.
 *
 *  @brief      receiveResponse () client method.
 *  @fn         int samp_receiveResponse (void *data)
 *
 *  @param  data        caller param data
 *  @return             status code or errno
 */
int
samp_receiveResponse (void *data)
{
    char  *priv_key = xr_getStringFromParam (data, 0);
    char  *sender   = xr_getStringFromParam (data, 1);
    char  *msg_tag  = xr_getStringFromParam (data, 2);
    int    msg_map  = xr_getStructFromParam (data, 3);
    Map    resp_map = (Map) -1, params = (Map) 0;
    char   buf[SZ_LINE], *mtype = buf;


    xr_getStringFromStruct (msg_map, "samp.mtype", &mtype);
    xr_getStructFromStruct (msg_map, "samp.params", &resp_map);

    if (sampP->trace) {
	fprintf (stderr, "rcvResponse(%s) id='%s' mtype='%s'\n", 
	    sender, msg_tag, mtype);
    }

    /*  Call the default user handler for all messages.  This is in addition
     *  to the handler installe for a particular mtype or Hub event.
     */
    if (sampP && sampP->defaultUserFunc)
        samp_execUserHandler (sender, mtype, 0, params);


    /*  Reply to the Hub.
     */
    xr_setStructInResult (data, OK_Map);

    xr_freeStruct (msg_map);
    if (priv_key) free ((void *) priv_key);
    if (sender)   free ((void *) sender);
    if (msg_tag)  free ((void *) msg_tag);

    return (0);
}


/**
 *  SAMP_SETHANDLERREPLY -- Set the Samp Handler reply map.
 *
 *  @brief      Set the Samp Handler reply map.
 *  @fn         samp_setHandlerReply (Map resp)
 *
 *  @param  resp        response map
 *  @return             nothing
 */
void
samp_setHandlerReply (Map resp)
{
    msg_reply = resp;
}


/**
 *  SAMP_GETHANDLERREPLY -- Get the Samp Handler reply map.
 *
 *  @brief      Get the Samp Handler reply map.
 *  @fn         int samp_getHandlerReply (void)
 *
 *  @return             Map handle for reply map
 */
Map
samp_getHandlerReply ()
{
    Map resp = msg_reply;

    msg_reply = nullMap;
    return ( (resp != nullMap && resp != 0) ? resp : OK_Map );
}
