/**
 *  SAMPHANDLERS.C -- Message handlers for the interface.
 *
 *  @brief      Message handlers for the interface.
 *
 *  @file       sampHandlers.c
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




/**
 *  SAMP_SETUSERHANDLER -- Set the user-interface handler for the mtype.
 *
 *  @brief      Set the user-interface handler for the mtype.
 *  @fn         samp_setUserHandler (handle_t handle, String mtype, void *func)
 *
 *  @param handle       user handle to samp struct
 *  @param mtype        mtype name
 *  @param func         callback function
 *  @return             nothing
 */
void
samp_setUserHandler (handle_t handle, String mtype, void *func)
{
    Samp *sampP = samp_H2P (handle);
    register int i;


    /*  Create the default user handler for a completely generic subscription.
     *	return;
     */
    if (mtype[0] == '*')
	sampP->defaultUserFunc = func;

    /*  Search first through the list of mtypes supported by the interface
     *  that may have handlers.  These handlers will parse the message map
     *  and call the user function with a simpler interface (e.g. call with
     *  the URL argument instead of the rawmessage map.
     */
    for (i=0; i < sampP->nsubs; i++) {
        if (strcasecmp (mtype, sampP->subs[i].mtype) == 0) {
            sampP->subs[i].userFunc = func;
            return;
        }
    }

    /*  This is a new mtype handler.
     */
    strcpy (sampP->subs[sampP->nsubs].mtype, mtype);
    sampP->subs[sampP->nsubs].userFunc = func;
    sampP->nsubs++;
}


/**
 *  SAMP_SETSAMPHANDLER -- Set the SAMP interface handler for the mtype.
 *
 *  @brief      Set the SAMP interface handler for the mtype.
 *  @fn         samp_setSampHandler (handle_t handle, String mtype, void *func)
 *
 *  @param handle       user handle to samp struct
 *  @param mtype        mtype name
 *  @param func         callback function
 *  @return             nothing
 */
void
samp_setSampHandler (handle_t handle, String mtype, void *func)
{
    Samp *sampP = samp_H2P (handle);
    register int i;


    /*  Search first through the list of mtypes supported by the interface
     *  that may have handlers.  These handlers will parse the message map
     *  and call the user function with a simpler interface (e.g. call with
     *  the URL argument instead of the rawmessage map.
     */
    for (i=0; i < sampP->nsubs; i++) {
        if (strcasecmp (mtype, sampP->subs[i].mtype) == 0) {
            sampP->subs[i].sampFunc = func;
            return;
        }
    }

    /*  This is a new mtype handler.
     */
    strcpy (sampP->subs[sampP->nsubs].mtype, mtype);
    sampP->subs[sampP->nsubs].sampFunc = func;
    sampP->nsubs++;
}


/**
 *  SAMP_GETUSERHANDLER -- Get the User handler for the named mtype.
 *
 *  @brief      Get the User handler for the named mtype.
 *  @fn         func = samp_getUserHandler (String mtype)
 *
 *  @param mtype        mtype string
 *  @return             nothing
 */
void *
samp_getUserHandler (String mtype)
{
    extern Samp *sampP;
    register int i;


    /*  Search the subscriptions for the specific mtype.
     */
    for (i=0; i < sampP->nsubs; i++) {
        if (strcasecmp (mtype, sampP->subs[i].mtype) == 0)
            return (sampP->subs[i].userFunc);
    }

    /*  If we get here, there is no mtype-specific handler, return the
     *  default handler for a generic mtype request.
     */
    if (strncmp (mtype, "*", 1) == 0)
	return (sampP->defaultUserFunc);

    return ( (void *) NULL );
}


/**
 *  SAMP_GETSAMPHANDLER -- Get the Samphandler for the named mtype.
 *
 *  @brief      Get the Samphandler for the named mtype.
 *  @fn         func = samp_getSampHandler (String mtype)
 *
 *  @param mtype        mtype string
 *  @return             nothing
 */
void *
samp_getSampHandler (String mtype)
{
    extern Samp *sampP;
    register int i, len;

    for (i=0; i < sampP->nsubs; i++) {
	len = min (strlen(mtype), strlen(sampP->subs[i].mtype));
        if (strncasecmp (mtype, sampP->subs[i].mtype, len) == 0)
            return (sampP->subs[i].sampFunc);
    }

    return ( (void *) NULL );
}


/**
 *  SAMP_EXECUSERHANDLER -- Execute the user-defined handler for the mtype.
 *
 *  @brief      Execute the user-defined handler for the mtype.
 *  @fn         samp_execUserHandler (String sender, String mtype,
 *			String msg_id, Map params)
 *
 *  @param sender       sender name
 *  @param mtype        mtype string
 *  @param msg_id       message-id string
 *  @param params       Mtype parameter Map
 *  @return             nothing
 */

#define	MATCH(s)	(func&&strncasecmp(mtype,s,min(len,16))==0)

void
samp_execUserHandler (String sender, String mtype, String msg_id, Map params)
{
    extern Samp *sampP;
    char   s1[SZ_NAME], s2[SZ_NAME], s3[SZ_NAME];
    char  *meta = NULL;
    int    ival1, len  = 0, slen = SZ_NAME;
    double dval1, dval2;
    void  (*func)();


    memset (s1, 0, SZ_NAME); 			/* initialize		*/
    memset (s2, 0, SZ_NAME); 
    memset (s3, 0, SZ_NAME);

    /*  See if we are going to match multiple mtypes.
     */
    if ( (meta = strchr (mtype, (int)'*')) )
	*meta = '\0';
    len = strlen (mtype);

    /*  Get the user handler pointer.
     */
    func = samp_getUserHandler (mtype);


    /*  Process the mtype.
     */
    if (MATCH ("samp.app.ping")) {
        (*func) (sender);

    } else if (MATCH ("samp.app.status")) {
        (*func) (sender);

    } else if (MATCH ("samp.app.event.*")) {		/*  no-op  */
	;

    } else if (MATCH ("samp.hub.event.*")) {		/*  no-op  */
	;

						 /***********************
						 ***   Table MTypes   ***
						 ***********************/
    } else if (MATCH ("table.load.fits")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, s3, strlen(s1), strlen(s2), strlen(s3));
	else
            (*func) (s1, s2, s3);

    } else if (MATCH ("table.load.votable")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, s3, strlen(s1), strlen(s2), strlen(s3));
	else
            (*func) (s1, s2, s3);

    } else if (MATCH ("table.highlight.row")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
	ival1 = samp_getIntFromMap (params, "row");

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, &ival1, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2, ival1);
	
    } else if (MATCH ("table.select.rowList")) {
	int   i, listlen, *rows;
	List  rowlist;

        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
	rowlist = samp_getListFromMap (params, "row-list");

	listlen = samp_listLen (rowlist);
	rows = calloc (1, listlen * sizeof(int));
	for (i=0; i < listlen; i++)
	    rows[i] = samp_getIntFromList (rowlist, i);

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, rows, &listlen, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2, rows, listlen);
	
	free ((void *) rows);
						 /***********************
						 ***   Image MTypes   ***
						 ***********************/
    } else if (MATCH ("image.load.fits")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "image-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, s3, strlen(s1), strlen(s2), strlen(s3));
	else
            (*func) (s1, s2, s3);
						 /***********************
						 ***   Coord MTypes   ***
						 ***********************/
    } else if (MATCH ("coord.pointAt.sky")) {
	dval1 = (double) samp_getFloatFromMap (params, "ra");
	dval2 = (double) samp_getFloatFromMap (params, "dec");

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (&dval1, &dval2);
	else
            (*func) (dval1, dval2);
						 /***********************
						 ***   Client MTypes  ***
						 ***********************/
    } else if (MATCH ("client.env.get")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, &slen, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2, slen);

    } else if (MATCH ("client.env.set")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
        strcpy (s2, samp_getStringFromMap (params, "value"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2);


    } else if (MATCH ("client.param.get")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, &slen, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2, slen);

    } else if (MATCH ("client.param.set")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
        strcpy (s2, samp_getStringFromMap (params, "value"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, strlen(s1), strlen(s2));
	else
            (*func) (s1, s2);
						 /***********************
						 ***  Bibcode MTypes  ***
						 ***********************/
    } else if (MATCH ("bibcode.load")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, strlen (s1));
	else
            (*func) (s1);
						 /***********************
						 ***  Spectrum MTypes ***
						 ***********************/
    } else if (MATCH ("spectrum.load.ssa-generic")) {
	Map  meta; 
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "spectrum-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));
  	meta = samp_getMapFromMap (params, "meta");
	
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, s2, s3, meta, strlen(s1), strlen(s2), strlen(s3));
	else
            (*func) (s1, s2, s3, meta);

						 /***********************
						 ***  Resource MTypes ***
						 ***********************/
    } else if (MATCH ("voresource.loadlist.")) {
	Map  idmap; 

        strcpy (s1, samp_getStringFromMap (params, "name"));
  	idmap = samp_getMapFromMap (params, "ids");

	if (sampP->handlerMode == SAMP_CBR)
            (*func) (s1, idmap, strlen(s1));
	else
            (*func) (s1, idmap);

						 /***********************
						 ***  Generic MTypes  ***
						 ***********************/
    } else {
	/* Call the generic handler.  The signature for this method is 
	 * required to be:
	 *
	 *     func (String sender, String mtype, String msg_id, int params)
	 */
	if (sampP->defaultUserFunc) {
	    func = (void *) sampP->defaultUserFunc;
	    if (sampP->handlerMode == SAMP_CBR) {
                (*func) (sender, mtype, msg_id, &params, 
		    strlen(mtype), strlen(sender), strlen(msg_id));
	    } else
                (*func) (sender, mtype, msg_id, params);

	} else
            samp_genericMsgHandler (sender, mtype, msg_id, params);
    }
}



/****************************************************************************
 ****************************************************************************
 **
 **  MType-specific SAMP handlers.  These methods allow us to wrap the user
 **  method so we can unpack the message map and allow the user's handler
 **  to have a more sensible signature.
 **
 ****************************************************************************
 ***************************************************************************/


/**
 *  SAMP_HUBHANDLER -- Handle Hub event messages.
 *
 *  @brief      Handle Hub event messages.
 *  @fn         int samp_hubHandler (String sender, String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_hubHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    /*  Process any special Hub events as a special case.
    return (samp_processHubEvent (mtype, params));
     */
    return (0);
}


/**
 *  SAMP_PINGHANDLER -- Simple aliveness test function.
 *
 *  @brief      Is app alive and responding to messages?
 *  @fn         int samp_PingHandler (String sender, String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_PingHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    void  (*func) ();

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("samp.app.ping")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (sender, strlen (sender));
	else
            (*func) (sender);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_STATUSHANDLER -- Return status of the task.
 *
 *  @brief       Return status of the task.
 *  @fn          int samp_StatusHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_StatusHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    void  (*func) ();

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("samp.app.status")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (sender, strlen (sender));
	else
            (*func) (sender);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_GENERICMSGHANDLER -- Handle an arbitrary message.
 *
 *  @brief      Handle an arbitrary message.
 *  @fn         int samp_genericMsgHandler (String sender, String mtype,
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  mtype       message type
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_genericMsgHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    void  (*func) ();


    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler (mtype)) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (sender, mtype, msg_id, &msg_map, 
		strlen(sender), strlen(mtype), strlen (msg_id));
        else
            (*func) (sender, mtype, msg_id, msg_map);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_IMLOADHANDLER -- Handle an image.load.fits message.
 *
 *  @brief      Handle an image.load.fits message.
 *  @fn         int samp_imLoadHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_imLoadHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], imId[SZ_NAME], name[SZ_NAME];
    void  (*func) ();


    strcpy (url,  samp_getStringFromMap (msg_map, "url"));
    strcpy (imId, samp_getStringFromMap (msg_map, "image-id"));
    strcpy (name, samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("image.load.fits")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, imId, name, strlen(url), strlen(imId), strlen(name));
        else
            (*func) (url, imId, name);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_TBLOADHANDLER -- Handle a generic table.load.* message
 *
 *  @brief       Handle a generic table.load.* message
 *  @fn          int samp_tbLoadHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tbLoadHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], tblId[SZ_NAME], name[SZ_NAME];
    void  (*func) ();


    strcpy (url,   samp_getStringFromMap (msg_map, "url"));
    strcpy (tblId, samp_getStringFromMap (msg_map, "table-id"));
    strcpy (name,  samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.load.*")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, tblId, name, strlen(url), strlen(tblId), 
		strlen(name));
        else
            (*func) (url, tblId, name);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_TBLOADFITSHANDLER -- Handle a generic table.load.fits message
 * *  @brief       Handle a generic table.load.fits message
 *  @fn          int samp_tbLoadFITSHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tbLoadFITSHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], tblId[SZ_NAME], name[SZ_NAME];
    void  (*func) ();


    strcpy (url,   samp_getStringFromMap (msg_map, "url"));
    strcpy (tblId, samp_getStringFromMap (msg_map, "table-id"));
    strcpy (name,  samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.load.fits")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, tblId, name, strlen(url), strlen(tblId), 
		strlen(name));
        else
            (*func) (url, tblId, name);
    }

    /*  Call the generic table handler version.
     */
    samp_tbLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_TBLOADVOTHANDLER -- Handle a generic table.load.votable message
 *
 *  @brief       Handle a generic table.load.votable message
 *  @fn          int samp_tbLoadVOTHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tbLoadVOTHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], tblId[SZ_NAME], name[SZ_NAME];
    void  (*func) ();


    strcpy (url,   samp_getStringFromMap (msg_map, "url"));
    strcpy (tblId, samp_getStringFromMap (msg_map, "table-id"));
    strcpy (name,  samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.load.votable")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, tblId, name, strlen(url), strlen(tblId), 
		strlen(name));
        else
            (*func) (url, tblId, name);
    }

    /*  Call the generic table handler version.
     */
    samp_tbLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_TBHIGHLIGHTHANDLER -- Handle a table.highlight.row message
 *
 *  @brief       Handle a table.highlight.row message
 *  @fn          int samp_tbHighlightHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tbHighlightHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], tblId[SZ_NAME];
    int	   row;
    void  (*func) ();


    strcpy (url,   samp_getStringFromMap (msg_map, "url"));
    strcpy (tblId, samp_getStringFromMap (msg_map, "table-id"));
    row = samp_getIntFromMap (msg_map, "row");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.highlight.row")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, tblId, &row, strlen(url), strlen(tblId));
        else
            (*func) (url, tblId, row);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_TBSELECTHANDLER -- Handle a table.select.rowList message
 *
 *  @brief       Handle a table.select.rowList message
 *  @fn          int samp_tbSelectHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tbSelectHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   url[SZ_URL], tblId[SZ_NAME];
    int	   *rowList = (int *) NULL, nrows = 0;
    List   rlist = (List) 0;
    void  (*func) ();


    strcpy (url,   samp_getStringFromMap (msg_map, "url"));
    strcpy (tblId, samp_getStringFromMap (msg_map, "table-id"));

    rlist = samp_getListFromMap (msg_map, "row-list");
    nrows = samp_listLen (rlist);
    rowList = calloc (nrows, sizeof (int));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.select.rowList")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (url, tblId, rowList, &nrows, strlen(url), strlen(tblId));
        else
            (*func) (url, tblId, rowList, nrows);
    }

    free ((void *) rowList);
    return (SAMP_OK);
}


/**
 *  SAMP_POINTATHANDLER -- Handle a coord.pointAt.sky message
 *
 *  @brief       Handle a coord.pointAt.sky message
 *  @fn          int samp_pointAtHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_pointAtHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    float  ra, dec;
    void  (*func) ();


    ra  = samp_getFloatFromMap (msg_map, "ra");
    dec = samp_getFloatFromMap (msg_map, "dec");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("table.select.rowList")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (&ra, &dec);
	else
            (*func) (ra, dec);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_SPECLOADHANDLER -- Handle a spectrum.load.* message
 *
 *  @brief       Handle a spectrum.load.* message
 *  @fn          int samp_specLoadHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_specLoadHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  url[SZ_URL], specId[SZ_NAME], name[SZ_NAME];
    Map   meta = (Map) 0;
    void  (*func) ();


    strcpy (url, samp_getStringFromMap (msg_map, "url"));
    strcpy (name, samp_getStringFromMap (msg_map, "name"));
    strcpy (specId, samp_getStringFromMap (msg_map, "spectrum-id"));
    meta = samp_getMapFromMap (msg_map, "meta");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("spectrum.load.*")) ) {
	if (sampP->handlerMode == SAMP_CBR) {
            (*func) (url, specId, name, &meta, 
		strlen(url), strlen(specId), strlen(name));
	} else
            (*func) (url, specId, name, meta);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_SPECSSAHANDLER -- Handle a spectrum.load.ssa-generic message
 *
 *  @brief       Handle a spectrum.load.ssa-generic message
 *  @fn          int samp_specSSAHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_specSSAHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  url[SZ_URL], specId[SZ_NAME], name[SZ_NAME];
    Map   meta = (Map) 0;
    void  (*func) ();


    strcpy (url, samp_getStringFromMap (msg_map, "url"));
    strcpy (name, samp_getStringFromMap (msg_map, "name"));
    strcpy (specId, samp_getStringFromMap (msg_map, "spectrum-id"));
    meta = samp_getMapFromMap (msg_map, "meta");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("spectrum.load.ssa-generic")) ) {
	if (sampP->handlerMode == SAMP_CBR) {
            (*func) (url, specId, name, &meta, 
		strlen(url), strlen(specId), strlen(name));
	} else
            (*func) (url, specId, name, meta);
    }

    /*  Call the generic table handler version.
     */
    samp_specLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_CMDEXECHANDLER -- Handle a client.cmd.exec message
 *
 *  @brief       Handle a client.cmd.exec message
 *  @fn          int samp_cmdExecHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_cmdExecHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   cmd[SZ_CMD];
    void  (*func) ();


    strcpy (cmd,  samp_getStringFromMap (msg_map, "cmd"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("client.cmd.exec")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (cmd, strlen(cmd));
	else
            (*func) (cmd);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_ENVGETHANDLER -- Handle a client.env.set message
 *
 *  @brief       Handle a client.env.set message
 *  @fn          int samp_envGetHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_envGetHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   name[SZ_NAME], value[SZ_NAME];
    Map    resp, vmap;
    int    maxch = SZ_NAME;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    memset (value, 0, SZ_NAME);
    if ( (func = samp_getUserHandler ("client.env.get")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, value, &maxch, strlen(name), strlen(value));
	else
            (*func) (name, value, maxch);
    }


    /*  Create the response to return the value.
     */
    resp = samp_newMap ();
        samp_setStringInMap (resp, "samp.status", "samp.ok");
        vmap = samp_newMap ();
            samp_setStringInMap (vmap, "value", value);
        samp_setMapInMap (resp, "samp.result", vmap);

    samp_setHandlerReply (resp);

#ifdef FREE_IT
    samp_freeMap (vmap);		/* clean up -- FIXME ??		*/
    samp_freeMap (resp);
#endif
    return (SAMP_OK);
}


/**
 *  SAMP_ENVSETHANDLER -- Handle a client.env.set message
 *
 *  @brief       Handle a client.env.set message
 *  @fn          int samp_envSetHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_envSetHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   name[SZ_NAME], value[SZ_NAME];
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    strcpy (value, samp_getStringFromMap (msg_map, "value"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("client.env.set")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, value, strlen(name), strlen(value));
	else
            (*func) (name, value);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_PARAMGETHANDLER -- Handle a client.param.set message
 *
 *  @brief       Handle a client.param.set message
 *  @fn          int samp_paramGetHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_paramGetHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   name[SZ_NAME], value[SZ_NAME];
    Map    resp, vmap;
    int    maxch = SZ_NAME;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));

    /*  Call the user handler.
     */
    memset (value, 0, SZ_NAME);
    if ( (func = samp_getUserHandler ("client.param.get")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, value, &maxch, strlen(name), strlen(value));
	else
            (*func) (name, value, maxch);
    }

    /*  Create the response to return the value.
     */
    resp = samp_newMap ();
        samp_setStringInMap (resp, "samp.status", "samp.ok");
        vmap = samp_newMap ();
            samp_setStringInMap (vmap, "value", value);
        samp_setMapInMap (resp, "samp.result", vmap);

    samp_setHandlerReply (resp);

#ifdef FREE_IT
    samp_freeMap (vmap);		/* clean up -- FIXME ??		*/
    samp_freeMap (resp);
#endif
    return (SAMP_OK);
}


/**
 *  SAMP_PARAMSETHANDLER -- Handle a client.param.set message
 *
 *  @brief       Handle a client.param.set message
 *  @fn          int samp_paramSetHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_paramSetHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   name[SZ_NAME], value[SZ_NAME];
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    strcpy (value, samp_getStringFromMap (msg_map, "value"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("client.param.set")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, value, strlen(name), strlen(value));
	else
            (*func) (name, value);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_BIBCODEHANDLER -- Handle a bibcode.load message
 *
 *  @brief       Handle a bibcode.load message
 *  @fn          int samp_bibcodeHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_bibcodeHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char   bibcode[SZ_NAME];
    void  (*func) ();


    strcpy (bibcode,  samp_getStringFromMap (msg_map, "bibcode"));

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("bibcode.load")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (bibcode, strlen(bibcode));
	else
            (*func) (bibcode);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_RESLOADHANDLER -- handle a voresource.loadlist.* message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resLoadHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resLoadHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.*")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    return (SAMP_OK);
}


/**
 *  SAMP_RESCONEHANDLER -- Handle a voresource.loadlist.cone message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resConeHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resConeHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.cone")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    /*  Call the generic table handler version.
     */
    samp_resLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_RESSIAPHANDLER -- Handle a voresource.loadlist.siap message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resSiapHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resSiapHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.siap")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    /*  Call the generic table handler version.
     */
    samp_resLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_RESSSAPHANDLER -- Handle a voresource.loadlist.ssap message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resSsapHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resSsapHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.ssap")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    /*  Call the generic table handler version.
     */
    samp_resLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_RESTAPHANDLER -- Handle a voresource.loadlist.tap message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resTapHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resTapHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.tap")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    /*  Call the generic table handler version.
     */
    samp_resLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/**
 *  SAMP_RESVOSPACEHANDLER -- Handle a voresource.loadlist.vospace message
 *
 *  @brief       handle a voresource.loadlist message
 *  @fn          int samp_resVOSpaceHandler (String sender, 
 *			String mtype, String msg_id, Map msg_map)
 *
 *  @param  sender      sender name
 *  @param  mtype       mtype string
 *  @param  msg_id      message id
 *  @param  msg_map     message map struct
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_resVOSpaceHandler (String sender, String mtype, String msg_id, Map msg_map)
{
    char  name[SZ_NAME];
    Map   ids = (Map) 0;
    void  (*func) ();


    strcpy (name,  samp_getStringFromMap (msg_map, "name"));
    ids = samp_getMapFromMap (msg_map, "ids");

    /*  Call the user handler.
     */
    if ( (func = samp_getUserHandler ("voresource.loadlist.vospace")) ) {
	if (sampP->handlerMode == SAMP_CBR)
            (*func) (name, &ids, strlen (name));
	else
            (*func) (name, ids);
    }

    /*  Call the generic table handler version.
     */
    samp_resLoadHandler (sender, mtype, msg_id, msg_map);

    return (SAMP_OK);
}


/*****************************************************************************/


/**
 *  SAMP_PRINTMESSAGE -- Print a message contents in readable form.
 *
 *  @brief       Print a message contents in readable form.
 *  @fn         samp_printMessage (String mtype, String sender,
 *			String mtype, String msg_id, Map params)
 *
 *  @param mtype        mtype string
 *  @param sender       sender name
 *  @param msg_id       message-id string
 *  @param params       Mtype parameter Map
 *  @return             nothing
 */

#define	PMATCH(s)	(strcasecmp(mtype,s)==0)
#define	POPT(s)		(s[0]?s:"INDEF")

void
samp_printMessage (String mtype, String sender, String msg_id, Map params)
{
    extern Samp *sampP;
    char   s1[SZ_NAME], s2[SZ_NAME], s3[SZ_NAME], s4[SZ_NAME];
    Map    meta = (Map) 0, subs = (Map) 0, ids = (Map) 0;
    List   list = (List) 0;
    int    i, ival1;
    double dval1, dval2;


    memset (s1, 0, SZ_NAME); 			/* initialize		*/
    memset (s2, 0, SZ_NAME); 
    memset (s3, 0, SZ_NAME);
    memset (s4, 0, SZ_NAME);

    printf ("%-35.35s  Sender: %-10.10s  Id: %s\n", mtype, sender, 
	(msg_id ? msg_id : ""));


    /*  Process the mtype.
     */
    if (PMATCH ("samp.app.ping")) {				/* no-op */
	;			

    } else if (PMATCH ("samp.app.status")) {
	printf ("\ttxt:  %s\n", samp_getStringFromMap (params, "txt"));

    } else if (PMATCH ("samp.app.event.shutdown")) {
	;							/* no-op */


    } else if (PMATCH ("samp.msg.progress")) {
        strcpy (s1, samp_getStringFromMap (params, "msgid"));
        strcpy (s2, samp_getStringFromMap (params, "txt"));
        strcpy (s3, samp_getStringFromMap (params, "percent"));
        strcpy (s4, samp_getStringFromMap (params, "timeLeft"));
	printf ("\tmsgid:  %s\n\ttxt:  %s\n\tpercent:  %s\n\ttimeLeft:  %s\n",
	    s1, s2, POPT(s3), POPT(s4));


    } else if (PMATCH ("samp.hub.event.shutdown")) {		/* no-op */
	;
    } else if (PMATCH ("samp.hub.event.register")) {
	printf ("\tid:  %s\n", samp_getStringFromMap (params, "id"));

    } else if (PMATCH ("samp.hub.event.unregister")) {
	printf ("\tid:  %s\n", samp_getStringFromMap (params, "id"));

    } else if (PMATCH ("samp.hub.event.metadata")) {
	printf ("\tid:  %s\n", samp_getStringFromMap (params, "id"));
	samp_printMap ("metadata", 
	    (meta = samp_getMapFromMap (params, "metadata")));

    } else if (PMATCH ("samp.hub.event.subscriptions")) {
	printf ("\tid:  %s\n", samp_getStringFromMap (params, "id"));
	samp_printMap ("subscriptions", 
	    (subs = samp_getMapFromMap (params, "subscriptions")));

    } else if (PMATCH ("samp.hub.disconnect")) {
        strcpy (s1, samp_getStringFromMap (params, "reason"));
	printf ("\treason:  %s\n", POPT(s1));



    } else if (PMATCH ("table.load.fits")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));
	printf ("\turl:  %s\n\ttable-id:  %s\n\tname:  %s\n", s1, s2, s3);

    } else if (PMATCH ("table.load.votable")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "table-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));
	printf ("\turl:  %s\n\ttable-id:  %s\n\tname:  %s\n", s1, s2, s3);

    } else if (PMATCH ("table.highlight.row")) {
        strcpy (s1, samp_getStringFromMap (params, "table-id"));
        strcpy (s2, samp_getStringFromMap (params, "url"));
	ival1 = samp_getIntFromMap (params, "row");
	printf ("\turl:  %s\n\ttable-id:  %s\n\trow:  %d\n", s1, s2, ival1);
	
    } else if (PMATCH ("image.load.fits")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "image-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));
	printf ("\turl:  %s\n\timage-id:  %s\n\trow:  %s\n", s1, s2, s3);

    } else if (PMATCH ("coord.pointAt.sky")) {
	dval1 = (double) samp_getFloatFromMap (params, "ra");
	dval2 = (double) samp_getFloatFromMap (params, "dec");
	printf ("\tra:  %g\n\tdec:  %g\n", dval1, dval2);

    } else if (PMATCH ("client.cmd.exec")) {
        strcpy (s1, samp_getStringFromMap (params, "cmd"));
	printf ("\tcmd:  %s\n", s1);

    } else if (PMATCH ("client.env.get")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
	printf ("\tname:  %s\n", s1);

    } else if (PMATCH ("client.env.set")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
        strcpy (s2, samp_getStringFromMap (params, "value"));
	printf ("\tname:  %s\n\tvalue:  %s\n", s1, s2);

    } else if (PMATCH ("client.param.get")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
	printf ("\tname:  %s\n", s1);

    } else if (PMATCH ("client.param.set")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
        strcpy (s2, samp_getStringFromMap (params, "value"));
	printf ("\tname:  %s\n\tvalue:  %s\n", s1, s2);

    } else if (PMATCH ("bibcode.load")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
	printf ("\turl:  %s\n", s1);

    } else if (PMATCH ("table.select.rowList")) {
        strcpy (s1, samp_getStringFromMap (params, "table-id"));
        strcpy (s2, samp_getStringFromMap (params, "url"));
	printf ("\turl:  %s\n\ttable-id:  %s\n\t", s1, s2);

        list = samp_getListFromMap (params, "row-list");
	printf ("rows:  ");
	for (i=0; i < samp_listLen (list); i++)
	    printf ("%d%c", samp_getIntFromList (list, i),
	        (i < samp_listLen (list) - 1) ? ',' : '\n');

    } else if (PMATCH ("spectrum.load.*")) {
        strcpy (s1, samp_getStringFromMap (params, "url"));
        strcpy (s2, samp_getStringFromMap (params, "spectrum-id"));
        strcpy (s3, samp_getStringFromMap (params, "name"));
	printf ("\turl:  %s\n\tspectrum-id:  %s\tname:  %s\n", s1, s2, s3);
	samp_printMap ("meta", (meta = samp_getMapFromMap (params, "meta")));

    } else if (PMATCH ("voresource.loadlist.*")) {
        strcpy (s1, samp_getStringFromMap (params, "name"));
	printf ("\tname:  %s\n\t", s1);
	samp_printMap ("ids", (ids = samp_getMapFromMap (params, "ids")));

    } else {
	/* Print a generic message.
	 */
	samp_printMap ("msg", params);
    }
}

	
/**
 *  SAMP_PRINTMAP -- Print the contents of a Map structure.
 */
void
samp_printMap (String name, Map map)
{
    register int i = 0, nelem = 0;
    char  *k, *v;

    printf ("\t%s:  {\n", name);
    nelem = xr_structSize (map);
    for (i=0; i < nelem; i++) {
	k = xr_getStructKey (map, i);
	v = xr_getStructVal (map, i);
        printf ("\t    %s:  %s\n", k, v);
    }
    printf ("\t}\n");
}




/***************************************************************************
 *  Utility methods.
 **************************************************************************/

/**
 *  SAMP_NULLRESPONSE -- Handler to ignore async replies.
 */
int
samp_nullResponse (void *data)
{
    return (OK);
}


/**
 *  SAMP_TESTECHO -- test.echo method
 *
 *  @brief      test.echo method
 *  @fn         int samp_receiveCall (void *data)
 *
 *  @param  data        caller param data
 *  @return             status code or errno
 */
int
samp_testEcho (void *data)
{
    char *str 	= xr_getStringFromParam (data, 0);

    xr_setStringInResult (data, str);
    return (0);
}
