/**
 *  SAMPMTYPES.C -- SAMP interface procedures for application mtypes.
 *
 *        samp_tableLoadVOTable  (handle, recip, url, tableId, name)
 *           samp_tableLoadFITS  (handle, recip, url, tableId, name)
 *       samp_tableHighlightRow  (handle, recip, tableId, url, row)
 *      samp_tableSelectRowList  (handle, recip, tableId, url, rows[], nrows)
 *
 *           samp_imageLoadFITS  (handle, recip, url, imageId, name)
 *         samp_coordPointAtSky  (handle, recip, ra, dec)
 *
 *      samp_specLoadSSAGeneric  (handle, recip, url, meta_map, specId, name)
 *                 samp_cmdExec  (handle, recip, cmd)
 *                  samp_envGet  (handle, recip, name)
 *                  samp_envSet  (handle, recip, name, value)
 *                samp_paramGet  (handle, recip, name)
 *                samp_paramSet  (handle, recip, name, value)
 *                 samp_bibLoad  (handle, recip, bibcode)
 *            samp_resourceLoad  (handle, recip, type, resList[])
 *
 *             samp_sendGeneric  (handle, recip, mtype, args)
 *                 samp_sendMsg  (handle, recip, Map msg_map)
 *
 *
 *  @brief      SAMP  interface procedures for application mtypes.
 *
 *  @file       sampMTypes.c
 *  @author     Mike Fitzpatrick
 *  @date       7/18/11
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <ctype.h>

#include "samp.h"




/******************************************************************************
 *  Application-specific MType callers.
 *
 *  Supported Mtypes:
 *
 *	table.load.votable 		url [table-id] [name]
 *	table.load.fits 		url [table-id] [name]
 *	table.highlight.row		table-id url row
 *	table.select.rowList		table-id url row-list
 *	image.load.fits			url [image-id] [name]
 *	coord.pointAt.sky		ra dec
 *	spectrum.load.ssa-generic  	url meta [spectrum-id] [name]
 *
 *	client.cmd.exec			cmd
 *	client.env.get			name
 *	client.env.set			name value
 *	client.param.get		name
 *	client.param.set		name value
 *	bibcode.load			bibcode
 *	voresource.loadlist		[name] ids  (map of ivoid -> URL)
 *	  voresource.loadlist.cone	  "     "
 *	  voresource.loadlist.siap	  "     "
 *	  voresource.loadlist.ssap	  "     "
 *	  voresource.loadlist.tap	  "     "
 *	  voresource.loadlist.vospace	  "     "
 *
 *
 *  Interface Methods:
 *
 *   stat = samp_tableLoadVOTable (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *   stat = samp_tableLoadFITS (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *   stat = samp_tableHighlightRow (handle_t handle, String recip, 
 *			String tableId, String url, int row)
 *
 *   stat = samp_tableSelectRowList (handle_t handle, String recip, 
 *			String tableId, String url, List rows, int nrows)
 *
 *   stat = samp_imageLoadFITS (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *   stat = samp_coordPointAtSky (handle_t handle, String recip, 
 *			float ra, float dec)
 *
 *   stat = samp_specLoadSSAGeneric (handle_t handle, String recip,
 *			String url, Map meta, String spectrumId, String name)
 *
 *   val = samp_cmdExec (handle_t handle, String recip, String cmd)
 *
 *   val = samp_envGet (handle_t handle, String recip, String name)
 *
 *   stat = samp_envSet (handle_t handle, String recip, String name,
 *                              String value)
 *
 *   val = samp_paramGet (handle_t handle, String recip, String name)
 *
 *   stat = samp_paramSet (handle_t handle, String recip, String name,
 *                              String value)
 *
 *   stat = samp_bibLoad (handle_t handle, String recip, String bibcode)
 *
 *   stat = samp_resourceLoad (handle_t handle, String recip, String type, 
 *				String name, Map resMap)
 * 
 *****************************************************************************/


/******************************************
 *  Table MTypes
 ******************************************/

/**
 *  SAMP_TABLELOADVOTABLE -- Tell an app to load a VOTable.
 *
 *  @brief      Tell an app to load a VOTable.
 *  @fn         stat = samp_tableLoadVOTable (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Message recipient (or "all" for broadcast)
 *  @param  url         URL to table to be loaded
 *  @param  tableId     ID assigned to table (optional)
 *  @param  name        Name assigned to table (optional)
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tableLoadVOTable (handle_t handle, String recip, String url, 
		String tableId, String name)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int	 status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "table.load.votable");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addStringParam (msg, "table-id", tableId);
	samp_addStringParam (msg, "name", name);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);					   /* clean up        */

    return (status);
}


/**
 *  SAMP_TABLELOADFITS -- Tell an app to load a FITS table.
 *
 *  @brief      Tell an app to load a FITS table.
 *  @fn         stat = samp_tableLoadFITS (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Message recipient (or "all" for broadcast)
 *  @param  url         URL to table to be loaded
 *  @param  tableId     ID assigned to table (optional)
 *  @param  name        Name assigned to table (optional)
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tableLoadFITS (handle_t handle, String recip, String url, 
		String tableId, String name)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int	 status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "table.load.fits");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addStringParam (msg, "table-id", tableId);
	samp_addStringParam (msg, "name", name);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);					   /* clean up        */

    return (status);
}


/**
 *  SAMP_TABLEHIGHLIGHTROW -- Tell an app to highlight a table row.
 *
 *  @brief      Tell an app to highlight a table row.
 *  @fn         stat = samp_tableHighlightRow (handle_t handle, String recip, 
 *			String tableId, String url, int row)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Message recipient (or "all" for broadcast)
 *  @param  tableId     ID associated with a previously loaded table
 *  @param  url         URL to table to be loaded
 *  @param  rows        List of (zero-based) row indices
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tableHighlightRow (handle_t handle, String recip, String tableId, 
		String url, int row)
{
    Samp  *sampP = samp_H2P (handle);
    Hub     *hub = sampP->hub;
    int   status = SAMP_OK;

    Msg    msg   = samp_newMsg ();
    Param  param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "table.highlight.row");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addStringParam (msg, "table-id", tableId);
	samp_addIntParam (msg, "row", row);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/**
 *  SAMP_TABLESELECTROWLIST -- Tell an app to select a list of table rows.
 *
 *  @brief      Tell an app to select a list of table rows.
 *  @fn         stat = samp_tableSelectRowList (handle_t handle, String recip, 
 *			String tableId, String url, int rows[], int nrows)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Message recipient (or "all" for broadcast)
 *  @param  tableId     ID associated with a previously loaded table
 *  @param  url         URL to table to be loaded
 *  @param  rows        Array of (zero-based) row indices
 *  @param  nrows       Number of rows
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_tableSelectRowList (handle_t handle, String recip, String tableId, 
		String url, int rows[], int nrows)
{
    register  int  i, status;
    Samp *sampP = samp_H2P (handle);
    Hub  *hub = sampP->hub;
    Msg   msg    = samp_newMsg ();
    Param param  = samp_newParam ();
    List  rowlist;


    if (!hub)
	return (SAMP_ERR);

    rowlist = samp_newList ();
    for (i=0; i < nrows; i++)
	samp_setIntInList (rowlist, rows[i]);

    /*  Create message map.
     */
    samp_msgMType (msg, "table.select.rowList");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addStringParam (msg, "table-id", tableId);
	samp_addListParam (msg, "row-list", rowlist);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);
    samp_freeList (rowlist);

    return (SAMP_OK);
}



/******************************************
 *  Image MTypes
 ******************************************/

/**
 *  SAMP_IMAGELOADFITS -- Tell an app to load a FITS image.
 *
 *  @brief      Tell an app to load a FITS image.
 *  @fn         stat = samp_imageLoadFITS (handle_t handle, String recip, 
 *			String url, String tableId, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Message recipient (or "all" for broadcast)
 *  @param  url         URL to table to be loaded
 *  @param  imageId     ID assigned to image (optional)
 *  @param  name        Name assigned to image (optional)
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_imageLoadFITS (handle_t handle, String recip, String url, 
		String imageId, String name)
{
    Samp  *sampP = samp_H2P (handle);
    Hub     *hub = sampP->hub;
    int	  status = SAMP_OK;

    Msg   msg    = samp_newMsg ();
    Param param  = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "image.load.fits");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addStringParam (msg, "image-id", imageId);
	samp_addStringParam (msg, "name", name);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);					   /* clean up        */

    return (status);
}


/******************************************
 *  General MTypes
 ******************************************/

/**
 *  SAMP_COORDPOINTATSKY -- Tell an app to point at an RA/Dec coordinate.
 *
 *  @brief      Tell an app to point at an RA/Dec coordinate.
 *  @fn         stat = samp_coordPointAtSky (handle_t handle, String recip, 
 *				float ra, float dec)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  ra          RA of coord in degrees
 *  @param  dec         Dec of coord in degrees
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_coordPointAtSky (handle_t handle, String recip, float ra, float dec)
{
    Samp  *sampP = samp_H2P (handle);
    Hub     *hub = sampP->hub;
    Msg    msg   = samp_newMsg ();
    Param  param = samp_newParam ();
    int   status = SAMP_OK;


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "coord.pointAt.sky");
    samp_msgParam (msg, param);
	samp_addFloatParam (msg, "ra", ra);
	samp_addFloatParam (msg, "dec", dec);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/******************************************
 *  Spectral MTypes
 ******************************************/

/**
 *  SAMP_SPECLOADSSAGENERIC -- Load a generic spectrum from an SSA service.
 *
 *  @brief      Load a generic spectrum from an SSA service.
 *  @fn         stat = samp_specLoadSSAGeneric (handle_t handle, String recip,
 *			String url, Map meta, String spectrumId, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  url         URL of spectrum to load
 *  @param  meta        Metadata map describing data found at the URL
 *  @param  spectrumId  Identifier for spectrum (optional)
 *  @param  name        Name used to label spectrum (optional);
 *  @return             SAMP_OK or SAMP_ERR
 */
int
samp_specLoadSSAGeneric (handle_t handle, String recip, String url, Map meta, 
		String spectrumId, String name)
{
    Samp  *sampP = samp_H2P (handle);
    Hub     *hub = sampP->hub;
    int	  status = SAMP_OK;

    Msg   msg    = samp_newMsg ();
    Param param  = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "spectrum.load.ssa-generic");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "url", url);
	samp_addMapParam (msg, "meta", meta);
	samp_addStringParam (msg, "spectrum-id", spectrumId);
	samp_addStringParam (msg, "name", name);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);					   /* clean up        */

    return (status);
}


/******************************************
 *  Environment MTypes
 ******************************************/

/**
 *  SAMP_CMDEXEC -- Execute a command in a remote application.
 *
 *  @brief      Execute a command in a remote application.
 *  @fn         stat = samp_cmdExec (handle_t handle, String recip, String cmd)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  cmd         Command string
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_cmdExec (handle_t handle, String recip, String cmd)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "client.cmd.exec");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "cmd", cmd);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/**
 *  SAMP_ENVGET -- Get an environment variable in a remote application.
 *
 *  @brief      Get an environment variable in a remote application.
 *  @fn         val = samp_envGet (handle_t handle, String recip, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  name        Name of variable in receiver's environment
 *  @return        	NULL, Value of variable, or list of values
 */
char *
samp_envGet (handle_t handle, String recip, String name)
{
    register int i, nfound = 0;
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    Map    resp = (Map) 0;
    Map    sres = (Map) 0;
    char val[SZ_LINE], *value = val;

    Msg     msg = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (NULL);

    memset (value, 0, SZ_LINE);

    /*  Create message map.
     */
    samp_msgMType (msg, "client.env.get");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "name", name);


    /*  Send the message using the synch pattern, don't provide a choice.
     */
    memset (value, 0, SZ_LINE);
    if (strcmp ("all", recip) == 0) {
        char result[SZ_LINE], *res = result;

	for (i=0; i < sampP->nclients; i++) {
	    if (strcasecmp (sampP->appName, sampP->clients[i].name) == 0)
		continue;
	    if (strcasecmp ("hub", sampP->clients[i].name) == 0)
		continue;

            xr_initParam (hub->id);        /* set calling parameters       */
            xr_setStringInParam (hub->id, hub->privateKey);
            xr_setStringInParam (hub->id, sampP->clients[i].pubId);
            xr_setStructInParam (hub->id, msg);
            xr_setStringInParam (hub->id, hub->timeout);

            if (xr_callSync (hub->id, "samp.hub.callAndWait") == OK ) {
                /*  Save the response to a new map so we can free the msg result.
                 */
                xr_getStructFromResult (hub->id, &resp);
                xr_getStructFromStruct (resp, "samp.result", &sres);
                xr_getStringFromStruct (sres, "value", &res);
		nfound++;

            } else {
		strcpy (value, "");
            }
	    if (i && nfound > 1)
	        strcat (value, ",");
	    strcat (value, sampP->clients[i].name);
	    if (strchr (res, (int) ' ') || strchr (res, (int) ',')) {
	        strcat (value, ":\"");
	        strcat (value, res);
	        strcat (value, "\"");
	    } else {
	        strcat (value, ":");
	        strcat (value, res);
	    }
	}

    } else {
        xr_initParam (hub->id);             /* set calling parameters       */
        xr_setStringInParam (hub->id, hub->privateKey);
        xr_setStringInParam (hub->id, samp_app2id(handle, recip));
        xr_setStructInParam (hub->id, msg);
        xr_setStringInParam (hub->id, hub->timeout);

        if (xr_callSync (hub->id, "samp.hub.callAndWait") == OK ) {
            /*  Save the response to a new map so we can free the msg result.
             */
            xr_getStructFromResult (hub->id, &resp);
            xr_getStructFromStruct (resp, "samp.result", &sres);
            xr_getStringFromStruct (sres, "value", &value);

        } else {
            strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
            return (NULL);
        }
    }

    samp_freeMsg (msg);

    return ( value );
}


/**
 *  SAMP_ENVSET -- Set an environment variable in a remote application.
 *
 *  @brief      Set an environment variable in a remote application.
 *  @fn         stat = samp_envSet (handle_t handle, String recip, String name,
 *				String value)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  name        Name of variable in receiver's environment
 *  @param  value       Value of variable or NULL            
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_envSet (handle_t handle, String recip, String name, String value)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "client.env.set");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "name", name);
	samp_addStringParam (msg, "value", value);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/**
 *  SAMP_PARAMGET -- Get a parameter variable in a remote application.
 *
 *  @brief      Get a parameter variable in a remote application.
 *  @fn         val = samp_paramGet (handle_t handle, String recip, String name)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  name        Name of variable in receiver's environment
 *  @return        	NULL, Value of variable, or list of values
 */
char *
samp_paramGet (handle_t handle, String recip, String name)
{
    register int  i, nfound = 0;
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    Map    resp = (Map) 0;
    Map    sres = (Map) 0;
    static char val[SZ_LINE], *value = val;

    Msg     msg = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (NULL);

    /*  Create message map.
     */
    samp_msgMType (msg, "client.param.get");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "name", name);


    /*  Send the message using the synch pattern, don't provide a choice.
     */
    memset (value, 0, SZ_LINE);
    if (strcmp ("all", recip) == 0) {
	char result[SZ_LINE], *res = result;

	for (i=0; i < sampP->nclients; i++) {
	    if (strcasecmp (sampP->appName, sampP->clients[i].name) == 0)
		continue;
	    if (strcasecmp ("hub", sampP->clients[i].name) == 0)
		continue;
	    if (!sampP->clients[i].name[0])
		continue;

            xr_initParam (hub->id);         /* set calling parameters       */
            xr_setStringInParam (hub->id, hub->privateKey);
            xr_setStringInParam (hub->id, sampP->clients[i].pubId);
            xr_setStructInParam (hub->id, msg);
            xr_setStringInParam (hub->id, hub->timeout);

            if (xr_callSync (hub->id, "samp.hub.callAndWait") == OK ) {
                /*  Save the response to a new map so we can free the result.
                 */
		memset (res, 0, SZ_LINE);
                xr_getStructFromResult (hub->id, &resp);
        	xr_getStructFromStruct (resp, "samp.result", &sres);
        	xr_getStringFromStruct (sres, "value", &res);
		nfound++;

            } else {
		strcpy (res, "Error");
            }
	    if (i && nfound > 1)
	        strcat (value, ",");

	    strcat (value, sampP->clients[i].name);
	    if (strchr (res, (int) ' ') || strchr (res, (int) ',')) {
	        strcat (value, ":\"");
	        strcat (value, res);
	        strcat (value, "\"");
	    } else {
	        strcat (value, ":");
	        strcat (value, res);
	    }
	}

    } else {
        xr_initParam (hub->id);             /* set calling parameters       */
        xr_setStringInParam (hub->id, hub->privateKey);
        xr_setStringInParam (hub->id, samp_app2id(handle, recip));
        xr_setStructInParam (hub->id, msg);
        xr_setStringInParam (hub->id, hub->timeout);

        if (xr_callSync (hub->id, "samp.hub.callAndWait") == OK ) {
            /*  Save the response to a new map so we can free the msg result.
             */
            xr_getStructFromResult (hub->id, &resp);
            xr_getStructFromStruct (resp, "samp.result", &sres);
            xr_getStringFromStruct (sres, "value", &value);

        } else {
            strcpy (sampP->errortxt, xr_getErrMsg (hub->id));
            return (NULL);
        }
    }

    samp_freeMsg (msg);
    return ( value );
}


/**
 *  SAMP_PARAMSET -- Set an parameter variable in a remote application.
 *
 *  @brief      Set an parameter variable in a remote application.
 *  @fn         stat = samp_paramSet (handle_t handle, String recip,
 *				String name, String value)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  name        Name of variable in receiver's environment
 *  @param  value       Value of variable or NULL            
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_paramSet (handle_t handle, String recip, String name, String value)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "client.param.set");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "name", name);
	samp_addStringParam (msg, "value", value);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/******************************************
 *  Bibcode MTypes
 ******************************************/

/**
 *  SAMP_BIBLOAD -- Exchange a bibcode.
 *
 *  @brief      Exchange a bibcode.
 *  @fn         val = samp_bibLoad (handle_t handle, String recip, String bibcode)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       name of recipient (or 'all')
 *  @param  bibcode     Bibcode string
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_bibLoad (handle_t handle, String recip, String bibcode)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, "bibcode.load");
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "bibcode", bibcode);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/******************************************
 *  Resource MTypes
 ******************************************/

/**
 *  SAMP_RESOURCELOAD -- Exchange a resource list.
 *
 *  @brief      Exchange a resource list.
 *  @fn         val = samp_resourceLoad (handle_t handle, String recip, 
 *				String type, String name, Map resMap)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Name of recipient (or 'all')
 *  @param  type        Resource type (or NULL)
 *  @param  name        Descriptive name (optional)
 *  @param  resMap      Resource map
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_resourceLoad (handle_t handle, String recip, String type, 
	String name, Map resMap)
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK;
    char resType[SZ_NAME], *ip;

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    if (type) {
        memset (resType, 0, SZ_NAME);
        for (ip=type; *ip; ip++)
	    *ip = tolower (*ip);
        sprintf (resType, "voresource.loadlist.%s", type);
    } else
        strcpy (resType, "voresource.loadlist");

    /*  Create message map.
     */
    samp_msgMType (msg, resType);
    samp_msgParam (msg, param);
	samp_addStringParam (msg, "name", name);
	samp_addMapParam (msg, "ids", resMap);

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}


/**
 *  SAMP_SENDGENERIC -- Send a generic message.
 *
 *  @brief      Send a geenric message.
 *  @fn         stat = samp_sendGeneric (handle_t handle, String recip, 
 *			    String mtype, String args[])
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Name of recipient (or 'all')
 *  @param  mtype       Message type
 *  @param  args        Argument list
 *  @return        	SAMP_OK or SAMP_ERR
 *
 *  This method can be used to send any user-defined message.  The 'mtype'
 *  string is arbitrary, we just assume the receiving client can understand
 *  what it means.  The 'args' String array can be an array of values in
 *  which case the parameter name will be a series of generic 'arg0' thru
 *  'argN-1' names.
 *
 *
 */
int
samp_sendGeneric (handle_t handle, String recip, String mtype, String args[])
{
    Samp *sampP = samp_H2P (handle);
    Hub    *hub = sampP->hub;
    int  status = SAMP_OK, i = 0;
    char   *arg = NULL, *par, *val, *eq, buf[SZ_NAME];

    Msg   msg   = samp_newMsg ();
    Param param = samp_newParam ();


    if (!hub)
	return (SAMP_ERR);

    /*  Create message map.
     */
    samp_msgMType (msg, mtype);
    samp_msgParam (msg, param);

    for (i=0; args[i]; i++) {
        arg = args[i];
	if (!arg || !arg[0])
	    break;

	if ((eq =strchr (arg, (int) '='))) {		/* param=value 	*/
	    *eq = '\0';
	    par = arg;
	    val = (isspace(*(eq+1)) ? NULL : (eq + 1));
	} else {
	    memset (buf, 0, SZ_NAME);			/* generic name	*/
	    sprintf (buf, "arg%d", i);
	    par = buf;
	    val = arg;
	}

	if (strncmp (par, "i:", 2) == 0) {		/* int param	*/
	    par += 2;
	    samp_addIntParam (msg, par, (val ? atoi(val) : 0));
	} else if (strncmp (par, "f:", 2) == 0) {	/* float param	*/
	    par += 2;
	    samp_addFloatParam (msg, par, (val ? atof(val) : 0.0));
	} else {					/* string param	*/
	    samp_addStringParam (msg, par, val);
	}
    }

    status = samp_sendMsg (handle, recip, msg);
    samp_freeMsg (msg);

    return (status);
}

             
/**
 *  SAMP_SENDMSG -- Send the specified message.
 *
 *  @brief      Send the specified message.
 *  @fn         stat = samp_sendMsg (handle_t handle, String recip, Map msg)
 *
 *  @param  handle      samp struct handle
 *  @param  recip       Name of recipient (or 'all')
 *  @param  msg         Message map
 *  @return        	SAMP_OK or SAMP_ERR
 */
int
samp_sendMsg (handle_t handle, String recip, Map msg)
{
    Samp   *sampP = samp_H2P (handle);
    String  tag = samp_msgTag();
    Map     resp = (Map) 0;
    int     i, status = SAMP_OK;
    char   *pubId = samp_app2id (handle, recip);
    char   *msg_id = NULL;


    /*  Execute the message call to the Hub.  This is where we implement the
     *  messaging pattern, i.e. broadcast vs directed message, synch vs
     *  asynch vs notify, etc.
     */
    if (strncasecmp (recip, "all", 3) == 0) {
	switch (sampP->msgMode) {
	case SAMP_SYNCH:			       	       /* call        */
	    for (i=0; i < sampP->nclients; i++) {
		pubId = sampP->clients[i].pubId;
                resp = samp_callAndWait (handle, pubId, tag, msg);
    		status = samp_setErr (handle, resp);
		if (status != SAMP_OK) {
		    ;  /* FIXME -- Do something about this. */
		}
	    }
	    break;
	case SAMP_ASYNCH:
            resp = samp_callAll (handle, tag, msg); 	       /* callAll     */
	    break;
	case SAMP_NOTIFY:
            resp = samp_notifyAll (handle, msg); 	       /* notifyAll   */
	    break;
	default:
	    ;
	}
    } else {
	switch (sampP->msgMode) {
	case SAMP_SYNCH:
            resp = samp_callAndWait (handle, pubId, tag, msg); /* callAndWait */
    	    status = samp_setErr (handle, resp);
	    break;
	case SAMP_ASYNCH:
            msg_id = samp_call (handle, pubId, tag, msg);      /* call        */
	    status = (msg_id ? SAMP_OK : SAMP_ERR);
	    break;
	case SAMP_NOTIFY:
            (void) samp_notify (handle, pubId, msg);           /* notify      */
	    break;
	default:
	    ;
	}
    }

    return (status);
}


/******************************************
 *  Utility Procedures
 ******************************************/

