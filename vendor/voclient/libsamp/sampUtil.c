/**
 *  SAMPUTIL.C  -- Utility methods to convert struct pointers to user handles.
 *
 *  @brief      Utility methods to convert struct pointers to user handles.
 *
 *  @file       sampUtil.c
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


#define	MAX_HANDLES	128


int     numHandles         = 0;
long    sampHandles[MAX_HANDLES];

/*  Public procedures
*/
handle_t  samp_P2H (void *ptr);
void     *samp_H2P (handle_t handle);
handle_t  samp_newHandle (void *ptr);
void      samp_freeHandle (handle_t handle);



/*  Utility routines for keep track of handles.
*/

/**
 *  SAMP_NEWHANDLE -- Get an unused object handle.
 * 
 *  @brief  Get an unused object handle
 *  @fn	    handle_t samp_newHandle (void *ptr)
 *
 *  @param  ptr		pointer to object to be stored
 *  @return		new object handle
 */
handle_t
samp_newHandle (void *ptr)
{
    /* Initialize the handle-to-ptr converter the first time we're called,
    ** or whenever we've restarted.
    */
    if (numHandles == 0)
	memset (sampHandles, 0, sizeof (sampHandles));
    sampHandles[++numHandles] = (long) ptr;

    return (numHandles);
}


/**
 *  SAMP_FREEHANDLE -- Free the handle for later re-use.
 *
 *  @brief  Free the handle for later re-use.
 *  @fn     samp_freeHandle (handle_t handle)
 *
 *  @param  handle	object handle
 *  @return 		nothing
 */
void
samp_freeHandle (handle_t handle)
{
    register int i, j;
    void *ptr = samp_H2P (handle);


    if (handle <= 0) {
	fprintf (stderr, "Error: Attempt to free zero handle!\n");
	return;
    }

    for (i=1; i < MAX_HANDLES; i++) {
	if ((void *) ptr == (void *) sampHandles[i]) {
	    for (j=i+1; j < MAX_HANDLES; j++) {
		if (sampHandles[j])
		    sampHandles[i++] = sampHandles[j];
		else
		    break;
	    }
	    numHandles = ((numHandles-1) >= 0 ? (numHandles-1) : 0);
	    break;
 	}
    }
}


/**
 *  SAMP_P2H -- Convert a pointer to a handle
 *
 *  @brief  Convert a pointer to a handle
 *  @fn     handle_t samp_P2H (void *ptr)
 *
 *  @param  ptr		pointer to object
 *  @return 		handle to object, < 0 on error
 */
handle_t	
samp_P2H (void *ptr)
{
    register int i;

    for (i=1; i < MAX_HANDLES; i++) {
	if ((void *) ptr == (void *) sampHandles[i])
	    return ((int) i);
    }

    return (-1);
}


/**
 *  SAMP_H2P -- Convert a handle to a pointer
 *
 *  @brief  Convert a handle to a pointer
 *  @fn     void *samp_H2P (int handle) 
 *
 *  @param  handle	object handle
 *  @return 		pointer to object or NULL
 */
void *
samp_H2P (handle_t handle) 
{ 
    return ((void *) sampHandles[handle]); 
}


/**
 *  SAMP_APP2ID -- Convert an application name to a public-ID.
 *
 *  @brief  Convert an application name to a public-ID.
 *  @fn     pubId = samp_app2id (handle_t handle, char *appName) 
 *
 *  @param  handle	samp struct handle
 *  @param  appName	name of registered application
 *  @return 		public ID of application
 */
char *
samp_app2id (handle_t handle, char *appName)
{
    Samp *samp = samp_H2P(handle);
    register int i = 0;

    for (i=0; i < samp->nclients; i++)
	if (strcasecmp (appName, samp->clients[i].name) == 0)
	    return (samp->clients[i].pubId);

    return (appName);		/* Not found, return input appName	*/
}


/**
 *  SAMP_ID2APP -- Convert a public-ID to the application name.
 *
 *  @brief  Convert a public-ID to the application name.
 *  @fn     appName = samp_id2app (handle_t handle, char *pubId) 
 *
 *  @param  handle	samp struct handle
 *  @param  pubId	public ID of application
 *  @return 		name of registered application
 */
char *
samp_id2app (handle_t handle, char *pubId)
{
    Samp *samp = samp_H2P(handle);
    register int i = 0;

    for (i=0; i < samp->nclients; i++)
	if (strcasecmp (pubId, samp->clients[i].pubId) == 0)
	    return (samp->clients[i].name);

    return (pubId);		/* Not found, return input pubId	*/
}


/**
 *  SAMP_SERVERPORT -- Return a unique port number for the server.
 *
 *  @brief  Return a unique port number for the server.
 *  @fn     port = samp_serverPort (void)
 *
 *  @return 		port number
 */
int
samp_serverPort ()
{
    static int port = 0;

    if ( port == 0 )
	port = (DEF_PORT + (getpid() % 32768));

    return ( port );
}


/**
 *  SAMP_PRINTMETADATA -- Print the metadata for the application.
 *
 *  @brief 	Print the metadata for the application.
 *  @fn		samp_printMetadata (handle_t handle, String name)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
void
samp_printMetadata (handle_t handle, String name)
{
    Samp *sampP = samp_H2P (handle);
    register int i;

    if (!name || strcmp (name, "samp.name") == 0)
        printf ((!name ? "samp.name:  %s\n" : "%s"), 
	    sampP->appName);
    if (!name || strcmp (name, "samp.description.text") == 0)
        printf ((!name ? "samp.description.text:  %s\n" : "%s"), 
	    sampP->description);
    if (!name || strcmp (name, "samp.icon.url") == 0)
        printf ((!name ? "samp.icon.url:  %s\n" : "%s"), 
	    sampP->meta.iconURL);
    if (!name || strcmp (name, "samp.documentation.url") == 0)
        printf ((!name ? "samp.documentation.url:  %s\n" : "%s"), 
	    sampP->meta.docURL);

    for (i=0; i < sampP->meta.nkeys; i++) {
	if (!name) {
	    printf ("%s:  %s\n", sampP->meta.aKey[i], sampP->meta.aVal[i]);

	} else if (strcmp (name, sampP->meta.aKey[i]) == 0) {
	    if (!name)
	        printf ("%s:  %s\n", sampP->meta.aKey[i], sampP->meta.aVal[i]);
	    else
	        printf ("%s\n", sampP->meta.aVal[i]);
	}
    }
}


/**
 *  SAMP_GETMETADATA -- Get the metadata item for the application.
 *
 *  @brief 	Get the metadata item for the application.
 *  @fn		str = samp_getMetadata (handle_t handle, String param)
 * 
 *  @param  handle	samp handle
 *  @return		nothing
 */
char *
samp_getMetadata (handle_t handle, String name)
{
    Samp *sampP = samp_H2P (handle);
    register int i;


    if (!name)
	return (NULL);

    if (strcmp (name, "samp.name") == 0)
	return (sampP->appName);
    if (strcmp (name, "samp.description.text") == 0)
	return (sampP->description);
    if (strcmp (name, "samp.icon.url") == 0)
	return (sampP->meta.iconURL);
    if (strcmp (name, "samp.documentation.url") == 0)
	return (sampP->meta.docURL);

    for (i=0; i < sampP->meta.nkeys; i++) {
	if (strcmp (name, sampP->meta.aKey[i]) == 0)
	    return (sampP->meta.aVal[i]);
    }

    return (NULL);
}
