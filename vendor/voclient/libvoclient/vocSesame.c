/**
 *  VOCSESAME.C  -- Interface to the Sesame name resolver service.
 *
 *  @section DESCRIPTION
 *
 *  Sesame Name Resolver Interface:
 *  -------------------------------
 *
 *          sr = voc_nameResolver  (target)
 *      pos_str = voc_resolverPos  (sr)
 *         radeg = voc_resolverRA  (sr)
 *       decdeg = voc_resolverDEC  (sr)
 *     ra_err = voc_resolverRAErr  (sr)
 *   dec_err = voc_resolverDECErr  (sr)
 *    typ_str = voc_resolverOtype  (sr)
 *
 *	Client programs may be written in any language that can interface to
 *  C code.  Sample programs using the interface are provided as is a SWIG
 *  interface definition file.  This inferface is based closely on the DAL
 *  client code produced for the 2005 NVOSS, as that interface evolves 
 * 
 * 
 *  @file  	vocSesame.c
 *  @author  	Michael Fitzpatrick
 *  @version	June 2006
 *
 *************************************************************************
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>


#define _VOCLIENT_LIB_
#include "VOClient.h"


#define MAX_OBJECTS		128
#define SZ_TARGET		128


/**
 *  @struct Object
 *
 *  Structure for the object being queried.
 */
typedef struct {
    char    target[SZ_TARGET];		/* target name			*/
    char    hms_pos[SZ_TARGET];		/* sexagesimal position		*/
    double  ra, dec;			/* decimal degrees position	*/
    double  era, edec;			/* decimal degrees error	*/
    char    type[SZ_TARGET];		/* object type			*/
} Object, *ObjectPtr;


/*  @internal
 *
 *  The runtime cache is implemented as a circular array of MAX_OBJECTS.
 *  We first check to see if the requested object is in the runtime cache,
 *  then look on the disk cache for the information.  If not found we
 *  query the server and store the result.  The Sesame handle returned will
 *  be the negative of the index in the runtime cache, i.e "-(1+sr)".
 */
Object clientCache[MAX_OBJECTS];	/* runtime client cache		*/
int    cacheTop = 0;

extern VOClient *vo; 			/* Interface runtime struct	*/


static Sesame   voc_isCachedObject (char *target);
static Sesame   voc_cacheObject (Sesame sr, char *target);

static char    *voc_resStrVal (Sesame sr, char *method, char *ifcall);
static double 	voc_resDblVal (Sesame sr, char *method, char *ifcall);



/*****************************************************************************/

/**
 *  NAMERESOLVER -- Query the CDS Sesame service to resolve the target name
 *  to coordinates.  The query is done when creating the Sesame object, 
 *  thereafter we simply query the object data.
 * 
 *  @brief	Query the CDS Sesame name resolver service.
 *  @fn		handle = voc_nameResolver (char *name)
 *
 *  @param  target  	name of target to be resolved
 *  @returns		Sesame  Sesame object handle
 */
Sesame
voc_nameResolver (char *target)
{
    Sesame   sr = (Sesame) VOC_NULL;


    /*  Make sure we've been initialized properly first.
     */
    if (vo == (VOClient *) NULL) {
        if (voc_initVOClient (NULL) == ERR) {
            fprintf (stderr, "ERROR: Can't initialize VO Client....\n");
            exit (1);
        } else if (VOC_DEBUG)
            printf ("Warning: Initializing VO Client....\n");
    }

    /* Before we query the server, see whether this is a familiar
    ** object and we're using the cache.  Otherwise, return the cached result.
    */
    if (vo->use_cache && (sr=voc_isCachedObject (target)) != (Sesame)VOC_NULL)
	return (sr);

    if (target) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (0, "nameResolver", 0);

        msg_addStringParam (msg, target);
    
        /* Read result and check for any faults.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: cannot resolve target: %s\n", target);
        } else
            sr = msg_getIntResult (result, 0);

        if (msg) free ((void *)msg);         /* free the pointers 	*/
        if (result) free ((void *)result);

	if (voc_resolverRA(sr)     == 0.0 &&
	    voc_resolverRAErr(sr)  == 0.0 &&
	    voc_resolverDEC(sr)    == 0.0 &&
	    voc_resolverDECErr(sr) == 0.0) {
		return (0);		    /* no match found		*/
	}

    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: no target specified\n");


    /* Store the result in the cache.  We can change the return handle if
    ** the object was successfully cached.
    */
    sr = voc_cacheObject (sr, target);

    return (sr);
}


/**
 *  RESOLVERPOS --  Return a string containing the (ra,dec) position as
 *  sexagesimal strings. 
 *
 *  @brief	Return the (ra,dec) position for the object
 *  @fn		str = voc_resolverPos (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		string containing (ra,dec) position
 */
char *
voc_resolverPos (Sesame sr)
{
    if (sr < 0)
        return ( strdup(clientCache[-(1+sr)].hms_pos) );
    else
        return ( voc_resStrVal (sr, "srGetPOS", "resolverPos") );
}


/**
 *  RESOLVEROTYPE --  Return a string containing the object type description
 *
 *  @brief	Return a string containing the object type description
 *  @fn		str = voc_resolverOtype (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		string to object type description
 */
char *
voc_resolverOtype (Sesame sr)
{
    if (sr < 0)
        return ( strdup (clientCache[-(1+sr)].type) );
    else
        return ( voc_resStrVal (sr, "srGetOtype", "resolverOtype") );
}


/**
 *  RESOLVERRA --  Return the RA as a double precision value.
 *
 *  @brief	Return the RA as a double precision value.
 *  @fn		str = voc_resolverRA (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		object RA (decimal degrees)
 */
double      
voc_resolverRA (Sesame sr)
{
    if (sr < 0)
        return ( clientCache[-(1+sr)].ra );
    else
        return ( voc_resDblVal (sr, "srGetRA", "resolverRA") );
}


/**
 *  RESOLVERRAERR --  Return the RA error as a double precision value.
 *
 *  @brief	Return the RA error as a double precision value.
 *  @fn		str = voc_resolverRAErr (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		object RA error (decimal degrees)
 */
double      
voc_resolverRAErr (Sesame sr)
{
    if (sr < 0)
        return ( clientCache[-(1+sr)].era );
    else
        return ( voc_resDblVal (sr, "srGetRAErr", "resolverRAErr") );
}


/**
 *  RESOLVERDEC --  Return the DEC as a double precision value.
 *
 *  @brief	Return the DEC as a double precision value.
 *  @fn		str = voc_resolverDEC (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		object Declination (decimal degrees)
 */
double      
voc_resolverDEC (Sesame sr)
{
    if (sr < 0)
        return ( clientCache[-(1+sr)].dec );
    else
        return ( voc_resDblVal (sr, "srGetDEC", "resolverDEC") );
}


/**
 *  RESOLVERDECERR --  Return the Dec error as a double precision value.
 *
 *  @brief	Return the Dec error as a double precision value.
 *  @fn		str = voc_resolverDECErr (Sesame sr)
 *
 *  @param  sr  	handle to previus query return
 *  @returns		object DEC error (decimal degrees)
 */
double      
voc_resolverDECErr (Sesame sr)
{
    if (sr < 0)
        return ( clientCache[-(1+sr)].edec );
    else
        return ( voc_resDblVal (sr, "srGetDECErr", "resolverDECErr") );
}



/************************************************************
 *	Private procedures
 ***********************************************************/

extern char *voc_getCacheDir (char *subdir);
    

/**
 *  VOC_ISCACHEDOBJECT -- See if the requested object is in the cache.
 *
 *  @brief	See if the requested object is in the cache.
 *  @fn		sr = voc_isCachedObject (char *target)
 *
 *  @param  target	target name
 *  @returns		handle to cached object
 */
static Sesame
voc_isCachedObject (char *target)
{
    FILE  *fd;
    register int index;
    char  *ip, *op, *dir, fname[SZ_FNAME], path[SZ_FNAME], buf[256];
    Object *obj = (Object *) NULL;
    struct stat info;


    /* Look first for the object in the runtime cache.
    */
    for (index=0; index < cacheTop; index++) {
	if (strcmp (target, clientCache[index].target) == 0) {
	    return (-(index+1));
	}
    }

    /* Not in the runtime cache, check to see if we have it on disk.
    */

    /* Turn the target name into a filename, replacing the white space
    ** with an underscore.
    */
    for (ip=target, op=fname; *ip; ip++) 
	*op++ = ((isspace(*ip) || *ip == '_') ? '+' : *ip);
    *op = '\0';

    sprintf (path, "%s/%s", (dir = voc_getCacheDir("sesame")), fname);
    if (! (fd = fopen (path, "r")) )
	return ((Sesame) VOC_NULL);		/* not in cache		*/
    if (fstat (fileno (fd), &info) < 0)
	return ((Sesame) VOC_NULL);		/* invalid cache	*/
    if (info.st_size == 0)
	return ((Sesame) VOC_NULL);		/* invalid cache	*/

    fgets (buf, 256, fd);

    /* Save it in the runtime cache.
    */
    index = ((cacheTop++) % MAX_OBJECTS);	/* runtime cache 	*/
    obj = &clientCache[index];

    memset (obj, 0, sizeof(Object));
    for (op=&obj->target[0], ip=buf; *ip && *ip != ':'; )
	*op++ = *ip++;
    ip++;					/* skip colon		*/
    ip++;					/* skip space		*/
    op = &obj->hms_pos[0];
    while (*ip && !isspace(*ip)) *op++ = *ip++; /* get ra		*/
    while (*ip && isspace(*ip))  *op++ = *ip++; /* skip spaces		*/
    while (*ip && !isspace(*ip)) *op++ = *ip++; /* get dec		*/

    sscanf (ip, " %lf %lf %lf %lf %s\n", 
	&obj->ra, &obj->dec, &obj->era, &obj->edec, obj->type);

    fclose (fd);

    return ((Sesame) -(index+1));
}


/**
 *  VOC_CACHEOBJECT -- Store the object in the cache.
 *
 *  @brief	Store the object in the cache.
 *  @fn		sr = voc_cacheObject (Sesame sr, char *target)
 *
 *  @param  sr		handle to sesame query
 *  @param  target	target name
 *  @returns		handle to cached object
 */
static Sesame
voc_cacheObject (Sesame sr, char *target)
{
    FILE  *fd;
    register int index;
    char  *ip, *op, *dir, *s, fname[SZ_FNAME], path[SZ_FNAME];
    Object *obj = (Object *) NULL;


    if ((s = getenv("VOC_NO_CACHE")))
	return (sr);

    /* Turn the target name into a filename, replacing the white space
    ** with an underscore.
    */
    for (ip=target, op=fname; *ip; ip++) 
	*op++ = (isspace(*ip) ? '_' : *ip);
    *op = '\0';

    /* Open the cache file and pre-fetch the results.
    */
    sprintf (path, "%s/%s", (dir = voc_getCacheDir("sesame")), fname);
    if (! (fd = fopen (path, "a+")) )
	return (sr);				/* error return	 	*/

    index = ((cacheTop++) % MAX_OBJECTS);	/* runtime cache 	*/
    obj = &clientCache[index];

    fprintf (fd, "%s: %s %f %f %.2f %.2f %s\n", 
	strcpy(obj->target, target),
	strcpy(obj->hms_pos, voc_resolverPos (sr)),
	(obj->ra   = voc_resolverRA (sr)),
	(obj->dec  = voc_resolverDEC (sr)),
	(obj->era  = voc_resolverRAErr (sr)),
	(obj->edec = voc_resolverRAErr (sr)),
	strcpy(obj->type, voc_resolverOtype (sr)) );

    fclose (fd);

    /* Don't cache a NULL return.
    */
    if (((int)obj->ra + (int)obj->dec + (int)obj->era + (int)obj->edec) == 0) {
	    unlink (path);
            return (sr);
    }

    if (dir)
	free ((char *)dir);

    return ((Sesame) -(index + 1));		/* return new sr	*/
}


/**
 *  VOC_RESSTRVAL -- Return a string value from a method with no arguments.
 *
 *  @brief	Return a string value from a method with no args.
 *  @fn		str = voc_resStrVal (Sesame sr, char *method, char *ifcall)
 *
 *  @param  sr		handle to sesame query
 *  @param  method	mehod to call
 *  @param  ifcall	interface method name
 *  @returns		the value
 */
static char *
voc_resStrVal (Sesame sr, char *method, char *ifcall)
{
    char     *val = NULL, *str = NULL;
    int       len = 0;

    if (sr > 0) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (sr, method, 0);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: %s() failed\n", ifcall);
        } else {
            val = msg_getStringResult (result, 0);
            str = calloc (1, (len = strlen(val)+1));
            strncpy (str, val, len);
        }

        if (msg) free ((void *)msg);     /* free the pointers            */
        if (result) free ((void *)result);

    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null object to %s()\n", ifcall);

    return (str);
}


/**
 *  VOC_RESDBLVAL -- Return a double precision value from a method with 
 *  no arguments.
 *
 *  @brief	Return a double precision value from a method with no args.
 *  @fn		dval = voc_resDblVal (Sesame sr, char *method, char *ifcall)
 *
 *  @param  sr		handle to sesame query
 *  @param  method	mehod to call
 *  @param  ifcall	interface method name
 *  @returns		the value
 */
static double      
voc_resDblVal (Sesame sr, char *method, char *ifcall)
{
    double   dval = (double) 0.0;


    if (sr > 0) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (sr, method, 0);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: %s() failed\n", ifcall);
        } else
            dval = msg_getFloatResult (result, 0);

        if (msg) free ((void *)msg);     /* free the pointers            */
        if (result) free ((void *)result);

    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null object to %s()\n", ifcall);

    return (dval);
}
