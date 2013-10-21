/**
 *  SkyBoT Minor Planet Service Interface:
 *  --------------------------------------
 *
 *  A utility class for accessing the IMCCE Skybot ephemerides service.  
 *  The service is called as part of the constructor for the object 
 *  allowing easy access to the result data.  Class methods also exist so
 *  a VO Client interface the same access to the data.
 *
 *  Reference:      http://www.imcce.fr/webservices/skybot/
 *
 *  Class Methods:      
 *  --------------
 *
 *              sb = voc_skybot  (ra, dec, rsz, dsz, epoch)
 *
 *      count = voc_skybotNObjs  (sb)
 *      str = voc_skybotStrAttr  (sb, attrname, index)
 *     dval = voc_skybotDblAttr  (sb, attrname, index)
 *
 *  Available Attributes:
 *  ---------------------
 *
 *      number  string      Asteroid number
 *      name    string      Asteroid name
 *      ra      double      J2000 Equatorial RA
 *      dec     double      J2000 Equatorial Dec
 *      class   string      Object classification
 *      vmag    double      Visual magnitude
 *      poserr  double      Error on position (arcsec)
 *      cdist   double      Body-to-center angular distance
 *      dra     double      RA motion (arcsec/hr)
 *      ddec    double      Dec motion (arcsec/hr)
 *      dgeo    double      Geocentric distance (AU)
 *      dhelio  double      Heliocentric distance (AU)
 *      px      double      Mean J2000 heliocentric position vector (AU)
 *      py      double          "       "       "       "       "
 *      pz      double          "       "       "       "       "
 *      vx      double      Mean J2000 heliocentric position vector (AU/day)
 *      vy      double          "       "       "       "       "
 *      vz      double          "       "       "       "       "       
 *      JD0     double      T0, epoch of position vector (JD)
 * 
 *
 *  Example Usage:      
 *  ---------------
 *
 *  1) Find bodies in a 900" radius around (0.0,0.0) on JD 2453939.123
 *
 *      VOCSkybot skybot = new VOCSkybot (0.0,0.0,900,2453939.123);
 *      for (int i=0; i < skybot.getNObjs(); i++)
 *          System.out.print   ("Name: '" + sb.getStrAttr("name",i)+"' " +
 *                              "RA: " + sb.getDblAttr("ra",i) + " " +
 *                              "Dec: " + sb.getDblAttr("dec",i) + " " +
 *                              "Mv: " + sb.getDblAttr("vmag",i) + " "); 
 * 
 * 
 *  @file       vocSkybot.c
 *  @author     Michael Fitzpatrick
 *  @version    June 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


extern VOClient *vo; 			/* Interface runtime struct	*/


/*  SkyBoT interface procedures.
 */
Skybot      voc_skybot (double ra, double dec, double rsz, double dsz,
		double epoch);
int         voc_skybotNObjs (Skybot sb);
char       *voc_skybotStrAttr (Skybot sb, char *attr, int index);
double      voc_skybotDblAttr (Skybot sb, char *attr, int index);


/**
 *  VOC_SKYBOT --  Call the SkyBoT (Sky Bodies Tracker) service from IMCCE.
 *  This service returns a list of minor planets withing the specified
 *  search radius/box about the poition at the epoch given.  Epoch is 
 *  assumed to be JD, ra/dec are J2000 decimal degrees, search size is 
 *  given in arcsec.
 *
 *  @brief      Call the SkyBoT (Sky Bodies Tracker) service from IMCCE.
 *  @fn         sb = voc_skybot (double ra, double dec, double rsz,
 *  			double dsz, double epoch)
 *
 *  @param  ra       	RA position of query (decimal degrees)
 *  @param  dec       	Dec position of query (decimal degrees)
 *  @param  rsz       	RA size of query (arcsec)
 *  @param  dsz       	Dec size of query (arcsec)
 *  @param  epoch      	epoch of query (JD)
 *  @returns            the string value
 */
Skybot      
voc_skybot (double ra, double dec, double rsz, double dsz, double epoch)
{
    Skybot   sb = (Skybot) VOC_NULL;
    vocRes_t *result = (vocRes_t *) NULL;
    vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (0, "skybot", 0);

    /*  Make sure we've been initialized properly first.
     */
    if (vo == (VOClient *) NULL) {
        if (voc_initVOClient (NULL) == ERR) {
            fprintf (stderr, "ERROR: Can't initialize VO Client....\n");
            exit (1);
        } else if (VOC_DEBUG)
            printf ("Warning: Initializing VO Client....\n");
    }

    /* Add the parameters of the call.
     */
    msg_addFloatParam (msg, ra);
    msg_addFloatParam (msg, dec);
    msg_addFloatParam (msg, rsz);
    msg_addFloatParam (msg, dsz);
    msg_addFloatParam (msg, epoch);
    
    /* Read result and check for any faults.
     */
    if (msg_resultStatus ((result = msg_sendMsg (vo->io_chan, msg))) == ERR) {
        if (!vo->quiet)
	    fprintf (stderr, "ERROR: cannot invoke SkyBoT service\n");
    } else
        sb = msg_getIntResult (result, 0);

    if (msg) free ((void *)msg);         /* free the pointers            */
    if (result) free ((void *)result);

    return (sb);
}


/**
 *  VOC_SKYBOTSTRATTR -- Return a real-valued field as a string value. 
 *
 *  @brief      Return a real-valued field as a string value. 
 *  @fn         dval = voc_skybotStrAttr (Skybot sb, char *attr, int index)
 *
 *  @param  sb          handle to skybot query
 *  @param  attr        attribute name
 *  @param  index       object index
 *  @returns            the string value
 */
char *
voc_skybotStrAttr (Skybot sb, char *attrname, int index)
{
    char     *val = NULL, *str = NULL;
    int       len = 0;

    if (sb > 0) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (sb, "sbStrAttr", 0);

    	msg_addStringParam (msg, attrname);
    	msg_addIntParam (msg, index);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: skybotStrAttr() failed\n");
        } else {
            val = msg_getStringResult (result, 0);
            str = calloc (1, (len = strlen(val)+1));
            strncpy (str, val, len);
        }

        if (msg) free ((void *)msg);     /* free the pointers            */
        if (result) free ((void *)result);
    }

    return (str);
}


/**
 *  VOC_SKYBOTDBLATTR -- Return a real-valued field as a double-precision value.
 *
 *  @brief      Return a real-valued field as a double-precision value.
 *  @fn         dval = voc_skybotDblAttr (Skybot sb, char *attr, int index)
 *
 *  @param  sb          handle to skybot query
 *  @param  attr        attribute name
 *  @param  index       object index
 *  @returns            the double-precision value
 */
double      
voc_skybotDblAttr (Skybot sb, char *attrname, int index)
{
    double   dval = (double) 0.0;

    if (sb > 0) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (sb, "sbDblAttr", 0);

    	msg_addStringParam (msg, attrname);
    	msg_addIntParam (msg, index);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: skybotDblAttr() failed\n");
        } else
            dval = msg_getFloatResult (result, 0);

        if (msg) free ((void *)msg);     /* free the pointers            */
        if (result) free ((void *)result);

    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null object to resolverRA\n");

    return (dval);
}


/**
 *  VOC_SKYBOTNOBJS -- Return the number of objects found from the query.
 *
 *  @brief      Return the number of objects found from the query.
 *  @fn         int = voc_skybotNObjs (Skybot sb)
 *
 *  @param  sb          handle to skybot query
 *  @returns            the number of objects found 
 */
int      
voc_skybotNObjs (Skybot sb)
{
    int   ival = (int) 0;

    if (sb > 0) {
        vocRes_t *result = (vocRes_t *) NULL;
        vocMsg_t *msg = (vocMsg_t *) msg_newCallMsg (sb, "sbNObjs", 0);

        /* Send message and read result.
         */
        if (msg_resultStatus ((result=msg_sendMsg(vo->io_chan,msg))) == ERR) {
            if (!vo->quiet)
		fprintf (stderr, "ERROR: skybotNObjs() failed\n");
        } else
            ival = msg_getFloatResult (result, 0);

        if (msg) free ((void *)msg);     /* free the pointers            */
        if (result) free ((void *)result);

    } else if (!vo->quiet)
        fprintf (stderr, "ERROR: Null object to skybotNObjs\n");

    return (ival);
}
