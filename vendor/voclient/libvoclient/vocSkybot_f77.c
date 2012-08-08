/***************************************************************************
 *
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
 *  Michael Fitzpatrick, NOAO, August 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


#ifdef _NO_US_

#define VF_SKYBOT 		vfskybot
#define VF_SKYBOTNOBJS 		vfskybotnobjs
#define VF_SKYBOTSTR 		vfskybotstr
#define VF_SKYBOTDBL 		vfskybotdbl

#else

#define VF_SKYBOT 		vfskybot_
#define VF_SKYBOTNOBJS 		vfskybotnobjs_
#define VF_SKYBOTSTR 		vfskybotstr_
#define VF_SKYBOTDBL 		vfskybotdbl_

#endif


/*  Private interface procedures.
*/
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);




/******************************************************************************
**  SKYBOT --  Call the SkyBoT (Sky Bodies Tracker) service from IMCCE.
**  This service returns a list of minor planets withing the specified
**  search radius/box about the poition at the epoch given.  Epoch is 
**  assumed to be JD, ra/dec are J2000 decimal degrees, search size is 
**  given in arcsec.
*/
void      
VF_SKYBOT (double *ra, double *dec, double *rsz, double *dsz, double *epoch,
	int *sb)
{
    *sb = (int) voc_skybot (*ra, *dec, *rsz, *dsz, *epoch);
}


/******************************************************************************
**  SKYBOTDBLATTR -- Return a real-valued field as a string value. 
**  Notice the index begins at 1 and not 0.
*/
void
VF_SKYBOTSTR (int *sb, char *attrname, int *index, char *outstr, int *len,
	int alen, int olen)
{
    char *_attrname = sstrip (attrname, alen);

    char *_result = voc_skybotStrAttr ((Skybot) *sb, _attrname, *index-1);
    memset (outstr, 0, olen);
    if ((*len = strlen(_result)) > olen)
        fprintf (stderr, "Warning: truncating attr string: len=%d maxch=%d\n",
            *len, olen);
    spad (strncpy (outstr, _result, *len), olen);

    free ((char *) _result);
    free ((char *) _attrname);
}


/******************************************************************************
**  SKYBOTDBLATTR -- Return a real-valued field as a double-precision value.
**  Notice the index begins at 1 and not 0.
*/
void      
VF_SKYBOTDBL (int *sb, char *attrname, int *index, double *dval, int alen)
{
    char *_attrname = sstrip (attrname, alen);

    *dval = voc_skybotDblAttr (*sb, _attrname, *index-1);

    free ((char *) _attrname);
}


/******************************************************************************
**  SKYBOTNOBJS -- Return the number of objects found from the query.
*/
void      
VF_SKYBOTNOBJS (int *sb, int *nobjs)
{
    *nobjs = voc_skybotNObjs (*sb);
}
