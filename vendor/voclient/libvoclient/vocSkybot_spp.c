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
 *
 *  @file       vocSkybot_f77.c
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


#ifdef _NO_US_

#define vx_skybot 		vxskyt
#define vx_skybotnobjs 		vxskys
#define vx_skybotstr 		vxskyr
#define vx_skybotdbl 		vxskyl

#else

#define vx_skybot 		vxskyt_
#define vx_skybotnobjs 		vxskys_
#define vx_skybotstr 		vxskyr_
#define vx_skybotdbl 		vxskyl_

#endif


/*  SPP Type definitions.
 */
#define XCHAR           short
#define PKCHAR          char
#define XINT            int
#define XEOS            NULL


/*  Public interface procedures.
*/
int     vx_skybot (double *ra, double *dec, double *rsz, double *dsz,
		double *epoch);
int     vx_skybotnobjs (int *sb);
int     vx_skybotstr (int *sb, XCHAR *attr, int *index, XCHAR *outstr,
		int *maxch);
double  vx_skybotdbl (int *sb, XCHAR *attr, int *index);


/*  Private interface procedures.
*/
extern PKCHAR *spp2c (XCHAR *instr,  int maxch);
extern int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
extern int    spplen (XCHAR *str);



/**
 *  VX_SKYBOT --  Call the SkyBoT (Sky Bodies Tracker) service from IMCCE.
 *  This service returns a list of minor planets withing the specified
 *  search radius/box about the poition at the epoch given.  Epoch is 
 *  assumed to be JD, ra/dec are J2000 decimal degrees, search size is 
 *  given in arcsec.
 *
 *  @brief      Call the SkyBoT (Sky Bodies Tracker) service from IMCCE.
 *  @fn         sb = vx_skybot (double *ra, double *dec, double *rsz,
 *                      double *dsz, double *epoch)
 *
 *  @param  ra          RA position of query (decimal degrees)
 *  @param  dec         Dec position of query (decimal degrees)
 *  @param  rsz         RA size of query (arcsec)
 *  @param  dsz         Dec size of query (arcsec)
 *  @param  epoch       epoch of query (JD)
 *  @returns            handle to Skybot query
 */
int      
vx_skybot (double *ra, double *dec, double *rsz, double *dsz, double *epoch)
{
    return ( (int) voc_skybot (*ra, *dec, *rsz, *dsz, *epoch) );
}


/**
 *  VX_SKYBOTSTR -- Return a real-valued field as a string value. 
 *
 *  @brief      Return a real-valued field as a string value.
 *  @fn         len = vx_skybotstr (int *sb, XCHAR *attr, int *index, 
 *				XCHAR *out, int *maxch)
 *
 *  @param  sb          handle to skybot query
 *  @param  attr        attribute name
 *  @param  index       object index
 *  @param  out         output string value
 *  @param  maxch       max length of output string
 *  @returns            length of string value
 */
int
vx_skybotstr (int *sb, XCHAR *attrname, int *index, XCHAR *outstr, int *maxch)
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    char *_result = voc_skybotStrAttr ((Skybot) *sb, _attrname, *index);
    int  len = c2spp (_result, outstr, *maxch);

    free ((char *) _result);
    free ((char *) _attrname);

    return (len);
}


/**
 *  VX_SKYBOTDBL -- Return a real-valued field as a double-precision value.
 *
 *  @brief      Return a real-valued field as a double-precision value.
 *  @fn         dval = vx_skybotdbl (int *sb, char *attr, int *index)
 *
 *  @param  sb          handle to skybot query
 *  @param  attr        attribute name
 *  @param  index       object index
 *  @returns            double-precision value
 */
double      
vx_skybotdbl (int *sb, XCHAR *attrname, int *index)
{
    char *_attrname = spp2c (attrname, spplen (attrname));

    double dval = voc_skybotDblAttr (*sb, _attrname, *index);

    free ((char *) _attrname);
    return (dval);
}


/**
 *  VX_SKYBOTNOBJS -- Return the number of objects found from the query.
 *
 *  @brief      Return the number of objects found from the query.
 *  @fn         nobjs = vx_skybotnobjs (int *sb)
 *
 *  @param  sb          handle to skybot query
 *  @returns            nothing
 */
int      
vx_skybotnobjs (int *sb)
{
    return ( voc_skybotNObjs (*sb) );
}
