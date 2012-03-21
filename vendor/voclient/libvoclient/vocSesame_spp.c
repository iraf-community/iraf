/***************************************************************************
 *
 *  Sesame Name Resolver SPP Interface:
 *  -----------------------------------
 *
 *          sr = vx_nameResolver (target)
 *      pos_str = vx_resolverPos (sr)
 *         radeg = vx_resolverRA (sr)
 *       decdeg = vx_resolverDEC (sr)
 *    typ_str = vx_resolverOtype (sr)
 *      raerr = vx_resolverRAErr (sr)
 *    decerr = vx_resolverDECErr (sr)
 * 
 *
 *  Michael Fitzpatrick, NOAO, June 2006
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

#define vx_nameresolver		vxnamr
#define vx_resolverra		vxresa
#define vx_resolverdec		vxresc
#define vx_resolverpos		vxress
#define vx_resolverotype	vxrese

#define vx_errresolverra	vxerra
#define vx_errresolverdec	vxerrc

#else

#define vx_nameresolver		vxnamr_
#define vx_resolverra		vxresa_
#define vx_resolverdec		vxresc_
#define vx_resolverpos		vxress_
#define vx_resolverotype	vxrese_

#define vx_errresolverra	vxerra_
#define vx_errresolverdec	vxerrc_


#endif


/* SPP Type definitions.
*/
#define XCHAR           short
#define PKCHAR          char
#define XINT            int
#define XEOS            NULL


/*  Public interface procedures
*/
int	vx_nameresolver (XCHAR *target);
int	vx_resolverpos (int *sr, XCHAR *pos, int *len);
double	vx_resolverra (int *sr);
double	vx_resolverdec (int *sr);
int	vx_resolverotype (int *sr, XCHAR *type, int *len);
double	vx_errresolverra (int *sr);
double	vx_errresolverdec (int *sr);


/*  Private interface procedures.
*/
extern PKCHAR *spp2c (XCHAR *instr,  int maxch);
extern int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
extern int    spplen (XCHAR *str);




/******************************************************************************
**  NAMERESOLVER -- Query the CDS Sesame service to resolve the target name
**  to coordinates.  The query is done when creating the Sesame object, 
**  thereafter we simply query the object data.
*/
int
vx_nameresolver (XCHAR *target)
{
    char *_target = spp2c (target, spplen (target));

    Sesame   ier = (Sesame) voc_nameResolver (_target);

    free ((char *) _target);
    return (ier);
}


/******************************************************************************
**  RESOLVERPOS --  Return a string containing the (ra,dec) position as
**  sexagesimal strings. 
*/
int
vx_resolverpos (int *sr, XCHAR *pos, int *maxch)
{
    char *_result = voc_resolverPos ((Sesame) *sr);
    int  len = c2spp (_result, pos, *maxch);

    free ((char *) _result);
    return (len);
}


/******************************************************************************
**  RESOLVEROTYPE --  Return a string containing the object type description.
*/
int
vx_resolverotype (int *sr, XCHAR *type, int *maxch)
{
    char *_result = voc_resolverOtype ((Sesame) *sr);
    int  len = c2spp (_result, type, *maxch);

    free ((char *) _result);
    return (len);
}


/******************************************************************************
**  RESOLVERRA --  Return the RA as a double precision value.
*/
double      
vx_resolverra (int *sr)
{
   return ( voc_resolverRA ((Sesame) *sr) );
}


/******************************************************************************
**  RESOLVERDEC --  Return the DEC as a double precision value. 
*/
double      
vx_resolverdec (int *sr)
{
   return ( voc_resolverDEC ((Sesame) *sr) );
}


/******************************************************************************
**  ERRRESOLVERRA --  Return the RA as a double precision value.
*/
double      
vx_errresolverra (int *sr)
{
   return ( voc_resolverRAErr ((Sesame) *sr) );
}


/******************************************************************************
**  ERRRESOLVERDEC --  Return the DEC as a double precision value. 
*/
double      
vx_errresolverdec (int *sr)
{
   return ( voc_resolverDECErr ((Sesame) *sr) );
}
