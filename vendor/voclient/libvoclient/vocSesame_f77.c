/***************************************************************************
**
**  Sesame Name Resolver F77 Interface:
**  -----------------------------------
**
**         vf_nameResolver (target, sr)
**          vf_resolverPos (sr, pos_str, len)
**           vf_resolverRA (sr, radeg)
**          vf_resolverDEC (sr, decdeg)
** 
**
**  Michael Fitzpatrick, NOAO, July 2006
**
***************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"

#ifdef _NO_US_

#define VF_NAMERESOLVER		vfnameresolver
#define VF_RESOLVERRA		vfresolverra
#define VF_RESOLVERDEC		vfresolverdec
#define VF_RESOLVERPOS		vfresolverpos
#define VF_RESOLVERRAERR	vfresolverraerr
#define VF_RESOLVERDECERR	vfresolverdecerr
#define VF_RESOLVEROTYPE	vfresolverotype

#else

#define VF_NAMERESOLVER		vfnameresolver_
#define VF_RESOLVERRA		vfresolverra_
#define VF_RESOLVERDEC		vfresolverdec_
#define VF_RESOLVERPOS		vfresolverpos_
#define VF_RESOLVERRAERR	vfresolverraerr_
#define VF_RESOLVERDECERR	vfresolverdecerr_
#define VF_RESOLVEROTYPE	vfresolverotype_

#endif


/** Private interface declarations.
**/
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);




/******************************************************************************
**  NAMERESOLVER -- Query the CDS Sesame service to resolve the target name
**  to coordinates.  The query is done when creating the Sesame object, 
**  thereafter we simply query the object data.
*/
void
VF_NAMERESOLVER (char *target, int *sr, int tlen)
{
    char *_target = sstrip (target, tlen);

    *sr = (int) voc_nameResolver (_target);

    free ((char *) _target);
}


/******************************************************************************
**  RESOLVERPOS --  Return a string containing the (ra,dec) position as
**  sexagesimal strings. 
*/
void
VF_RESOLVERPOS (int *sr, char *pos, int *len, int plen)
{
    char *_result = voc_resolverPos ((Sesame) *sr);

    memset (pos, 0, plen);
    if ((*len = strlen(_result)) > plen)
        fprintf (stderr, "Warning: truncating POS string: len=%d maxch=%d\n",
            *len, plen);
    spad (strncpy (pos, _result, *len), plen);

    free ((char *) _result);
}


/******************************************************************************
**  RESOLVEROTYPE --  Return a string containing the object type description
*/
void
VF_RESOLVEROTYPE (int *sr, char *otype, int *len, int olen)
{
    char *_result = voc_resolverOtype ((Sesame) *sr);

    memset (otype, 0, olen);
    if ((*len = strlen(_result)) > olen)
        fprintf (stderr, "Warning: truncating OType string: len=%d maxch=%d\n",
            *len, olen);
    spad (strncpy (otype, _result, *len), olen);

    free ((char *) _result);
}


/******************************************************************************
**  RESOLVERRA --  Return the RA as a double precision value.
*/
void
VF_RESOLVERRA (int *sr, double *ra)
{
   *ra = voc_resolverRA ((Sesame) *sr);
}


/******************************************************************************
**  RESOLVERDEC --  Return the DEC as a double precision value. 
*/
void
VF_RESOLVERDEC (int *sr, double *dec)
{
   *dec = voc_resolverDEC ((Sesame) *sr);
}


/******************************************************************************
**  RESOLVERRAERR --  Return the RA error as a double precision value.
*/
void
VF_RESOLVERRAERR (int *sr, double *err)
{
   *err = voc_resolverRAErr ((Sesame) *sr);
}


/******************************************************************************
**  RESOLVERDECERR --  Return the DEC error as a double precision value. 
*/
void
VF_RESOLVERDECERR (int *sr, double *err)
{
   *err = voc_resolverDECErr ((Sesame) *sr);
}
