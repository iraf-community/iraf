/**
 *  VOCSESAME_F77.C -- F77 Interface to the Sesame name resolver service.
 *
 *  @section DESCRIPTION
 *
 *  Sesame Name Resolver F77 Interface:
 *  -----------------------------------
 *
 *         vf_nameResolver  (target, sr)
 *          vf_resolverPos  (sr, pos_str, len)
 *           vf_resolverRA  (sr, radeg)
 *        vf_resolverRAErr  (sr, radeg)
 *          vf_resolverDEC  (sr, decdeg)
 *       vf_resolverDECErr  (sr, decdeg)
 * 
 *  @file       vocSesame_f77.c
 *  @author     Michael Fitzpatrick
 *  @version    June 2006
 */

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


/** 
 *  Private interface declarations.
 */
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);



/*****************************************************************************/

/**
 *  VF_NAMERESOLVER -- Query the CDS Sesame service to resolve the target
 *  name to coordinates.  The query is done when creating the Sesame object, 
 *  thereafter we simply query the object data.
 *
 *  @brief      Query the CDS Sesame name resolver service.
 *  @fn         call vfnameresolver (char *target, int *sr)
 *
 *  @param  target      name of target to be resolved
 *  @param  sr      	returned Sesame handle
 *  @returns            nothing
 */
void
VF_NAMERESOLVER (char *target, int *sr, int tlen)
{
    char *_target = sstrip (target, tlen);

    *sr = (int) voc_nameResolver (_target);

    free ((char *) _target);
}


/**
 *  VF_RESOLVERPOS --  Return a string containing the (ra,dec) position as
 *  sexagesimal strings. 
 *
 *  @brief      Get the resolved position as a string
 *  @fn         call vfresolverpos (int *srm char *target, int *sr)
 *
 *  @param  sr      	Sesame handle
 *  @param  pos         position string
 *  @returns            nothing
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


/**
 *  VF_RESOLVEROTYPE --  Return a string containing the object type description
 *
 *  @brief      Get the object type description string
 *  @fn         call vfresolverotype (int *sr, char *type)
 *
 *  @param  sr      	Sesame handle
 *  @param  type        object type string
 *  @returns            nothing
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


/**
 *  VF_RESOLVERRA --  Return the RA as a double precision value.
 *
 *  @brief      Return the RA as a double precision value.
 *  @fn         call vfresolverra (int *sr, double *ra)
 *
 *  @param  sr      	Sesame handle
 *  @param  ra          RA of object (decimal degrees)
 *  @returns            nothing
 */
void
VF_RESOLVERRA (int *sr, double *ra)
{
   *ra = voc_resolverRA ((Sesame) *sr);
}


/**
 *  VF_RESOLVERDEC --  Return the DEC as a double precision value. 
 *
 *  @brief      Return the DEC as a double precision value. 
 *  @fn         call vfresolverdec (int *sr, double *dec)
 *
 *  @param  sr      	Sesame handle
 *  @param  dec      	Dec of object (decimal degrees)
 *  @returns            nothing
 */
void
VF_RESOLVERDEC (int *sr, double *dec)
{
   *dec = voc_resolverDEC ((Sesame) *sr);
}


/**
 *  VF_RESOLVERRAERR --  Return the RA error as a double precision value.
 *
 *  @brief      Return the RA error as a double precision value.
 *  @fn         call vfresolverraerr (int *sr, double *err)
 *
 *  @param  sr      	Sesame handle
 *  @param  target      RA error of position (decimal degrees)
 *  @returns            nothing
 */
void
VF_RESOLVERRAERR (int *sr, double *err)
{
   *err = voc_resolverRAErr ((Sesame) *sr);
}


/**
 *  VF_RESOLVERDECERR --  Return the DEC error as a double precision value. 
 *
 *  @brief      Return the DEC error as a double precision value. 
 *  @fn         call vfresolverdecerr (int *sr, double *err)
 *
 *  @param  sr      	Sesame handle
 *  @param  target      Dec error of position (decimal degrees)
 *  @returns            nothing
 */
void
VF_RESOLVERDECERR (int *sr, double *err)
{
   *err = voc_resolverDECErr ((Sesame) *sr);
}
