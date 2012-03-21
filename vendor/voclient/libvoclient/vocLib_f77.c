/***************************************************************************
 *
 *  VOCF77.C  -- Fortran binding for the VOClient interface.  As part of 
 *  the binding we map the interface procedure names and convert string
 *  constants as needed per Fortran rules.  Note that another aspect of
 *  the fortran calling convention is that the length of strings in the
 *  argument list are appended to the call stack.  As C code we need to
 *  take this into account when defining the interface.
 *
 *  M. Fitzpatrick, NOAO, Jul 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/*  Fortran Interface Definitions.
 *  
 *  Fortran compilers on various platforms may append one or more trailing 
 *  underscores to symbol names, we'll use macros for the interface names 
 *  and use defines to see what the symbol name is.
 */

#ifdef _NO_US_
#define	VF_INITVOCLIENT 	vfinitvoclient 
#define	VF_CLOSEVOCLIENT 	vfclosevoclient 
#define	VF_ABORTVOCLIENT 	vfabortvoclient 
#define	VF_DBGLEVEL 		vfdbglevel 
#define	VF_VALIDATE 		vfvalidate 
#define	VF_MKFNAME 		vfmkfname

#else
#define	VF_INITVOCLIENT 	vfinitvoclient_
#define	VF_CLOSEVOCLIENT 	vfclosevoclient_
#define	VF_ABORTVOCLIENT 	vfabortvoclient_
#define	VF_DBGLEVEL 		vfdbglevel_
#define	VF_VALIDATE 		vfvalidate 
#define	VF_MKFNAME 		vfmkfname_

#endif


/*  Function prototypes.
*/
void VF_INITVOCLIENT (char *opts, int *ier, int len);
void VF_CLOSEVOCLIENT (int *shutdown, int *ier);
void VF_ABORTVOCLIENT (int *code, char *msg, int *ier, int len);
void VF_DBGLEVEL (int *level);
void VF_VALIDATE (int *hcode, int *flag);
void VF_MKFNAME (char *root, int *num, char *fname, int *len, 
	int rlen, int flen);


/** Private interface declarations.
**/
extern char *sstrip (char *instr, int len);
extern void  spad (char *outstr, int len);
extern int   typecode (char *typestr);

extern void voc_debugLevel ();



/******************************************************************************
**  OPENVOCLIENT --  Open and initialize the VOClient interface.
*/
void
VF_INITVOCLIENT (char *opts, int *ier, int len)
{
    char *_opts = sstrip (opts, len);

    *ier = voc_initVOClient (_opts);

    free ((char *) _opts);
}


/******************************************************************************
**  CLOSEVOCLIENT -- Close and free the VOClient interface.
*/
void
VF_CLOSEVOCLIENT (int *shutdown, int *ier)
{
    voc_closeVOClient (*shutdown);
    *ier = OK;
}



/******************************************************************************
**  ABORTVOCLIENT -- Close the VOClient interface and abort the application.
*/
void
VF_ABORTVOCLIENT (int *code, char *msg, int *ier, int len)
{
    char *_msg = sstrip (msg, len);

    voc_abortVOClient (*code, _msg);
    *ier = OK;

    free ((char *) _msg);
}


/******************************************************************************
** VALIDATE -- Validate an object in the daemon.
*/
void 
VF_VALIDATE (int *hcode, int *flag) { *flag = voc_validateObject (*hcode); }



/******************************************************************************
** DEBUGLEVEL -- Set the package debugging output level.
*/
void 
VF_DBGLEVEL (int *level) { voc_debugLevel(*level); }



/******************************************************************************
** MKFNAME -- Utility procedure for creating a file name.
*/
void
VF_MKFNAME (char *root, int *num, char *fname, int *len, int rlen, int flen)
{
    memset (fname, 0, flen);
    sprintf (fname, sstrip (root,rlen), *num);
    *len = strlen (fname);
    spad (fname, flen);
}
