/***************************************************************************
 *
 *  SPP Language binding for the VOClient interface.
 *
 *  Michael Fitzpatrick, NOAO, Jul 2006
 *
 **************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>

#define _VOCLIENT_LIB_
#include "VOClient.h"


/*  SPP Name mapping macros.  SPP procedure names are mappad as the first-5
**  plus the last character of a name minus any underscores.  This should
**  be done such that a unique 6-character name is produced for each SPP
**  symbol.  In these definitions the SPP code may use the long form of the
**  name in the code, the mapping is done automatically and so we need the
**  macros here so the symbol entered in the library is actually the short
**  name.
*/

#ifdef _NO_US_
#define	vx_initvoclient 	vxinit
#define	vx_closevoclient	vxclot
#define	vx_abortvoclient 	vxabot
#define	vx_validate 		vxvale
#define	vx_dbglevel 		vxdbgl

#else

#define	vx_initvoclient 	vxinit_
#define	vx_closevoclient 	vxclot_
#define	vx_abortvoclient 	vxabot_
#define	vx_validate 		vxvale_
#define	vx_dbglevel 		vxdbgl_

#endif



/* SPP Type definitions.
*/
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		NULL


/*  Public interface procedures.
**
*/
int	vx_initvoclient (XCHAR *opts);
void	vx_closevoclient (int *shutdown);
void	vx_abortvoclient (int *code, XCHAR *msg);
int	vx_validate (int *hcode);
void	vx_dbglevel (int *level);



/*  Private interface procedures.
*/
extern PKCHAR *spp2c (XCHAR *instr,  int maxch);
extern int     c2spp (PKCHAR *instr, XCHAR *outstr, int maxch);
extern int    spplen (XCHAR *str);
extern int  dal_typecode (char *typestr);
extern int  out_typecode (char *typestr);




/******************************************************************************
**  OPENVOCLIENT --  Open and initialize the VOClient interface.
*/
int
vx_initvoclient (XCHAR *opts)
{
    char *_opts = spp2c (opts, spplen (opts));

    int ier = voc_initVOClient (_opts);

    free ((char *) _opts);
    return (ier);
}


/******************************************************************************
**  CLOSEVOCLIENT -- Close and free the VOClient interface.
*/
void
vx_closevoclient (int *shutdown)
{
    voc_closeVOClient (*shutdown);
}



/******************************************************************************
**  ABORTVOCLIENT -- Close the VOClient interface and abort the application.
*/
void
vx_abortvoclient (int *code, XCHAR *msg)
{
    char *_msg = spp2c (msg, spplen (msg));

    voc_abortVOClient (*code, _msg);

    free ((char *) _msg);
}


/******************************************************************************
** VALIDATE -- Validate and object handle in the daemon.
*/
int 
vx_validate (int *hcode) { return (voc_validateObject (*hcode)); }


/******************************************************************************
** DEBUGLEVEL -- Set the package debugging output level.
*/
void 
vx_dbglevel (int *level) { 
    extern void voc_debugLevel ();
    voc_debugLevel (*level); 
}
