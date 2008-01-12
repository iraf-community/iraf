/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#define import_spp
#define import_knames
#include <iraf.h>

#include "zos.h"

/* ZZSETK -- Internal kernel routine, used by the zmain to set the values
 * of certain internal kernel parameters.
 */
int ZZSETK ( XCHAR *ospn, XCHAR *osbfn, XINT *prtype, XINT *isatty, XINT *in, 
	     XINT *out )
{
	safe_strcpy (os_process_name, SZ_PROCNAME, (const char *)ospn);
	safe_strcpy ((char *)osfn_bkgfile, SZ_PATHNAME/sizeof(PKCHAR)+1, 
		     (const char *)osbfn);
	save_prtype = *prtype;
	ipc_isatty = *isatty;
	ipc_in = *in;
	ipc_out = *out;

	return XOK;
}
