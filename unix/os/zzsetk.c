/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#define	import_spp
#define	import_knames
#include <iraf.h>

extern	char os_process_name[];
extern	PKCHAR osfn_bkgfile[];
extern	int save_prtype;
extern	ipc_isatty;
extern	ipc_in, ipc_out;

/* ZZSETK -- Internal kernel routine, used by the zmain to set the values
 * of certain internal kernel parameters.
 */
ZZSETK (ospn, osbfn, prtype, isatty, in, out)
char	*ospn;
char	*osbfn;
int	prtype;
int	isatty;
int	in, out;
{
	strcpy (os_process_name, ospn);
	strcpy (osfn_bkgfile, osbfn);
	save_prtype = prtype;
	ipc_isatty = isatty;
	ipc_in = in;
	ipc_out = out;
}
