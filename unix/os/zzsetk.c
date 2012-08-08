/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>

#define	import_spp
#define	import_knames
#include <iraf.h>

extern	char os_process_name[];
extern	PKCHAR osfn_bkgfile[];
extern	int save_prtype;
extern	int ipc_isatty;
extern	int ipc_in, ipc_out;

/* ZZSETK -- Internal kernel routine, used by the zmain to set the values
 * of certain internal kernel parameters.
 */
int
ZZSETK (
  char	*ospn,
  char	*osbfn,
  int	prtype,
  int	isatty,
  int	in, 
  int   out
)
{
	strcpy (os_process_name, ospn);
	strcpy ((char *)osfn_bkgfile, osbfn);
	save_prtype = prtype;
	ipc_isatty = isatty;
	ipc_in = in;
	ipc_out = out;

	return (XOK);
}
