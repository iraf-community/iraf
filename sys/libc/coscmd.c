/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_OSCMD -- Send a command to the host system.  If the filename strings
** are nonnull the kernel will attempt to redirect the standard i/o streams
** to the indicated streams during execution of the command.  OK is returned
** if execution is completes successfully, otherwise a positive integer error
** code is returned.
*/
int
c_oscmd (
  char	*cmd,			/* command to be executed	*/
  char	*infile,		/* stdin file			*/
  char	*outfile,		/* stdout file			*/
  char	*errfile		/* stderr file			*/
)
{
	XCHAR	spp_infile[SZ_FNAME+1];
	XCHAR	spp_outfile[SZ_FNAME+1];
	XCHAR	spp_errfile[SZ_FNAME+1];

	c_strupk (infile,  spp_infile,  SZ_FNAME);
	c_strupk (outfile, spp_outfile, SZ_FNAME);
	c_strupk (errfile, spp_errfile, SZ_FNAME);

	return (OSCMD (c_sppstr(cmd), spp_infile, spp_outfile, spp_errfile));
}
