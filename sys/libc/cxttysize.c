/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_XTTYSIZE -- Get the terminal screen size, dynamically querying the
** terminal for the screen size if the terminal has this capabability (e.g.,
** a workstation window).
*/
void
c_xttysize (
  int	*ncols,			/* ncols (output)	*/
  int	*nlines			/* nlines (output)	*/
)
{
	XINT  x_ncols, x_nlines;

	XTTYSIZE (&x_ncols, &x_nlines);
	*ncols = x_ncols;
	*nlines = x_nlines;
}
