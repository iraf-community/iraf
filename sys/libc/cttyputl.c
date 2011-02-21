/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_xnames
#include <iraf.h>


/* C_TTYPUTLINE -- Put a line of text to the output device.  Any device
** independent control characters embedded in the text, e.g., tab, newline,
** formfeed, backspace, or the special control codes SO (enter standout mode)
** or SI (leave standout mode) are converted as necessary to drive the device.
** Unknown control codes are converted to printable sequences (e.g. ^C) if
** the map_cc flag is set.
*/
void
c_ttyputline (
  XINT	fd,			/* output file			*/
  XINT	tty,			/* tty descriptor		*/
  char	*line,			/* line to be output		*/
  int	map_cc			/* map unknown ctrl chars	*/
)
{
	XINT  x_fd = fd, x_tty = tty, x_map_cc = map_cc;

	TTYPUTLINE (&x_fd, &x_tty, c_sppstr(line), &x_map_cc);
}
