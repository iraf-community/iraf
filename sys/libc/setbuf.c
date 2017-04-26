/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_fset
#include <iraf.h>


/* SETBUF -- Assign a buffer to be used by the i/o system to access a file.
** Should be called after opening the file but before doing any i/o.
*/
void
setbuf (
  FILE	*fp,
  char	*buf
)
{
	void setbuffer();

	setbuffer (fp, buf, BUFSIZ);
}


/* SETBUFFER -- Assign a buffer of arbitrary size to be used by the i/o system
** to access a file.  Should be called after opening the file but before doing
** any i/o.  If the the pointer buf has the value NULL, i/o is unbuffered
** (or as close to unbuffered as we can manage).
*/
void
setbuffer (
  FILE	*fp,
  char	*buf,
  int	size
)
{
	register XINT	fd = fileno(fp);


	if (buf == NULL)
	    c_fseti (fd, F_BUFSIZE, 1);
	else {
	    c_fseti (fd, F_BUFPTR, Memcptr(buf));
	    c_fseti (fd, F_BUFSIZE, size);
	}
}


/* SETLINEBUF -- Set line buffered mode for a file.  A line buffered file
 * buffers each line and flushes it to the output device when newline is
 * seen.  We may be even after doing i/o to the file.
 */
void
setlinebuf (
  FILE	*fp
)
{
	register XINT	fd = fileno(fp);

	extern int  c_fstati();
	extern void c_fseti();


	if (c_fstati (fd, F_BUFSIZE) < SZ_LINE)
	    c_fseti (fd, F_BUFSIZE, SZ_LINE);
	c_fseti (fd, F_FLUSHNL, YES);
}
