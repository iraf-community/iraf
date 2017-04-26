/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#define import_fset
#define import_stdio
#define import_error
#include <iraf.h>


/* C_WRITE -- FIO write to a file.  Write exactly nbytes bytes from the buffer
** buf to the stream fd.  If the actual file is a text file C chars are output
** as XCHARs.  If the actual file is a binary file no conversion is performed,
** but an integral number of XCHARs are always written.
*/
int
c_write (
  XINT	fd,			/* FIO file descriptor			*/
  char	*buf,			/* buffer containing data to be written	*/
  int	nbytes			/* nbytes to be written			*/
)
{
	XINT  x_fd = fd;

	if (c_fstati (fd, F_TYPE) == TEXT_FILE) {
	    register FILE	*fp = FDTOFP(fd);
	    register char	*ip;
	    register int	n = nbytes;

	    for (ip=buf;  --n >= 0;  ip++)
		putc (*ip, fp);
	    if (ferror (fp))
		c_erract (EA_ERROR);

	} else {
	    XINT	x_nchars = (nbytes + sizeof(XCHAR)-1) / sizeof(XCHAR);
	    XCHAR	*bp = (XCHAR *)buf;

	    /* Verify that the pointer coercion char->XCHAR->char is legal,
	     * i.e., that the char pointer is aligned to an XCHAR word
	     * boundary if required on this machine.
	     */
	    if (buf != (char *) bp)
		return (ERR);
	    WRITE (&x_fd, bp, &x_nchars);
	}

	return (nbytes);
}
