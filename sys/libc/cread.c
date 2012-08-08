/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define import_spp
#define	import_libc
#define	import_xnames
#define import_stdio
#define import_error
#define import_fset
#include <iraf.h>


/* C_READ -- FIO read from a file.  Read up to maxbytes bytes from the stream
** fd into the buffer buf.  If the device associated with file fd is a record
** structured device a single record is read, and the byte count will generally
** be less than the maximum.  If the physical record was larger than maxbytes
** the remainder of the record is returned in successive reads.  If the actual
** file is a text file FIO XCHARs are returned as C chars.  If the actual file
** is a binary file no conversion is performed, and an integral number of XCHARs
** are read.
**
** For reasons of consistency with SPP usage, EOF is returned when end of file
** is reached (fread returns 0), and an error action is taken if a file read
** error occurs.  We cannot return ERR when an error occurs since ERR and EOF
** have the same value in STDIO land.  IFERR may be used to catch file read
** errors.
*/
int
c_read (
  XINT	fd,			/* FIO file descriptor		*/
  char	*buf,			/* output buffer		*/
  int	maxbytes		/* max bytes to read		*/
)
{
	XINT  x_fd = fd;
	int   nchars_read;


	if (c_fstati (fd, F_TYPE) == TEXT_FILE) {
	    register char	*op = buf;
	    register int	ch, n = maxbytes;
	    register FILE	*fp = FDTOFP(fd);

	    while (--n >= 0 && (ch = getc(fp)) >= 0) {
		*op++ = ch;
		if (ch == '\n')
		    break;
	    }
	    if (ferror (fp))
		c_erract (EA_ERROR);
	    if (!(nchars_read = op - buf))
		nchars_read = XEOF;

	} else {
	    XINT	x_maxchars = maxbytes / sizeof(XCHAR);
	    XCHAR	*bp = (XCHAR *)buf;

	    /* Verify that the pointer coercion char->XCHAR->char is legal,
	     * i.e., that the char pointer is aligned to an XCHAR word
	     * boundary if required on this machine.
	     */
	    if (buf != (char *)bp)
		c_error (1, "c_read: buffer not xchar aligned");
	    
	    if ((nchars_read = READ (&x_fd, bp, &x_maxchars)) > 0)
		nchars_read *= sizeof(XCHAR);
	}

	return (nchars_read == XEOF ? EOF : nchars_read);
}
