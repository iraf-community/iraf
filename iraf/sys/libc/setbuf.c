/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_fset
#include <iraf.h>

/* SETBUF -- Assign a buffer to be used by the i/o system to access a file.
 * Should be called after opening the file but before doing any i/o.
 */
int setbuf ( FILE *fp, char *buf )
{
	setbuffer (fp, buf, BUFSIZ);
	return 0;
}


/* SETBUFFER -- Assign a buffer of arbitrary size to be used by the i/o system
 * to access a file.  Should be called after opening the file but before doing
 * any i/o.  If the the pointer buf has the value NULL, i/o is unbuffered
 * (or as close to unbuffered as we can manage).
 */
int setbuffer ( FILE *fp, char *buf, size_t size )
{
	int fd = fileno(fp);

	if (buf == NULL)
	    c_fsetl (fd, F_BUFSIZE, 1);
	else {
	    c_fsetp (fd, F_BUFPTR, Memcptr(buf));
	    c_fsetl (fd, F_BUFSIZE, size);
	}

	return 0;
}


/* SETLINEBUF -- Set line buffered mode for a file.  A line buffered file
 * buffers each line and flushes it to the output device when newline is
 * seen.  We may be even after doing i/o to the file.
 */
int setlinebuf ( FILE *fp )
{
	int fd = fileno(fp);

	if (c_fstatl (fd, F_BUFSIZE) < SZ_LINE)
	    c_fsetl (fd, F_BUFSIZE, SZ_LINE);
	c_fseti (fd, F_FLUSHNL, YES);

	return 0;
}
