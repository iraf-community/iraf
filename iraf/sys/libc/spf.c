/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_xnames
#include <iraf.h>

/*
 * SPF.C -- String spoolfile utility package.  Used to capture in a string
 * buffer the output of a routine set up to write to a file.
 *
 * Example:
 *
 *	fd = spf_open (buf, maxch)
 *	fprop (pp, fdopen(fd,"w"));
 *	spf_close (fd);
 *
 * leaves the output of the fprop() function in the user buffer "buf".
 */

static	XCHAR	*spf_buf;
static	char	*spf_userbuf;
static	int	spf_bufsize;

/* SPF_OPEN -- Spoolfile open.  Open a string spoolfile to be written into as
 * a file, using ordinary file i/o.  Only one such file can be open at a time.
 */
/* buf     : user string buffer   */
/* bufsize : buffer size of storage */
int spf_open ( char *buf, size_t bufsize )
{
	XINT	mode = NEW_FILE;
	XINT	x_maxch = bufsize -1;

	spf_userbuf = buf;
	spf_bufsize = bufsize;

	/* Malloc always returns a buffer which aligned to at least XCHAR. */
	spf_buf = (XCHAR *) malloc ((bufsize + 1) * sizeof(XCHAR));

	return (STROPEN (spf_buf, &x_maxch, &mode));
}


/* SPF_CLOSE -- Close the spoolfile string, which should have been written
 * into via file i/o while the string was open.  This leaves SPP chars in
 * the string; pack the string and return a pointer to the string.
 */
/* fd : file descriptor of stringbuf */
void spf_close ( int fd )
{
	XINT	x_fd = fd;
	CLOSE (&x_fd);
	c_strpak (spf_buf, spf_userbuf, spf_bufsize);
	free ((void *)spf_buf);
}
