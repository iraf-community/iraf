/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <ctype.h>

#define	import_kernel
#define	import_knames
#define import_spp
#include <iraf.h>

#define	SZ_TESTBLOCK	1024		/* for TEXT/BINARY heuristic	*/
#define	MAX_LINELEN	256		/* when looking for newlines	*/
#define	R		04		/* UNIX access() codes		*/
#define W		02
#define ctrlcode(c)	((c) >= '\007' && (c) <= '\017')


/* ZFACSS -- Try to determine if FILE is accessible in the indicated MODE.
 * If file is accessible for reading and TYPE is given as TEXT_FILE,
 * look at the first block of data to see if it is legal text data.
 * ACCESS(file,0,0) merely checks that the file exists.  Any file is a
 * legal binary file.
 */
int
ZFACSS (
  PKCHAR  *fname,
  XINT	  *mode, 
  XINT 	  *type,
  XINT 	  *status
)
{
	static	char modebits[] = { 0, R, R|W, W, R|W };
	register char	*ip, ch;
	register int	n;
	char	buf[SZ_TESTBLOCK];
	int	fd, acmode, accessible, nchars, newline_seen;
	struct	stat fi;

	/* Null filename? */
	if (*(char *)fname == EOS) {
	    *status = NO;
	    return (NO);
	}

	/* Map IRAF access mode into UNIX access mode.
	 */
	if (*mode >= READ_ONLY && *mode <= APPEND)
	    acmode = modebits[*mode];
	else if (*mode == 0 && *type != 0)
	    acmode = R;
	else
	    acmode = 0;

	/* Is file accessible with the given mode.
	 */
	accessible = (access ((char *)fname, acmode) == 0);

	if (accessible && *type == DIRECTORY_FILE) {
	    stat ((char *)fname, &fi);
	    if (fi.st_mode & S_IFDIR)
		*status = YES;
	    else
		*status = NO;
	    return (*status);

	} else if (!accessible && *type == SYMLINK_FILE) {
	    lstat ((char *)fname, &fi);
	    if (fi.st_mode & S_IFLNK)
		*status = YES;
	    else
		*status = NO;

	    return (*status);
	}

	/* If we have to check the file type (text or binary), then we must
	 * actually look at some file data since UNIX does not discriminate
	 * between text and binary files.  NOTE that this heuristic is not
	 * completely reliable and can fail, although in practice it does
	 * very well.
	 */
	if (accessible && (acmode & R) && *type != 0) {
	    stat ((char *)fname, &fi);

	    /* Do NOT read from a special device (may block) */
	    if ((fi.st_mode & S_IFMT) & S_IFREG) {
		/* If we are testing for a text file the portion of the file
		 * tested must consist of only printable ascii characters or
		 * whitespace, with occasional newline line delimiters.
		 * Control characters embedded in the text will cause the
		 * heuristic to fail.  We require newlines to be present in
		 * the text to disinguish the case of a binary file containing
		 * only ascii data, e.g., a cardimage file.
		 */
		fd = open ((char *)fname, O_RDONLY);
		if (fd >= 0 && (nchars = read (fd, buf, SZ_TESTBLOCK)) > 0) {
		    ip = buf;
		    for (n=nchars, newline_seen=0;  --n >= 0;  ) {
			ch = *ip++;
			if (ch == '\n')
			    newline_seen++;
			else if (!isprint(ch) && !isspace(ch) && !ctrlcode(ch))
			    break;
		    }

		    if (*type == TEXT_FILE) {
			if (n >= 0 || (nchars > MAX_LINELEN && !newline_seen))
			    accessible = NO;
		    } else if (*type == BINARY_FILE && n < 0)
			accessible = NO;
		    close (fd);
		}
	    } else if (fi.st_mode & S_IFCHR && *type != TEXT_FILE)
		accessible = NO;
	}
		
	(*status) = accessible;

	return (*status);
}
