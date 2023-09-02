/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


/*
 * OS Character I/O.  This set of routines are provided as a workaround in
 * the event that the host system cannot execute FTELL/FSEEK reliably (VMS/C
 * could not).  The idea here is to keep track of the character offset from
 * the beginning of the file.  K_FTELL returns the character offset.  K_FSEEK
 * rewinds the file and reads characters forward to the indicated offset.
 * K_GETC keeps a count of the file position.  (the k_ stands for kludge).
 */

extern	int debug;

FILE * k_fopen (char *fname, char *mode)
{
	register FILE	*fp;

	if ((fp = fopen (fname, mode)) == NULL)
	    return (NULL);
	return ((FILE *)fp);
}

int k_fclose (FILE *fp)
{
	return (fclose (fp));
}

int k_getc (FILE *fp)
{
	return (getc (fp));
}

int k_fseek (FILE *fp, long offset, int type)
{
	rewind (fp);
	return (fseek (fp, offset, type));
}

long k_ftell (FILE *fp)
{
	return (ftell (fp));
}
