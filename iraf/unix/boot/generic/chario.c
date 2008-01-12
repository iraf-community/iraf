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

struct context {
	FILE	*fp;			/* file descriptor	*/
	long	fpos;			/* saved file pointer	*/
	char	fname[512];		/* file being scanned	*/
};

FILE *k_fopen ( const char *fname, const char *mode )
{
	struct context *cx;
	FILE *fp;

	if ((fp = fopen (fname, mode)) == NULL)
	    return (NULL);

	cx = (struct context *) malloc (sizeof(struct context));
	strncpy (cx->fname, fname, 512);
	cx->fname[512-1] = '\0';
	cx->fpos = 0;
	cx->fp = fp;

	return ((FILE *)cx);
}


int k_fclose ( FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	int status;

	status = fclose (cx->fp);
	free (cx);

	return (status);
}

#ifdef vms

int k_getc ( FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	int ch;

	cx->fpos++;
	if (debug > 3) {
	    if ((ch = getc (cx->fp)) > 0)
		printf ("%5ld %03o %c\n", cx->fpos, ch, ch > 040 ? ch : 040);
	    return (ch);
	} else
	    return (getc (cx->fp));
}

char *k_fgets ( char *obuf, size_t bufsize, FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	char *op, *maxop;
	int ch;

	maxop = obuf + bufsize -1;
	for ( op=obuf ; op < maxop ; ) {
	    ch = k_getc(cx);
	    if ( ch < 0 ) break;
	    else {
		*op = ch;
		op++;
		if (ch == '\n') break;
	    }
	}
	if ( op <= maxop ) *op = '\0';

	if ( op == obuf ) return (NULL);
	else return (obuf);
}

int k_fseek ( FILE *cx_i, long offset, int type )
{
	struct context *cx = (struct context *)cx_i;
	FILE *fp = cx->fp;
	int ch;

	if (debug > 1)
	    printf ("seek (%s, %ld, %d)\n", cx->fname, offset, type);
	    
	if (type == 0) {
	    fseek (fp, 0L, 0);
	    cx->fpos = 0;

	    while (cx->fpos < offset && (ch = getc(fp)) != EOF) {
		if (debug > 1)
		    fputc (ch, stdout);
		cx->fpos++;
	    }

	    if (debug > 1)
		printf ("[]\n");

	    return (0);
	}

	if (fseek (fp, offset, type) == -1)
	    return (-1);
	else {
	    cx->fpos = ftell (fp);
	    return (0);
	}
}

long k_ftell ( FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;

	if (debug > 1) {
	    printf ("ftell returns %ld\n", cx->fpos);
	    fflush (stdout);
	}

	return (cx->fpos);
}

#else

int k_getc ( FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	return (getc (cx->fp));
}

char *k_fgets ( char *op, size_t bufsize, FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	return (fgets (op, bufsize, cx->fp));
}

int k_fseek ( FILE *cx_i, long offset, int type )
{
	struct context *cx = (struct context *)cx_i;
	return (fseek (cx->fp, offset, type));
}

long k_ftell ( FILE *cx_i )
{
	struct context *cx = (struct context *)cx_i;
	return (ftell (cx->fp));
}

#endif
