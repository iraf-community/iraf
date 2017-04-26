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

FILE *
k_fopen (fname, mode)
char	*fname;
char	*mode;
{
	register struct context *cx;
	register FILE	*fp;

	if ((fp = fopen (fname, mode)) == NULL)
	    return (NULL);

	cx = (struct context *) malloc (sizeof(struct context));
	strcpy (cx->fname, fname);
	cx->fpos = 0;
	cx->fp = fp;

	return ((FILE *)cx);
}


int
k_fclose (cx_i)
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	int	status;

	status = fclose (cx->fp);
	free (cx);

	return (status);
}

#ifdef vms

int
k_getc (cx_i)
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	register int	ch;

	cx->fpos++;
	if (debug > 3) {
	    if ((ch = getc (cx->fp)) > 0)
		printf ("%5d %03o %c\n", cx->fpos, ch, ch > 040 ? ch : 040);
	    return (ch);
	} else
	    return (getc (cx->fp));
}

char *
k_fgets (obuf, maxch, cx_i)
char	*obuf;
int	maxch;
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	register int	ch, n;
	register char	*op;

	for (op=obuf, n=maxch;  --n >= 0;  )
	    if ((ch = k_getc(cx)) < 0)
		return (NULL);
	    else {
		*op++ = ch;
		if (ch == '\n')
		    break;
	    }

	return (obuf);
}

seek
k_fseek (cx_i, offset, type)
FILE	*cx_i;
long	offset;
int	type;
{
	register struct context *cx = (struct context *)cx_i;
	register FILE	*fp = cx->fp;
	register int	ch;

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

long
k_ftell (cx_i)
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;

	if (debug > 1) {
	    printf ("ftell returns %d\n", cx->fpos);
	    fflush (stdout);
	}

	return (cx->fpos);
}

#else

int
k_getc (cx_i)
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	return (getc (cx->fp));
}

char *
k_fgets (op, maxch, cx_i)
char	*op;
int	maxch;
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	return (fgets (op, maxch, cx->fp));
}

int
k_fseek (cx_i, offset, type)
FILE	*cx_i;
long	offset;
int	type;
{
	register struct context *cx = (struct context *)cx_i;
	return (fseek (cx->fp, offset, type));
}

int
k_ftell (cx_i)
FILE	*cx_i;
{
	register struct context *cx = (struct context *)cx_i;
	return (ftell (cx->fp));
}

#endif
