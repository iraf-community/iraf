/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#define import_stdarg
#include <iraf.h>

#include "config.h"
#include "operand.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "proto.h"

extern void u_doprnt();


/*
 * CLPRINTF -- These are just printf's with various implied write files for
 * convenience.  Also here are other assorted printing utilities.
 */

/* EPRINTF -- Printf that always writes to the current pseudo-file t_stderr.
 */
void
eprintf (char *fmt, ...)
{
	va_list	args;
	FILE *eout;

	va_start (args, fmt);
	eout = currentask->t_stderr;
	u_doprnt (fmt, &args, eout);
	va_end (args);
	fflush (eout);
}


/* OPRINTF -- Printf that always writes to the current pseudo-file t_stdout.
 */
void
oprintf (char *fmt, ...)
{
	va_list	args;
	FILE *sout;

	va_start (args, fmt);
	sout = currentask->t_stdout;
	u_doprnt (fmt, &args, sout);
	va_end (args);
	fflush (sout);
}


/* TPRINTF -- Printf that always goes through the pipe out to the currently
 * running task.  Be a bit more careful here in case a pipe is broken or
 * something is going haywire.
 */
void
tprintf (char *fmt, ...)
{
	va_list	args;
	FILE *out;

	out = currentask->t_out;
	if (out == NULL)
	    cl_error (E_IERR, "no t_out for currentask `%s'",
	    currentask->t_ltp->lt_lname);
	else {
	    va_start (args, fmt);
	    u_doprnt (fmt, &args, out);
	    va_end (args);
	    fflush (out);
	    if (ferror (out))
		cl_error (E_UERR|E_P, "pipe write error to `%s'",
		    currentask->t_ltp->lt_lname);
	}
}


/* TWRITE -- Write a binary block of data to the current task.
 *
 * This function is currently not used by anyone.
void
twrite (
  char	*buf,
  int	nbytes
)
{
	FILE *out;

	out = currentask->t_out;
	if (out == NULL) {
	    cl_error (E_IERR, "no t_out for currentask `%s'",
		currentask->t_ltp->lt_lname);
	} else if (nbytes > 0) {
	    fwrite (buf, sizeof(*buf), nbytes, out);
	    fflush (out);
	    if (ferror (out))
		cl_error (E_UERR|E_P, "pipe write error to `%s'",
		    currentask->t_ltp->lt_lname);
	}
}
*/


/* PRPARAMVAL -- Print the value field of param pp on file fp.
 * Give name of file if list, don't do anything if undefinded. 
 * Do not include a trailing \n.
 */
void
prparamval (
  struct param *pp,
  FILE	*fp
)
{
	char	buf[SZ_LINE];

	spparval (buf, pp);
	fputs (buf, fp);
}


/* STRSORT -- Sort a list of pointers to strings.
 */
void
strsort (
  char	*list[],		/* array of string pointers */
  int	nstr			/* number of strings */
)
{
	extern	int qstrcmp();

	qsort ((char *)list, nstr, sizeof(char *), qstrcmp);
}


/* QSTRCMP -- String comparison routine (strcmp interface) for STRSRT.
 */
int
qstrcmp (
  char	*a, 
  char  *b
)
{
	return (strcmp (*(char **)a, *(char **)b));
}


/* STRTABLE -- Given a list of pointers to strings as input, format and print
 * the strings in the form of a nice table on the named output file.  Adjust
 * the number of columns to fill the page (64 cols) as nearly as possible,
 * with at least two spaces between strings.  Excessively long strings
 * are truncated (adapted from "fmtio/strtbl.x").
 */
void
strtable (
  FILE	*fp,			/* output file */
  char	*list[],		/* array of string pointers */
  int	nstr,			/* number of strings */
  int	first_col,		/* where to place table on a line */ 
  int   last_col,
  int	maxch,			/* maximum chars to print from a string */
  int	ncol 			/* desired # of columns (0 to autoscale) */
)
{
	int	row, i, j, nspaces, len, maxlen, colwidth;
	int	numcol, numrow, str;
	char	*p;

	/* Find the maximum string length.  */
	maxlen = 0;
	for (i=1;  i <= nstr;  i++)
	    if ((len = strlen (list[i-1])) > maxlen)
		maxlen = len;

	/* Cannot be longer than "maxch" characters, if given.  */
	if (maxch > 0 && maxch < maxlen)
	    maxlen = maxch;

	/* Compute the optimum number of columns. */
	if ((numcol = (last_col - first_col + 1) / (maxlen + 2)) < 1)
	    numcol = 1;
	if (ncol > 0 && ncol < numcol)
	    numcol = ncol;
	colwidth = (last_col - first_col + 1) / numcol;
	numrow = (nstr + numcol-1) / numcol;

	/* For each row in the table:
	 */
	for (row=1;  row <= numrow;  row=row+1) {
	    for (i=1;  i < first_col;  i=i+1)	/* space to first col */
		putc (' ', fp);
	    /* For each string in the row:
	     */
	    for (i=1;  i <= numcol;  i=i+1) {
		str = row + (i-1) * numrow;
		if (str > nstr)
		    continue;
		p = list[str-1];		/* output string */
		for (j=0;  p[j] != '\0' && j < maxlen;  j=j+1)
		    putc (p[j], fp);
		if (i < numcol) {		/* advance to next col */
		    if ((nspaces = colwidth - j) < 2)
			nspaces = 2;
		    for (j=1;  j <= nspaces;  j=j+1)
			putc (' ', fp);
		}
	    }
	    putc ('\n', fp);			/* end of row of table */
	}
}
