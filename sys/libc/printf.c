/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#include <stdarg.h>

#define	import_spp
#define	import_libc
#define	import_xnames
#define	import_stdio
#include <iraf.h>

static void u_doarg(FILE *fp, XCHAR *formspec, int prec[], int varprec);

#define	todigit(c)	((c)+'0')


/* PRINTF -- Emulation of the UNIX printf facilities with the IRAF FMTIO
** interface as the backend.  All features of the UNIX printf are supported
** without modification.  Additional format codes are supported in conformance
** with the IRAF printf, e.g., hms format, variable radix, tabstops, etc.,
** but these are upward compatible with standard UNIX usage.
*/

#define	SZ_FMTSPEC	25		/* max size single format spec	*/
#define	SZ_OBUF		SZ_COMMAND	/* sz intermediate buffer	*/
#define	MAX_PREC	4		/* max "*" deferred args	*/
#define	NOARG		(-1)		/* % spec with no data value	*/


/* PRINTF -- Formatted print to the standard output.
*/
void
printf (char *format, ...)
{
        va_list argp;

	va_start (argp, format);
	vfprintf (stdout, format, argp);
	va_end (argp);
}


/* FPRINTF -- Formatted print to a file.
*/
void
fprintf (FILE *fp, char *format, ...)
{
        va_list argp;

	va_start (argp, format);
	vfprintf (fp, format, argp);
	va_end (argp);
}


/* VFPRINTF -- Process the format to the output file, taking arguments from
** the list pointed to by argp as % format specs are encountered in the input.
** The main point of this routine is to handle the variable number of arguments.
** The actual encoding is all handled by the IRAF FPRINF and PARG calls.
** N.B. we assume chars are stacked as ints, and floats are stacked as doubles.
*/
void
vfprintf (
  FILE	*fp,				/* output file			*/
  char *format,				/* "%w.dC" etc. format spec	*/
  va_list argp				/* pointer to first value arg	*/
)
{
	register int ch;		/* next format char reference	*/
	XCHAR	formspec[SZ_FMTSPEC];	/* copy of single format spec	*/
	XCHAR	*fsp;			/* pointer into formspec	*/
	int	done, dotseen;		/* one when at end of a format	*/
	int	varprec;		/* runtime precision is used	*/
	int	prec[MAX_PREC];		/* values of prec args		*/
	XINT	ival;
	XCHAR	sbuf[SZ_OBUF+1];
	XDOUBLE	dval;
	char	*cptr;


	while ( (ch = *format++) ) {
	    if (ch == '%') {
		fsp = formspec;
		*fsp++ = ch;
		varprec = 0;
		dotseen = 0;
		done = 0;

		while (!done) {
		    ch = *fsp++ = *format++;

		    switch (ch) {
		    case EOS:
			--format;
			done++;
			break;

		    case 'l':
			/* arg size modifier; ignored for now */
			fsp--;
			break;

		    case '*':
			prec[varprec++] = va_arg (argp, int);
			break;

		    case '.':
			dotseen++;
			break;

		    case 'r':			/* nonstandard UNIX	*/
			if ((ch = *fsp++ = *format++) == '*') {
			    int		radix;
			    int		radchar;

			    radix = va_arg (argp, int);
			    if (radix < 0)
				radchar = 'A';
			    else if (radix > 9)
				radchar = radix - 10 + 'A';
			    else
				radchar = todigit (radix);
			    *(fsp-1) = radchar;
			} else if (ch == EOS) {
			    --format;
			    break;
			}
			/* fall through */

		    case 'b':			/* nonstandard UNIX	*/
		    case 'c':
		    case 'd':
		    case 'o':
		    case 'x':
		    case 'u':
			*fsp = EOS;
			u_doarg (fp, formspec, prec, varprec);
			ival = va_arg (argp, int);
			PARGI (&ival);
			done++;
			break;

		    case 'E':			/* ANSI emulation	*/
			*(fsp-1) = 'e';
			goto rval;
		    case 'G':			/* ANSI emulation	*/
			*(fsp-1) = 'g';
			goto rval;

		    case 'z':			/* nonstandard UNIX	*/
		    case 'h':			/* nonstandard UNIX	*/
		    case 'H':			/* nonstandard UNIX	*/
		    case 'm':			/* nonstandard UNIX	*/
		    case 'M':			/* nonstandard UNIX	*/
		    case 'e':
		    case 'f':
		    case 'g':
			/* If no precision was specified, default to 14 digits
			 * for %[efgz] and 3 digits for %[hm].
			 */
rval:			if (!dotseen) {
			    *(fsp-1) = '.';
			    if (ch == 'h' || ch == 'm' ||
				ch == 'H' || ch == 'M') {
				*fsp++ = '3';
			    } else {
				*fsp++ = '1';
				*fsp++ = '4';
			    }
			    *fsp++ = ch;
			}

			*fsp = XEOS;
			u_doarg (fp, formspec, prec, varprec);
			dval = va_arg (argp, double);
			PARGD (&dval);
			done++;
			break;

		    case 's':
			*fsp = EOS;
			u_doarg (fp, formspec, prec, varprec);
			cptr = va_arg (argp, char *);
			PARGSTR (c_strupk (cptr, sbuf, SZ_OBUF));
			done++;
			break;

		    case 't':			/* nonstandard UNIX	*/
		    case 'w':			/* nonstandard UNIX	*/
			*fsp = EOS;
			u_doarg (fp, formspec, prec, varprec);
			done++;
			break;

		    case '%':
			putc (ch, fp);
			done++;
			break;
		    }
		}

	    } else 
		putc (ch, fp);
	}
}


/* U_DOARG -- Encode a single argument acording to the simplified format
** specification given by formspec.  This is the interface to the IRAF
** formatted output procedures.
*/
static void
u_doarg (
  FILE		*fp,		/* output file			*/
  XCHAR		*formspec,	/* format string                */
  int		prec[],		/* varprec args, if any		*/
  int		varprec		/* number of varprec args	*/
)
{
	register int	p;
	XINT	fd = fileno (fp);
	XINT	ival;

	/* Pass format string and any variable precision arguments.
	 */
	FPRINTF (&fd, formspec);
	for (p=0;  p < varprec;  p++) {
	    ival = prec[p];
	    PARGI (&ival);
	}

}
