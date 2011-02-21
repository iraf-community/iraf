/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
*/

#define	import_spp
#define	import_libc
#define	import_stdio
#define	import_ctype
#define	import_stdarg
#include <iraf.h>


/*
** SCANF -- Formatted input.  The syntax of the calls and of the format strings
** are UNIX standard, but the lexical forms of numbers recognized are IRAF
** standard.
*/

#define	SCAN_STRING	0
#define	SCAN_FILE	1
#define	SZ_NUMBUF	256		/* maximum numeric field len	*/
#define	SZ_UCC		128		/* maximum size user char class	*/
#define	HUGE		999
#define	ISHEX(c)	((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))

struct _format {
	int	f_type;			/* field type (doxscef[%)	*/
	int	f_skipfield;		/* skip next field scanned	*/
	int	f_width;		/* max chars in field		*/
	int	f_longword;		/* output longword		*/
	int	f_halfword;		/* output halfword		*/
	int	f_delimset;		/* ucc chars are delimiters	*/
	char	f_ucc[SZ_UCC+1];
};

/* Character input including pushback.  Character input may come either from
** a file or from a string, depending on the value of the "intype" flag.
** We cannot open the string as a file because file buffer characters are
** of type XCHAR.
*/
struct _input {
	int	i_type;			/* file input if !0, else str	*/
	int	i_nchars;		/* nchars read thus far		*/
	union {
	FILE	*fp;			/* file pointer if file		*/
	char	*ip;			/* char pointer if string	*/
	} u;
};

#define	input()\
    (in->i_nchars++, in->i_type ? (int)getc(in->u.fp) : (int)*in->u.ip++)
#define	unput(ch)\
    (in->i_nchars--, in->i_type ? ungetc((ch),in->u.fp) : (int)(*--(in->u.ip)))
#define	ateof()\
    (in->i_type ? feof(in->u.fp) : *(in->u.ip-1) == EOS)


static int   u_doscan (struct _input *in, char *format, va_list *argp);
static char *u_crackformat (char *format, struct _format *fmt);
static int   u_scannum (struct _input *in, va_list **argp, 
			struct	_format *fmt, int *eofflag);
static char *u_setucc (char *format, struct _format *fmt);
static int   u_scanstr (struct  _input *in, va_list **argp,
  			struct  _format *fmt, int *eofflag);



/* SCANF -- Scan the standard input.  The output arguments must be
** pointers.  The number of fields successfully converted is returned as
** the function value.  EOF is returned for a scan at EOF.
*/
int
scanf (char *format, ...)
{
	va_list	argp;
	struct _input in;
	int status;

	extern int u_doscan();


	va_start (argp, format);
	in.i_type = SCAN_FILE;
	in.i_nchars = 0;
	in.u.fp = stdin;

	status = u_doscan (&in, format, &argp);
	va_end (argp);
	return (status);
}


/* FSCANF -- Formatted scan from a file.
*/
int
fscanf (FILE *fp, char *format, ...)
{
	va_list	argp;
	int	status;
	struct	_input in;

	extern int u_doscan();


	va_start (argp, format);
	in.i_type = SCAN_FILE;
	in.i_nchars = 0;
	in.u.fp = fp;

	status = u_doscan (&in, format, &argp);

	va_end (argp);
	return (status);
}


/* SSCANF -- Formatted scan from a string.
*/
int
sscanf (char *str, char *format, ...)
{
	va_list	argp;
	struct _input in;
	int status;

	extern int u_doscan();


	va_start (argp, format);
	in.i_type = SCAN_STRING;
	in.i_nchars = 0;
	in.u.ip = str;

	status = u_doscan (&in, format, &argp);

	va_end (argp);
	return (status);
}


/* U_DOSCAN -- Step along the format string, processing each %[*][w][lh]C
** field specification and returning each argument using the pointer
** supplied in the argument list.  Ordinary characters appearing in the format
** string must match actual characters in the input stream.  Input may be
** taken from either a string or a file.  The technique used to handle the
** variable number of arguments is machine dependent.
*/
static int
u_doscan (
  struct _input *in,			/* input descriptor		*/
  char	   *format,			/* format string		*/
  va_list  *argp			/* pointer to first argument	*/
)
{
	register int	ch;
	struct	_format fmt;
	int	nscan = 0, match;
	int	eofflag = 0;
	char	*u_crackformat(), *u_setucc();
	int	u_scanstr(), u_scannum();


	while ( (ch = *format++) ) {
	    if (ch == '%' && *format != '%') {
		/* Parse format specification.
		 */
		format = u_crackformat (format, &fmt);

		/* Extract, decode, and output the next field according to
		 * the field specification just generated.
		 */
		ch = *format++;
		if (ch == 'n') {
		    *(va_arg ((*argp), int *)) = in->i_nchars;
		    continue;
		} else if (ch == '[') {
		    format = u_setucc (format, &fmt);
		} else if (isupper (ch)) {
		    fmt.f_longword++;
		    ch = tolower (ch);
		}
		if (ch <= 0)
		    return (EOF);
		fmt.f_type = ch;

		if (ch == 's' || ch == 'c' || ch == '[')
		    match = u_scanstr (in, &argp, &fmt, &eofflag);
		else
		    match = u_scannum (in, &argp, &fmt, &eofflag);

		if (match && !fmt.f_skipfield)
		    nscan++;
		if (eofflag)
		    break;

	    } else if (isspace (ch)) {
		/* Skip optional whitespace. */
		while (isspace (ch = input()))
		    ;
		if (ateof()) {
		    eofflag++;
		    break;
		}
		unput (ch);

	    } else {
		/* Match normal character. */
		if (ch == '%')
		    format++;
		match = ch;
		if (match != (ch = input())) {
		    if (ateof())
			eofflag++;
		    else
			unput (ch);
		    break;
		}
	    }
	}

	if (eofflag)
	    return (nscan ? nscan : EOF);
	else
	    return (nscan);
}



/* U_CRACKFORMAT -- Decode a %[*][w][lh]C input field format specification,
 * returning the decoded format parameters in the output structure fmt.
 * The number of format characters is returned as the function value.  The
 * format string pointer is left pointing at the C character.
 */
static char *
u_crackformat (
  char	*format,		/* pointer to %+1 in format string	*/
  struct _format *fmt		/* output format descriptor		*/
)
{
	register int	ch;
	register int	width = 0;


	fmt->f_skipfield = 0;
	fmt->f_longword  = 0;
	fmt->f_halfword  = 0;

	/* Skip output if "*" present. */
	ch = *format++;
	if (ch == '*') {
	    fmt->f_skipfield++;
	    ch = *format++;
	}

	/* Get max field width, if any. */
	while (isdigit (ch)) {
	    width = width * 10 + tointeg (ch);
	    ch = *format++;
	}
	fmt->f_width = (width == 0) ? HUGE : width;

	/* Datatype size modifier. */
	if (ch == 'l') {
	    fmt->f_longword++;
	    ch = *format++;
	} else if (ch == 'h') {
	    fmt->f_halfword++;
	    ch = *format++;
	}

	return (--format);
}


/* U_SCANNUM -- Extract a numeric token from the input stream.  The lexical
** range of numbers recognized is as follows (ignoring leading +-):
**
**	INDEF						all types
**	[0-7]+						o
**	[0-9]+						d
**	[0-9][0-9a-fA-F]*				x
**	'.'[0-9]+([eEdD]([+-])?[0-9]+)?			f,e
**	[0-9][0-9:]'.'[0-9]+([eEdD]([+-])?[0-9]+)?	f,e (sexagesimal)
**
** If the conversion fails the token is pushed back into the input, ready
** to be rescanned.  Argp is bumped if skipfield is false whether or not
** a legal number is matched (else one might assign a string to a pointer
** to int in the next format, overruning memory).  If the match fails 0 is
** output to argp and 0 is returned as the function value, indicating no
** match.
*/
static int
u_scannum (
  struct _input *in,		/* input descriptor			*/
  va_list	**argp,		/* where output goes			*/
  struct	_format *fmt,	/* format descriptor			*/
  int	*eofflag		/* set to 1 on output if end of input	*/
)
{
	char	numbuf[SZ_NUMBUF+1];
	register int	ch;
	register long	num = 0;
	register char	*op = numbuf;
	int	floating = 0, radix=10, n;
	int	neg=0, ndigits=0, match=1;
	int	dotseen=0, expseen=0;


	ch = fmt->f_type;
	n  = fmt->f_width;

	if (ch == 'd')
	    radix = 10;
	else if (ch == 'o')
	    radix = 8;
	else if (ch == 'x')
	    radix = 16;
	else if ( (floating = (ch == 'f' || ch == 'e' || ch == 'g') )) {
	    radix = 10;
	    dotseen = expseen = 0;
	}

	while (isspace ( (ch = input()) ))
	    ;
	
	if (ch == '-' || ch == '+') {
	    if (ch == '-') {
		neg++;
		*op++ = ch;
	    }
	    ch = input();
	    --n;
	}

	/* Check for INDEF.  Abbreviations are not permitted; if the match
	 * fails the input must be restored.
	 */
	if (ch == 'I' && op == numbuf && n >= 5) {
	    char *ip = "INDEF";

	    for (n=5;  --n >= 0 && (ch == *ip++);  ch=input())
		*op++ = ch;
	    *op = EOS;
	    if (!ateof())
		unput (ch);

	    if (n < 0) {
		/* If the 6th char is not a legal character in an identifier
		 * then we have a match.
		 */
		if (! (isalnum (ch) || ch == '_')) {
		    if (fmt->f_skipfield)
			return (1);
		    else if (floating)
			goto out_;
		    else {
			if (fmt->f_longword)
			    *(va_arg ((**argp), long  *)) = INDEFL;
			else if (fmt->f_halfword)
			    *(va_arg ((**argp), short *)) = INDEFS;
			else
			    *(va_arg ((**argp), int   *)) = INDEFI;
			return (1);
		    }
		}
	    }

	    /* No match; neither INDEF nor a number.  Push back chars and exit.
	     * (the FIO ungetc may be called repeatedly).
	     */
	    for (--op;  op >= numbuf;  --op)
		unput (*op);

	    match = 0;
	    num = 0;
	    numbuf[0] = '0';
	    goto out_;
	}

	/* Extract token into numbuf.  If the token contains only digits it
	 * will have been converted to binary and left in the variable num.
	 */
	for (*op++ = ch;  --n >= 0;  *op++ = ch = input()) {
	    if (isdigit (ch)) {
		ch = tointeg (ch);
		if (ch >= radix)
		    break;
		ndigits++;
		num = num * radix + ch;

	    } else if (radix == 16 && ISHEX(ch)) {
		if (isupper (ch))
		    ch = tolower (ch);
		ndigits++;
		num = num * 16 + ch - 'a' + 10;

	    } else if (ch == '.') {
		if (!floating || dotseen)
		    break;
		dotseen++;
	    } else if (ch == ':') {
		if (!floating || ndigits == 0 || dotseen || expseen)
		    break;

	    } else if (ch == 'e' || ch == 'E' || ch == 'd' || ch == 'D') {
		if (!floating || expseen || (ndigits == 0 && !dotseen))
		    break;
		expseen++;
		*op++ = ch = input();
		if (! (ch == '+' || ch == '-' || isdigit(ch)))
		    break;

	    } else
		break;
	}

	*--op = EOS;
	if (ateof())
	    *eofflag = 1;
	else
	    unput (ch);

	if (ndigits == 0)
	    match = 0;				/* not a number		*/
	if (neg)
	    num = -num;
out_:
	if (fmt->f_skipfield)
	    return (match);

	/* Output value.
	 */
	if (floating) {
	    float	rval;
	    double	dval, atof();

	    if (fmt->f_longword) {
		*(va_arg ((**argp), double *)) = atof (numbuf);
	    } else if (fmt->f_halfword) {
		dval = atof (numbuf);
		rval = (dval == INDEFD) ? INDEFR : dval;
		*(va_arg ((**argp), float  *)) = rval;
	    } else
		*(va_arg ((**argp), double *)) = atof (numbuf);
	} else {
	    if (fmt->f_longword)
		*(va_arg ((**argp), long  *)) = num;
	    else if (fmt->f_halfword)
		*(va_arg ((**argp), short *)) = num;
	    else
		*(va_arg ((**argp), int   *)) = num;
	}

	return (match);
}


/* U_SETUCC -- Extract a user defined character class from the format string.
** A full 128 char table is not used since it is more expensive to prepare
** than it is worth for small sets.
*/
static char *
u_setucc (
  char	*format,
  struct _format *fmt
)
{
	register char	*op = fmt->f_ucc;
	register int	n = SZ_UCC;

	fmt->f_delimset = (*format == '^') ? *format++ : 0;
	while (--n && *format && (*op = *format++) != ']') {
	    if (*op == '\\' && *format == ']')
		*op = *format++;
	    op++;
	}
	*op = EOS;

	return (format);
}



/* U_SCANSTR -- Extract a whitespace delimited sequence of characters.
*/
static int
u_scanstr (
  struct  _input *in,		/* input descriptor			*/
  va_list **argp,		/* output arglist			*/
  struct  _format *fmt,		/* numeric format expected		*/
  int	  *eofflag		/* set to 1 on output if end of input	*/
)
{
	register int	ch, n;
	register char	*ip, *op;
	int	delimset;
	char	*ucc = "";


	op = fmt->f_skipfield ? (char *)NULL : va_arg ((**argp), char *);
	ch = fmt->f_type;
	n  = fmt->f_width;
	if (ch == 'c' && n == HUGE)
	    n = 1;

	/* Set up character class to be matched.
	 */
	delimset = 1;
	if (ch == 'c')
	    ucc = "";
	else if (ch == 's')
	    ucc = " \t\n";
	else if (ch == '[') {
	    ucc = fmt->f_ucc;
	    delimset = fmt->f_delimset;
	}
 
	/* Skip leading whitespace only for format %s.
	 */
	if (ch == 's') {
	    while (isspace (ch = input()))
		;
	} else
	    ch = input();

	/* Extract chars matching set into output buffer.
	 */
	while (--n >= 0 && (ch > 0 || !ateof())) {
	    /* Is char in set ucc. */
	    for (ip=ucc;  *ip != EOS;  ip++)
		if (ch == *ip) {
		    if (delimset)
			goto done_;
		    if (op)
			*op++ = ch;
		    goto next_;
		}

	    /* Char not in set if we get here. */
	    if (delimset) {
		if (op)
		    *op++ = ch;
	    } else
		break;
next_:
	    ch = input();
	}
done_:
	if (ateof())
	    *eofflag = 1;
	else {
	    unput (ch);
	}

	if (op && fmt->f_type != 'c')
	    *op = EOS;
	
	return (1);
}
