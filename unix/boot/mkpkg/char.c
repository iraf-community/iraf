/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#define	import_spp
#define	import_error
#include <iraf.h>

#include "mkpkg.h"
#include "extern.h"

/*
 * CHAR.C -- Character functions, character i/o.
 */

/* M_GETC -- Get a (possibly pushed back) character from the mkpkgfile
 * associated with the given context.  If the sequence $( is encountered
 * in the input, fetch the value of the named macro and push it back into
 * the input stream and continue scanning.  Implementing recursive macro
 * expansion at this low level permits the use of macros in any part of
 * the input except comments.
 */
int
m_getc (register struct context *cx)
{
	register int	ch, nch;
	register char	*op;
	char	name[SZ_FNAME+1], *val;
	char	lbuf[SZ_CMD+1];

	while ((ch = m_rawgetc (cx)) == '$') {
	    /* Check for the escape sequence "$$" and return the literal $
	     * if this is seen.  Also return if $ is seen but the next char
	     * is not left paren ("$(..)" is a macro reference).
	     */
	    nch = m_rawgetc (cx);
	    if (nch == '$')
		return (nch);
	    else if (nch != '(') {
		m_ungetc (nch, cx);
		break;
	    }

	    /* Extract the name of the macro from the input stream.
	     */
	    for (op=name;  (*op = m_rawgetc(cx)) != ')';  op++)
		if (*op == '\n' || *op == EOF) {
		    *op = EOS;
		    warns ("missing right paren in $(M) macro reference: `%s'",
			name);
		    *op++ = '\n';
		    *op = EOS;
		    val = name;
		    goto push;
		    break;
		}
	    *op = EOS;

	    /* If the symbol name is prefixed by a question mark, e.g., $(?sym),
	     * query for the symbol and read the value from the standard input.
	     * If the syntax is "$(@file)" return the contents of the named
	     * file as the value of the macro reference.  Otherwise look in
	     * the symbol table and then in the environment for the named
	     * symbol.  If the symbol cannot be found in either place push
	     * its name and hope for the best.
	     */
	    if (name[0] == '?') {
		/* Interactive query. */
		if ((cx->fp == stdin)) {
		    warns ("`$(%s)': cannot query in -f stdin mode", name);
		    val = &name[1];
		} else {
		    printf ("%s: ", &name[1]);
		    fflush (stdout);
		    if (fgets (lbuf, SZ_CMD, stdin) == NULL)
			strcpy (lbuf, name);
		    if ((val = index (lbuf, '\n')))
			*val = EOS;
		    val = lbuf;
		}
	    } else if (name[0] == '@') {
		/* Return contents of a file. */
		FILE    *fp;
		int     ch, n;

		if ((fp = fopen (&name[1], "r")) == NULL) {
		    warns ("`$(%s)': cannot open file", name);
		    val = &name[1];
		} else {
		    for (n=SZ_CMD,op=lbuf; --n >= 0 && (ch=getc(fp)) != EOF; )
			*op++ = isspace(ch) ? ' ' : ch;
		    while (op > lbuf) {
			ch = *(op-1);
			if (isspace (ch))
			    --op;
			else
			    break;
		    }
		    *op = EOS;
		    val = lbuf;
		    fclose (fp);
		}

	    } else if ((val = getsym (name)) == NULL) {
		if ((val = os_getenv (name)) == NULL) {
		    warns ("macro `%s' not found", name);
		    val = name;
		}
	    }
push:
	    if (debug > 1) {
		printf ("pushback macro `%s' = `%s'\n", name, val);
		fflush (stdout);
	    }

	    m_pushstr (cx, val);
	}

	/* Get rid of the tabs once and for all.
	 */
	return ((ch == '\t') ? ' ' : ch);
}



/* M_RAWGETC -- Get a (possibly pushed back) character from the mkpkgfile
 * associated with the given context.
 */
int
m_rawgetc (register struct context *cx)
{
	register struct	pushback *pb;
	register int	ch;

	for (;;) {
	    /* Check for single character pushback first.  This type of pushback
	     * occurs at the end of every token.
	     */
	    if ((ch = cx->pbchar)) {
		if (debug > 3) {
		    if (ch <= 040)
			printf ("return pushback character 0%o\n", ch);
		    else
			printf ("return pushback character `%c'\n", ch);
		    fflush (stdout);
		}
		cx->pbchar = 0;
		break;
	    }

	    /* Check for string type pushback; return character directly from
	     * file if no pushback.
	     */
	    if (!cx->pushback) {
		ch = k_getc (cx);
		break;
	    }

	    /* Get pushed back character from pushback buffer.
	     */
	    pb = cx->pb;
	    if ((ch = *(pb->ip)++) != EOS) {
		if (debug > 3) {
		    if (ch <= 040)
			printf ("return pbbuf character 0%o\n", ch);
		    else
			printf ("return pbbuf character `%c'\n", ch);
		    fflush (stdout);
		}
		break;
	    }

	    /* End of pushed back string; pop pushback stack.
	     */
	    if (debug > 3) {
		printf ("pop pushback stack at level=%d\n", pb->npb);
		fflush (stdout);
	    }

	    pb->op = pb->pbstk[--(pb->npb)];
	    pb->ip = pb->pbstk[--(pb->npb)];

	    if (pb->npb <= 0)
		cx->pushback = 0;
	}

	if (ch == '\n')
	    cx->lineno++;

	return (ch);
}


/* M_UNGETC -- Pushback a single character, last in first out.  Only a single
 * character of this type of pushback is normally allowed, however by using
 * PUSHSTR we can provide additional pushback at additional expense (no
 * problem provided it is not used a lot).
 */ 
void
m_ungetc (
  int	ch,
  struct context *cx
)
{
	static	char ps[2] = "\0";

	if (ch == '\n')
	    --cx->lineno;
	    
	if ((ps[0] = cx->pbchar))
	    m_pushstr (cx, ps);

	cx->pbchar = ch;

	if (debug > 3) {
	    if (ch <= 040)
		printf ("ungetc 0%o\n", ch);
	    else
		printf ("ungetc `%c'\n", ch);
	    fflush (stdout);
	}
}


/* M_PUSHSTR -- Pushback a string.  Pushed strings are read back LIFO, although
 * of course the individual characters are returned FIFO.
 */ 
void
m_pushstr (
  struct context *cx,
  char	*str
)
{
	register struct	pushback *pb;
	register char	*ip, *op, *otop, ch;

	if (debug > 2) {
	    if (str[0] <= 040)
		printf ("pushback punct char 0x%lx\n", (long) str);
	    else
		printf ("pushback string `%s'\n", str);
	    fflush (stdout);
	}

	cx->pushback++;
	while ((pb = cx->pb) == NULL)
	    mk_pbbuf (cx);

	pb->pbstk[(pb->npb)++] = pb->ip;
	pb->pbstk[(pb->npb)++] = pb->op;
	otop = pb->otop;

	for (ip=str, op=pb->op;  (*op++ = ch = *ip++);  ) {
	    if (ch == '\n')
		--cx->lineno;
	    if (op >= otop)
		break;
	}

	pb->ip = pb->op;
	pb->op = op;

	if (debug > 2) {
	    printf ("pb status: ");
	    printf ("level=%d(%d) nleft=%ld ip=%ld op=%ld bp=%ld otop=%ld\n",
		pb->npb, SZ_PBSTK, 
		(long) (otop-op), 
		(long) pb->ip, 
		(long) pb->op, 
		(long) pb->pbbuf, 
		(long) otop);
	    fflush (stdout);
	}

	if (pb->npb + 2 >= SZ_PBSTK || pb->op >= pb->otop)
	    fatals ("excessive pushback in `%s'; macro recursion?",
		cx->mkpkgfile);
}


/* MK_PBBUF -- Allocate and initialize the pushback descriptor.
 */
void
mk_pbbuf (register struct context *cx)
{
	register struct	pushback *pb;

	pb = cx->pb = (struct pushback *) malloc (sizeof (struct pushback));
	if (pb == NULL)
	    fatals ("out of memory in `%s'", cx->mkpkgfile);

	pb->npb  = 0;
	pb->ip   = pb->pbbuf;
	pb->op   = pb->pbbuf;
	pb->otop = &pb->pbbuf[SZ_PBBUF];
}


/* PB_CANCEL -- Cancel any pushback.
 */
void
pb_cancel (register struct context *cx)
{
	register struct	pushback *pb;

	cx->pushback = 0;
	cx->pbchar   = 0;

	if ((pb = cx->pb) != NULL) {
	    pb->npb  = 0;
	    pb->ip   = pb->pbbuf;
	    pb->op   = pb->pbbuf;
	    pb->otop = &pb->pbbuf[SZ_PBBUF];
	}
}


/* PUTSTR -- Add a string to end of the string buffer.  It is a fatal error
 * if the string buffer overflows.
 */
char *
putstr (char *s)
{
	register char *ip, *op, *otop;
	char	*start;

	start = cp;
	otop  = ctop;

	for (ip=s, op=cp;  (*op++ = *ip++);  )
	    if (op >= otop)
		fatals ("string buffer overflow at `%s'", s);

	cp = op;

	if (debug > 2) {
	    printf ("putstr `%s': nleft=%ld\n", s, (long)(otop-op));
	    fflush (stdout);
	}

	return (start);
}


/*
 * OS Character I/O.  This set of routines are provided as a workaround in
 * the event that the host system cannot execute FTELL/FSEEK reliably (VMS/C
 * could not).  The idea here is to keep track of the character offset from
 * the beginning of the file.  K_FTELL returns the character offset.  K_FSEEK
 * rewinds the file and reads characters forward to the indicated offset.
 * K_GETC keeps a count of the file position.  (the k_ stands for kludge).
 */

int
k_getc (struct	context *cx)
{
	return (getc (cx->fp));
}

char *
k_fgets (
  char	*op,
  int	maxch,
  register struct context *cx
)
{
	return (fgets (op, maxch, cx->fp));
}

int
k_fseek (
  struct context *cx,
  long	offset,
  int	type 
)
{
	return (fseek (cx->fp, offset, type));
}

long
k_ftell (struct context *cx)
{
	return (ftell (cx->fp));
}
