/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>

#define import_spp
#include <iraf.h>

#include "xpp.h"


#define SZ_SBUF		4096		/* max chars in proc. decls.	*/
#define SZ_TOKEN	63		/* max chars in a token		*/
#define MAX_SYMBOLS	300		/* max symbol table entries	*/
#define UNDECL		0

/*
 * DECL.C -- A package of routines for parsing argument lists and declarations
 * and generating the Fortran (actually, RPP) declarations required to compile
 * a procedure.  The main functions of this package at present are to remove
 * arbitrary limitations on the ordering of argument declarations imposed by
 * Fortran, and to perform various compile time checks on all declarations.
 * Specifically, we allow scalar arguments to be used to dimension array
 * arguments before the scalar arguments are declared, and we check for
 * multiple declarations of the same object.
 *
 * Package Externals:
 *
 *	    d_newproc (name, type)	process procedure declaration
 *	d_declaration (typestr)		process typed declaration statement
 *	    d_codegen (fp)		output declarations for sym table
 *	    d_runtime (text,bufsize)	return any runtime initialization text
 *
 *	*symbol =  d_enter (symbol, dtype, flags)
 *	*symbol = d_lookup (symbol)
 *
 * The external procedures YY_INPUT() and YY_UNPUT() are called to get/putpack
 * characters from the input.
 */

struct symbol {
	char	*s_name;		/* symbol name			*/
	char	*s_dimstr;		/* dimension string if array	*/
	short	s_dtype;		/* datatype (0 until declared)	*/
	short	s_flags;		/* type flags			*/
};

#define S_ARGUMENT	001		/* symbol is an argument	*/
#define S_ARRAY		002		/* symbol is an array		*/
#define S_FUNCTION	004		/* symbol is a function()	*/
#define S_EXTERN	010		/* symbol is an external	*/

static char sbuf[SZ_SBUF+1];		/* string buffer		*/
static char *nextch = sbuf;		/* next location in sbuf	*/
static char *maxnextch = sbuf + SZ_SBUF+1 -1;
static char procname[SZ_FNAME+1];	/* procedure name		*/
static int  proctype;			/* procedure type if function	*/
static struct symbol sym[MAX_SYMBOLS];	/* symbol table			*/
static int nsym = 0;			/* number of symbols		*/

static void d_makedecl ( struct symbol *, FILE * );
static void d_declfunc ( struct symbol *, FILE * );
static struct symbol *d_enter ( const char *, int, int );
static struct symbol *d_lookup ( const char * );

/* D_NEWPROC -- Process a procedure declaration.  The name of the procedure
 * is passed as the single argument.  The input stream is left positioned
 * with the ( of the argument list as the next token (if present).  INPUT is
 * called repeatedly to read the remainder of the declaration, which may span
 * several lines.  The symbol table is cleared whenever a new procedure
 * declaration is started.
 */
/* name  : procedure name		*/
/* dtype : procedure type (0 if subr)	*/
void d_newproc ( const char *name, int dtype )
{
	int	token;
	char	tokstr[SZ_TOKEN+1];

	/* Print procedure name to keep the user amused in case the file
	 * is large and the machine slow.
	 */
	fprintf (stderr, "   %s:\n", name);
	fflush (stderr);

	snprintf (procname, SZ_FNAME+1, "%s", name);
	proctype = dtype;
	nextch = sbuf;
	maxnextch = sbuf + SZ_SBUF+1 -1;
	nsym = 0;

	/* Check for null argument list. */
	if (d_gettok(tokstr,SZ_TOKEN+1) != '(')
	    return;

	/* Process the argument list.
	 */
	while ((token = d_gettok(tokstr,SZ_TOKEN+1)) != ')') {
	    if (isalpha(token)) {
		/* Enter argument name into the symbol table.
		 */
		if (d_lookup (tokstr) != NULL) {
		    char lbuf[200];
		    snprintf (lbuf, 200, "%s.%s multiply declared",
			procname, tokstr);
		    xpp_warn (lbuf);
		} else
		    d_enter (tokstr, UNDECL, S_ARGUMENT);
	    } else if (token == '\n') {
		linenum[istkptr]++;
		continue;
	    } else if (token == ',') {
		continue;
	    } else
		error (XPP_SYNTAX, "bad syntax in procedure argument list");
	}
}


/* D_DECLARATION -- Process a declaration statement.  This is any statement
 * of the form
 *
 *	type	obj1, obj2, ..., objn
 *
 * ignoring comments and newlines following commas.  The recognized types are
 *
 *	bool, char, short, int, long, real, double, complex, pointer, extern
 *
 * If "obj" is followed by "()" the function type bit is set.  If followed
 * by "[...]" the array bit is set and the dimension string is accumulated,
 * converting [] into (), adding 1 for char arrays, etc. in the process.
 * Each OBJ identifier is entered into the symbol table with its attributes.
 */
/* dtype : data type			*/
void d_declaration ( int dtype )
{
	struct symbol *sp = NULL;
	char tokstr[SZ_TOKEN+1];
	char ch;
	int token, ndim;

	while ((token = d_gettok(tokstr,SZ_TOKEN+1)) != '\n') {
	    if (isalpha(token)) {

#ifdef CYGWIN
	        {   if (strncmp ("procedure", tokstr, 9) == 0) { 
/*
			extern char *yytext;
			pushcontext (PROCSTMT);
			d_gettok (yytext, SZ_TOKEN+1-1);
			d_newproc (yytext, dtype);
*/
			pushcontext (PROCSTMT);
			d_gettok (tokstr, SZ_TOKEN+1-1);
			d_newproc (tokstr, dtype);
			return (1);
		    }
	        }
#endif

		/* Enter argument or variable name into the symbol table.
		 * If symbol is already in table it must be an argument
		 * or we have a multiple declaration.
		 */
		if ((sp = d_lookup (tokstr)) != NULL) {
		    if (dtype == XTY_EXTERN)
			sp->s_flags |= S_EXTERN;
		    else if (sp->s_flags & S_ARGUMENT && sp->s_dtype == UNDECL)
			sp->s_dtype = dtype;
		    else {
			char lbuf[200];
			snprintf (lbuf, 200, "%s.%s multiply declared",
			    procname, tokstr);
			xpp_warn (lbuf);
		    }
		} else
		    sp = d_enter (tokstr, dtype, 0);

		/* Check for trailing () or [].
		 */
		token = d_gettok (tokstr, SZ_TOKEN+1);

		switch (token) {
		case ',':
		case '\n':
		    yy_unput (token);
		    continue;

		case '(':
		    /* Function declaration.
		     */
		    if ((token = d_gettok(tokstr,SZ_TOKEN+1)) != ')') {
			yy_unput (token);
			error (XPP_SYNTAX,
			    "missing right paren in function declaration");
		    } 
		    sp->s_flags |= S_FUNCTION;
		    continue;

		case '[':
		    /* Array declaration.  Turn [] into (), add space for EOS
		     * if char array, set array bit for operand in symbol table.
		     */
		    sp->s_dimstr = nextch;
		    if ( nextch < maxnextch ) *nextch++ = '(';
		    ndim = 1;

		    while ((ch = yy_input()) != ']' && ch > 0) {
			if (ch == '\n') {
			    yy_unput (ch);
			    error (XPP_SYNTAX,
				"missing right bracket in array declaration");
			    break;
			} else if (ch == ',') {
			    /* Add one char for the EOS in the first axis of
			     * a multidimensional char array.
			     */
			    if (ndim == 1 && dtype == TY_CHAR) {
				if ( nextch < maxnextch ) *nextch++ = '+';
				if ( nextch < maxnextch ) *nextch++ = '1';
			    }
			    if ( nextch < maxnextch ) *nextch++ = ',';
			    ndim++;
			} else if (ch == 'A') {
			    /* Turn [ARB] into [*] for array arguments. */
			    if ((ch = yy_input()) == 'R') {
				if ((ch = yy_input()) == 'B') {
				    if ( nextch < maxnextch ) *nextch++ = '*';
				    ndim++;
				    if (!(sp->s_flags & S_ARGUMENT)) {
					error (XPP_SYNTAX,
					    "local variable dimensioned ARB");
					break;
				    }
				} else {
				    if ( nextch < maxnextch ) *nextch++ = 'A';
				    if ( nextch < maxnextch ) *nextch++ = 'R';
				    yy_unput (ch);
				}
			    } else {
				if ( nextch < maxnextch ) *nextch++ = 'A';
				yy_unput (ch);
			    }
			} else {
			    if ( nextch < maxnextch ) *nextch++ = ch;
			}
		    }

		    if (ndim == 1 && dtype == TY_CHAR) {
			if ( nextch < maxnextch ) *nextch++ = '+';
			if ( nextch < maxnextch ) *nextch++ = '1';
		    }

		    if ( nextch < maxnextch ) *nextch++ = ')';
		    else
			error (XPP_COMPERR, "decl string buffer overflow");

		    *nextch = '\0';
		    if ( nextch < maxnextch ) nextch++;

		    sp->s_flags |= S_ARRAY;
		    break;

		default:
		    error (XPP_SYNTAX, "declaration syntax error");
		}

	    } else if (token == ',') {
		/* Check for implied continuation on the next line.
		 */
		do {
		    ch = yy_input();
		} while (ch == ' ' || ch == '\t');

		if (ch == '\n')
		    linenum[istkptr]++;
		else
		    yy_unput (ch);

	    } else if (sp && (sp->s_flags & S_ARGUMENT)) {
		error (XPP_SYNTAX, "bad syntax in procedure argument list");
	    } else
		error (XPP_SYNTAX, "declaration syntax error");
	}

	yy_unput ('\n');
}


/* D_CODEGEN -- Output the RPP declarations for all symbol table entries.
 * Declare scalar arguments first, followed by array arguments, followed
 * by nonarguments.
 */
void d_codegen ( FILE *fp )
{
	struct symbol *sp;
	struct symbol *top = &sym[nsym-1];
	int col;

	/* Declare the procedure itself.
	 */
	if (proctype) {
	    fputs (type_decl[proctype], fp);
	    fputs (" x$func ", fp);
	} else
	    fputs ("x$subr ", fp);

	fputs (procname, fp);
	fputs (" ", fp);

	/* Output the argument list.  Keep track of the approximate line length
	 * and break line if it gets too long for the second pass.
	 */
	fputs ("(", fp);
	col = strlen(procname) + 9;

	for (sp=sym;  sp <= top;  sp++)
	    if (sp->s_flags & S_ARGUMENT) {
		if (sp > sym) {
		    fputs (", ", fp);
		    col += 2;
		}
		col += strlen (sp->s_name);
		if (col >= 78) {
		    fputs ("\n\t", fp);
		    col = strlen (sp->s_name) + 1;
		}
		fputs (sp->s_name, fp);
	    }
	fputs (")\n", fp);

	/* Declare scalar arguments. */
	for (sp=sym;  sp <= top;  sp++)
	    if (sp->s_flags & S_ARGUMENT)
		if (!(sp->s_flags & S_ARRAY))
		    d_makedecl (sp, fp);

	/* Declare vector arguments. */
	for (sp=sym;  sp <= top;  sp++)
	    if (sp->s_flags & S_ARGUMENT)
		if (sp->s_flags & S_ARRAY)
		    d_makedecl (sp, fp);

	/* Declare local variables and externals. */
	for (sp=sym;  sp <= top;  sp++)
	    if (sp->s_flags & S_ARGUMENT)
	        continue;
	    else if (sp->s_flags & S_FUNCTION)
	        d_declfunc (sp, fp);
	    else
		d_makedecl (sp, fp);
}


/* D_RUNTIME -- Return any runtime procedure initialization statements,
 * i.e., statements to be executed at runtime when a procedure is entered,
 * in the given output buffer.
 */
void d_runtime ( char *text, size_t bufsize )
{
	/* For certain types of functions, ensure that the function value
	 * is initialized to a legal value, in case the procedure is exited
	 * without returning a value (e.g., during error processing).
	 */
	switch (proctype) {
	case XTY_REAL:
	case XTY_DOUBLE:
	    snprintf (text, bufsize, "\t%s = 0\n", procname);
	    break;
	default:
	    if ( 0 < bufsize ) text[0] = EOS;
	    break;
	}
}


/* D_MAKEDECL -- Output a single RPP symbol declaration.  Each declaration
 * is output on a separate line.
 */
/* sp : symbol table entry		*/
/* fp : output file			*/
static void d_makedecl ( struct symbol *sp, FILE *fp )
{
	if (sp->s_dtype != UNDECL) {
	    fputs (type_decl[sp->s_dtype], fp);
	    fputs ("\t", fp);
	    fputs (sp->s_name, fp);
	    if (sp->s_flags & S_ARRAY)
		fputs (sp->s_dimstr, fp);
	    fputs ("\n", fp);
	}

	if (sp->s_flags & S_EXTERN) {
	    fputs (type_decl[XTY_EXTERN], fp);
	    fputs ("\t", fp);
	    fputs (sp->s_name, fp);
	    fputs ("\n", fp);
	}
}


/* D_ENTER -- Add a symbol to the symbol table.  Return a pointer to the
 * new symbol.
 */
/* name  : symbol name			*/
/* dtype : data type code		*/
/* flags : flag bits			*/
static struct symbol *d_enter ( const char *name, int dtype, int flags )
{
	struct symbol *sp;

	sp = &sym[nsym];
	nsym++;
	if (nsym > MAX_SYMBOLS)
	    error (XPP_COMPERR, "too many declarations in procedure");

	sp->s_name = nextch;
	snprintf (nextch, SZ_SBUF+1 - (nextch-sbuf), "%s", name);
	if ( strlen(nextch) < strlen(name) )
	    error (XPP_COMPERR, "decl string buffer overflow");
	nextch += strlen(name);
	if ( nextch < maxnextch ) nextch++;

	sp->s_dimstr = NULL;
	sp->s_dtype = dtype;
	sp->s_flags = flags;

	return (sp);
}


/* D_LOOKUP -- Lookup a symbol in the symbol table.  Return a pointer to the
 * symbol table entry.
 */
/* name : symbol name			*/
static struct symbol *d_lookup ( const char *name )
{
	struct symbol *sp;
	struct symbol *top = &sym[nsym-1];

	for (sp=sym;  sp <= top;  sp++)
	    if (sp->s_name[0] == name[0])
		if (strcmp (sp->s_name, name) == 0)
		    return (sp);

	return (NULL);
}


/* D_GETTOK -- Get the next token from the input stream.  Return the integer
 * value of the first character of the token as the function value.  EOF
 * is an error in this application, not a token.
 */
/* tokstr : receives token string	*/
/* bufsiz : max chars to token string	*/
int d_gettok ( char *tokstr, size_t bufsize )
{
	char *op = tokstr;
	char *maxop = tokstr + bufsize -1;
	int ch;

	/* Skip whitespace and comments to first char of next token.
	 */
	do {
	    ch = yy_input();
	} while (ch == ' ' || ch == '\t');

	if (ch == '#') {
	    /* Skip a comment.
	     */
	    while ((ch = yy_input()) != '\n' && ch > 0)
		;
	}

	if (ch <= 0)
	    error (XPP_SYNTAX, "unexpected EOF");

	if ( op < maxop ) *op++ = ch;

	if (isalpha (ch)) {
	    /* Identifer.
	     */
	    while ((ch = yy_input()) > 0)
		if (isalnum(ch) || ch == '_') {
		    if ( op < maxop ) *op++ = ch;
		    else
			error (XPP_SYNTAX, "identifier too long");
		} else {
		    yy_unput (ch);
		    break;
		}

	} else if (isdigit (ch)) {
	    /* Number.
	     */
	    while ((ch = yy_input()) > 0)
		if (isdigit(ch)) {
		    if ( op < maxop ) *op++ = ch;
		    else
			error (XPP_SYNTAX, "number too long");
		} else {
		    yy_unput (ch);
		    break;
		}

	}

	if ( op <= maxop ) *op = '\0';
	if (ch <= 0)
	    error (XPP_SYNTAX, "unexpected EOF");

	return (tokstr[0]);
}


/* D_DECLFUNC -- Declare a function.  This module is provided to allow
 * for any special treatment required for certain types of function
 * declarations.
 */
static void d_declfunc ( struct symbol *sp, FILE  *fp )
{
	d_makedecl (sp, fp);
}
