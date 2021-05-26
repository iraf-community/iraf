/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_libc
#define import_stdio
#include <iraf.h>

#include "config.h"
#include "clmodes.h"
#include "operand.h"
#include "mem.h"
#include "grammar.h"
#include "opcodes.h"
#include "param.h"
#include "task.h"
#include "errs.h"
#include "construct.h"
#include "ytab.h"		/* pick up yacc token #defines		*/
#include "proto.h"


/*
 * GRAM -- These routines are used by the parser and lex files grammar.y and
 *   grammar.l.  Also we handle other things that are very visible to the user
 *   here too, such as cracking and running the intrinsic functions.
 *
 * We define our own yywrap() here to set yeof.
 * TODO: clean up having to keep some of the strings upper, some lower case.
 */

#define SZ_PIPEFILENAME		(6+4+5)		/* uparm$ // pipe // XXXXX */
#define	MAX_PIPECODE		30000		/* modulus for pipecodes   */

int	yeof;			/* set by yywrap when it sees eof.	*/
extern	int yylval;		/* declared in y.tab.c			*/
extern	int cldebug;

extern	int inarglist;		/* set by parser when in argument list	*/
extern	int parenlevel;		/* nesting level of parens		*/
extern	int get_nscanval();
int	pipetable[MAXPIPES];	/* for maintaining pipe temp files	*/
int	nextpipe = 0;

char	*truestr    = "yes";	/* true constant as it appears in ASCII	*/
char	*falsestr   = "no";	/* false                "		*/
char	*nullstr    = "";
char	*undefval   = "";	/* used in nextfield(), pvaldefined()	*/
char	*indefstr   = "INDEF";	/* input or output for indef operands	*/
char	*indeflc    = "indef";	/*   lower case version.		*/
char	*eofstr     = "EOF";	/* list file EOF or input		*/
char	*eoflc      = "eof";	/*   lower case version			*/
char	*epsilonstr="epsilon";	/* a small value; see config.h		*/
char	*errorstr   = "error";	/* the error statement			*/
char	*err_cmdblk;		/* Pointer where error detected		*/
extern  char  cmdblk[SZ_CMDBLK+1]; /* current command block (in history.c) */


/* Usually the following routine is provided by the yacc library but we need
 * our own here to signal the parser that an eof has been read.
 */
int
yywrap (void)
{
	yeof = 1;
	return (1);
}

/* Yacc calls this when it gets a general error.  We are doing all our own
 * error handling so just provide an entry point and store where the
 * error occurred in the input stream.
 */
/* ARGSUSED */
void
yyerror (char *s)
{
	extern	char	*ip_cmdblk;

	if (cldebug)
	   eprintf ("yyerror: %s, ip_cmdblk=%d %s\n", s, ip_cmdblk, ip_cmdblk);
	err_cmdblk = ip_cmdblk;	
}


/* Used by the . command: repeat whatever was last compiled.
 * All we need to do is advance the pc up to what it would be if the
 * command were typed in again.  See grammar.y '.' rule.
 */
void
rerun (void)
{
	register struct codeentry *cp;

	do {
	    cp = coderef (pc);
	    pc += cp->c_length;
	} while (cp->c_opcode != END);
}


/* CRACKIDENT -- Check given string s against keyword, set yylval, and return
 *   token.  Used from grammar when see an identifier or from "?" and "??" lex
 *   rules.  Make these look like identifiers for the special help commands.
 * A few that need more complicated processing are checked separately.
 * This is much more core efficient than putting the keywords in the
 *   lex spec and also makes the grammer very stable.
 * TODO: sort keyword list and do binary search if things get slow.
 *   (better yet use a hashed symbol table - this list is getting huge)
 */
int
crackident (char *s)
{
	struct keywords {
		char *k_name;		/* the keyword string itself.	*/
		short k_token;		/* yacc %token for the keyword	*/
		short k_yylval;		/* the value associated with token.*/
	};

	static struct keywords kw[] = {

	    /* Control flow keywords.
	     */
	    { "while",  Y_WHILE,  0 },	{ "if",        Y_IF,        0 },
	    { "else",   Y_ELSE,   0 },	{ "switch",    Y_SWITCH,    0 },
	    { "case",   Y_CASE,   0 },	{ "default",   Y_DEFAULT,   0 },
	    { "break",  Y_BREAK,  0 },	{ "next",      Y_NEXT,      0 }, 
	    { "return", Y_RETURN, 0 },	{ "goto",      Y_GOTO,      0 },
	    { "for",    Y_FOR,    0 },	{ "procedure", Y_PROCEDURE, 0 },
	    { "begin",  Y_BEGIN,  0 },	{ "end",       Y_END,       0 },

	    /* Parameter and variable types.
	     */
	    { "int",    Y_INT,    0 },	{ "char",      Y_STRING,    0 },
	    { "real",   Y_REAL,   0 },	{ "string",    Y_STRING,    0 },
	    { "file",   Y_FILE,   0 },	{ "gcur",      Y_GCUR,      0 },
	    { "imcur",  Y_IMCUR,  0 },	{ "ukey",      Y_UKEY,      0 },
	    { "pset",   Y_PSET,   0 },	{ "bool",      Y_BOOL,      0 },
	    { "struct", Y_STRUCT, 0 },

	    /* debugging commands.
	     */
	    { "d_d",    D_D,      0 },
	    { "d_peek", D_PEEK,   0 },

	    { "", 0, 0 } 		/* sentinel; leave it here... */
	};

	static struct keywords kf[] = {
	    /* Keywords of intrinsic functions that get built into
	     * the grammar.  Most intrinsics handled by intrinsic().
	     */
	    { "scan",   Y_SCAN,   0 },
	    { "scanf",  Y_SCANF,  0 },
	    { "fscan",  Y_FSCAN,  0 },
	    { "fscanf", Y_FSCANF, 0 },

	    /* sentinel; leave it here... */
	    { "", 0, 0 } 
	};

	register struct keywords *kp;
	XINT	oldtopd;
	static	char sch, kch;		/* static storage is faster here   */
	char	*scopy;			/* non-makelower'd copy		   */

	oldtopd = topd;			/* save topd			   */
	scopy = comdstr(s);		/* make a copy in the dictionary   */
	makelower (scopy);		/* make it lower case for compares */
	topd = oldtopd;			/*restore topd but scopy still there!*/

	/* Put the first character of the identifier we are searching for
	 * in local storage to permit fast rejection of keywords without all
	 * the overhead involved in a call to strcmp.  This is an easy way
	 * to speed things up several times w/o coding fancy data structures.
	 */
	sch = *scopy;

	/* Check for and handle special-case keywords first.
	 */
	if (sch == *truestr && !strcmp (scopy, truestr)) {
	    yylval = addconst (truestr, OT_BOOL);
	    return (Y_CONSTANT);
	} else if (sch == *falsestr && !strcmp (scopy, falsestr)) {
	    yylval = addconst (falsestr, OT_BOOL);
	    return (Y_CONSTANT);
	} else if (sch == *indeflc && !strcmp (scopy, indeflc)) {
	    yylval = addconst (scopy, OT_INT);
	    return (Y_CONSTANT);
	} else if (sch == *epsilonstr && !strcmp (scopy, epsilonstr)) {
	    char sb[REALWIDTH];
	    sprintf (sb, "%e", EPSILON);
	    yylval = addconst (sb, OT_REAL);
	    return (Y_CONSTANT);
	} else if (sch == *eoflc && !strcmp (scopy, eoflc)) {
	    yylval = addconst (CL_EOFSTR, OT_INT);
	    return (Y_CONSTANT);
	} else if (sch == *errorstr && !strcmp (scopy, errorstr)) {
	    yylval = addconst (errorstr, OT_STRING);
	    return (Y_IDENT);

	} else if (!inarglist && parenlevel == 0) {
	    /* Search the keyword list; kewords are not recognized in argument
	     * lists and expressions, else unquoted strings like "for" and
	     * "file" will cause syntax errors.
	     */
	    for (kp=kw;  (kch = *kp->k_name);  kp++)
		if (kch == sch)
		    if (strcmp (scopy, kp->k_name) == 0) {
			yylval = kp->k_yylval;
			return (kp->k_token);
		    }

	} else {
	    /* Search the list of intrinsic functions.
	     */
	    for (kp=kf;  (kch = *kp->k_name);  kp++)
		if (kch == sch)
		    if (strcmp (scopy, kp->k_name) == 0) {
			yylval = kp->k_yylval;
			return (kp->k_token);
		    }
	}

	/* S not a keyword, so it's just an identifier.
	 */
	yylval = addconst (s, OT_STRING);	/* use original */
	return (Y_IDENT);
}


/* ADDCONST -- Called during parsing to convert string s into operand of
 *   type t and push it as an operand onto the dictionary (NOT the operand
 *   stack).
 * Use dictionary because this routine is called at compile time and the 
 *   operand stack is being filled with compiled code; use dictionary as
 *   a quiet workspace.
 * Convert as per makeop().
 * Return dictionary index of new operand entry so that it may be used as
 *   ((struct operand *)&dictionary[$1])->o_... in yacc specs.
 */
XINT
addconst (char *s, int t)
{
	register struct operand *op;
	XINT	lasttopd;

	lasttopd = topd;		/* could just derefenece op	*/
	op = (struct operand *) memneed (OPSIZ);
	
	if (t == OT_STRING) {
	    /* makeop with an OT_STRING type will reuse the
	     * string pointer but we want to compile into the dictionary.
	     * fortunately, it's easy because lex has already removed any
	     * surrounding quotes.
	     */
	    op->o_type = t;
	    op->o_val.v_s = comdstr (s);
	} else
	    *op = makeop (s, t);

	return (lasttopd);
}


/* LISTPARAMS -- Go through the given pfile and list out its parameters on
 * t_stdout.  Give all non-hidden ones first, then all hidden ones in
 * parentheses.
 */
void
listparams (struct pfile *pfp)
{
	register struct param *pp;

	for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
	    if (!(pp->p_mode & M_HIDDEN))
		pretty_param (pp, currentask->t_stdout);

	for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
	    if (pp->p_mode & M_HIDDEN)
		pretty_param (pp, currentask->t_stdout);
}


/* PRETTY_PARAM -- Pretty print the name, value, and prompt string of
 * a parameter on the output file.  Put parens around the name=value string
 * if a hidden parameter.
 */
void
pretty_param (struct param *pp, FILE *fp)
{
	register char	ch, *p;
	char	buf[SZ_LINE];
	int	nchars, maxch;

	/* Get terminal dimensions from the environment.
	 */
	maxch = c_envgeti ("ttyncols") - 1;

	p = buf;					/* name =	*/
	if (pp->p_mode & M_HIDDEN)
	    *p++ = '(';
	sprintf (p, "%0.12s = ", pp->p_name);
	nchars = strlen (buf);
	while (nchars < 16) {
	    fputc (' ', fp);
	    nchars++;
	}
	fputs (buf, fp);

	/* For arrays print the index list. 
	 */
	if (pp->p_type & PT_ARRAY) {
	    int    dim, d, amin, amax;
	    short  *len, *off;
	    char   ibuf[15]; /* Maximum length of an index range should
			      * be 13 e.g. -DDDDD:-DDDDD, plus one for the
			      * terminator, and one for good luck.
			      */
	    buf[0]= '[';
	    buf[1] = '\0';

	    dim = pp->p_val.v_a->a_dim;
	    len = &(pp->p_val.v_a->a_len);
	    off = &(pp->p_val.v_a->a_off);

	    for (d=0; d<dim; d++) {
		amin = *(off + 2*d);
		amax = amin + *(len + 2*d) - 1;

		if (amin != 1)
		    sprintf (ibuf, "%d:%d", amin, amax);
		else
		    sprintf (ibuf, "%d", amax);
	
		strcat (buf, ibuf);
		if (d+1<dim)
		    strcat (buf, ",");

		if (strlen (buf) > SZ_LINE-14)
		    break;
	    }
	    strcat (buf, "]");
	    fputs (buf, fp);
	    nchars += strlen (buf);

	} else if (!(pp->p_valo.o_type & OT_UNDEF)) {
	    /* For scalars print the value if available.
	     */
	    sprop (buf, &pp->p_valo);
	    if ((pp->p_type & OT_BASIC) == OT_STRING && *buf != PF_INDIRECT) {
		fputc ('"', fp);
		nchars++;
	    }
	    fputs (buf, fp);
	    nchars += strlen (buf);
	    if ((pp->p_type & OT_BASIC) == OT_STRING && *buf != PF_INDIRECT) {
		fputc ('"', fp);
		nchars++;
	    }
	}

	if (pp->p_mode & M_HIDDEN) {
	    fputc (')', fp);
	    nchars++;
	}
	fputc (' ', fp);
	nchars++;

	/* Advance to next field. */
	while (nchars < 32) {
	    fputc (' ', fp);
	    nchars++;
	}
							/* prompt	*/
	for (p=pp->p_prompt;  (ch = *p++) != '\0' && nchars < maxch;  nchars++)
	    switch (ch) {
	    case '\t':
		fputs ("\\t", fp);
		nchars++;
		break;
	    case '\n':
		fputs ("\\n", fp);
		nchars++;
		break;
	    case '\r':
		fputs ("\\r", fp);
		nchars++;
		break;
	    case '\f':
		fputs ("\\f", fp);
		nchars++;
		break;
	    default:
		fputc (ch, fp);
	    }
	fputc ('\n', fp);
}


/* DUMPPARAMS -- Go through the given pfile and list out its parameters on
 * t_stdout in the form `task.param=value'.
 */
void
dumpparams (struct pfile *pfp)
{
	register struct param *pp;
	register FILE	*fp = currentask->t_stdout;

	for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
	    if (!(pp->p_mode & M_HIDDEN))
		show_param (pfp->pf_ltp, pp, fp);

	for (pp = pfp->pf_pp;  pp != NULL;  pp = pp->p_np)
	    if (pp->p_mode & M_HIDDEN)
		show_param (pfp->pf_ltp, pp, fp);
	
	fputs ("# EOF\n", fp);
}


/* SHOW_PARAM -- Print the name and value of a parameter on the output file
 * in the format `task.param = value'.
 */
void
show_param (struct ltask *ltp, struct param *pp, FILE *fp)
{
	char	buf[SZ_LINE+1];
	int	isstr;

	if (ltp)
	    fprintf (fp, "%s.%s", ltp->lt_lname, pp->p_name);
	else
	    fputs (pp->p_name, fp);

	fputs (" = ", fp);

	if (!(pp->p_valo.o_type & OT_UNDEF)) {
	    sprop (buf, &pp->p_valo);
	    isstr = ((pp->p_type & OT_BASIC) == OT_STRING &&
		*buf != PF_INDIRECT);
	    if (isstr)
		fputc ('"', fp);
	    fputs (buf, fp);
	    if (isstr)
		fputc ('"', fp);
	}

	fputc ('\n', fp);
}


/* LISTHELP -- List all the (visible) ltasks in the given package in the form
 * of a sorted table.  Used to give menus in response to ? and ?? directives.
 */
void
listhelp (struct package *pkp, int show_invis)
{
	static	int first_col=7, maxch=20, ncol=0;
	register struct ltask *ltp;
	register char	*ip, *op;

	char	buf[4096], *list[MAXMENU];
	int	nltask, last_col;
	FILE	*fp;

	nltask = 0;
	last_col = c_envgeti ("ttyncols") - 1;

	fp = currentask->t_stdout;
	op = buf;

	for (ltp = pkp->pk_ltp;  ltp != NULL;  ltp = ltp->lt_nlt) {
	    if (ltp->lt_flags & LT_INVIS && show_invis == NO)
		continue;
	    if (nltask >= MAXMENU)
		cl_error (E_UERR, "too many ltasks in menu");

	    /* Get task name. */
	    list[nltask++] = op;
	    for (ip=ltp->lt_lname;  (*op = *ip++);  op++)
		;

	    /* If special task, add character defining task type. */
	    if (showtype()) {
		if (ltp->lt_flags & LT_DEFPCK)
		    *op++ = '.';
		else if (ltp->lt_flags & LT_PSET)
		    *op++ = '@';
	    }

	    *op++ = EOS;
	}

	/* Sort the list and output the table. */
	if (nltask) {
	    strsort (list, nltask);
	    strtable (fp, list, nltask, first_col, last_col, maxch, ncol);
	}
}


/* LISTALLHELP -- Starting at curpack, list out all packages and their tasks
 * in a circular fashion until get back to curpack. this is like the search
 * path works.  Label the current package in some way.  Serves ?? directive.
 * TODO: this should be optimized once a nice form is settled on.
 */
void
listallhelp (int show_invis)
{
	register struct package *pkp;

	pkp = curpack;
	do {
		oprintf ("    %s:\n", pkp->pk_name);
		listhelp (pkp, show_invis);
		if ((pkp = pkp->pk_npk) == NULL)
			pkp = reference (package, pachead);
	} until (pkp == curpack);
}


/* Break a param spec of the form [[pack.]task.]param[.field] into its
 *   component parts.  Full is a pointer to the full name.  The others are the 
 *   addresses of char pointers in the calling routine that are to be set to
 *   point to the starting address, within full, of their respective components.
 *   All dots are set to \0 and serve as eos's for each component.
 *   If any of the parts are absent, the respective pointer is made to point at
 *   the trailing null of full.
 * The last part, field, is handled by fieldcvt(). it overwrites the first
 *   char of the field component with the proper FN_XXX character; it is not
 *   made into a string by adding an additional \0.
 * Call error() and do not return if something goes wrong.
 * Also used to break apart the components of full task names, [pack.]task.  In
 *   this case, the task name will be found at p and the package name at t.
 *   Fair enough; just keep them straight when calling.
 * Modified 3/26/84 by TAM to use a static buffer, rather than mutilating
 *   the input string.  This caused problems when programs looped and
 *   executed the same PUSHPARAM (or similar) more than once, e.g.
 *   	while (i<10) {= task.param; i += 1; }.
 * This bug is particularly manifest when accessing arrays in specified tasks,
 *   e.g. = task.array[*]
 */
void
breakout (char *full, char **pk, char **t, char **p, char **f)
{
	register char *cp;
	register int npts, n;
	char	*pts[3];
	static	char	buffer[SZ_LINE+1];

	strncpy (buffer, full, SZ_LINE);
	buffer[SZ_LINE] = '\0';

	for (npts=0, cp=buffer;  *cp;  cp++) {
	    if (*cp == '.') {
		if (*(cp+1) == EOS) {
		    *cp = EOS;		/* chop dot if last character	*/
		    break;
		} else {
		    if (npts > 3)
			cl_error (E_UERR, "too many dots in param name `%s'",
			    full);
		    pts[npts++] = cp;
		}
	    }
	}

	for (n=0;  n < npts;  n++)
	    *(pts[n]++) = '\0';		/* null over and skip dots	*/

	switch (npts) {
	case 0:	/* just a simple param name without dots */
	    *p = buffer;
	    *pk = *t = *f = cp;
	    break;

	case 1: /* p.f or t.p depending on f */
	    if (fieldcvt (pts[0])) {
		/* p.f */
		*pk = *t = cp; *p = buffer; *f = pts[0];
	    } else {
		/* t.p */
		*pk = *f = cp; *t = buffer; *p = pts[0];
	    }
	    break;

	case 2: /* t.p.f or pk.t.p depending on f */
	    if (fieldcvt (pts[1])) {
		/* t.p.f */
		*pk = cp; *t = buffer; *p = pts[0]; *f = pts[1];
	    } else {
		/* pk.t.p */
		*pk = buffer; *t = pts[0]; *p = pts[1]; *f = cp;
	    }
	    break;

	case 3: /* full spec */
	    *pk = buffer; *t = pts[0]; *p = pts[1]; *f = pts[2];
	    fieldcvt (*f);
	    break;
	}
}


/* If f is a valid parameter field spec, such as p_val, then overwrite *f
 *   with the proper FN_XXX character and return YES, else return NO.
 * Let the p_value field also be called p_filename, p_length and p_default.
 * Call error() if f starts with p_ but is not found or if ambiguous
 *   (and abbrevs are enabled).
 */
int
fieldcvt (register char *f)
{
	/* Field name and corresponding code tables.
	 */
	static char *fntbl[] = {
	    "p_name", "p_type", "p_mode", "p_value", "p_minimum",
	    "p_maximum", "p_prompt", "p_filename", "p_length", "p_default",
	    "p_xtype", NULL
	};
	static char fctbl[] = {
	    FN_NAME, FN_TYPE, FN_MODE, FN_VALUE, FN_MIN,
	    FN_MAX, FN_PROMPT, FN_VALUE, FN_LENGTH, FN_VALUE,
	    FN_XTYPE, NULL
	};

	int kentry;

	/* Do a quick screening first. returning NO just means that f does
	 * not even look like a field name.
	 */
	if (strncmp (f, "p_", 2))
	    return (NO);

	kentry = keyword (fntbl, f);
	if (kentry == KWBAD)
	    cl_error (E_UERR, "bad param field `%s'", f);
	else if (kentry == KWAMBIG)
	    cl_error (E_UERR, "ambiguous param field `%s'", f);

	*f = fctbl[kentry];
	return (YES);
}


/* Search though string table, tbl, for string s. last pointer in table
 * should be NULL, ie, tbl[last] == NULL (not *tbl[last] == '\0').
 * Settle for an abbreviation if they are enabled.  Return KWBAD if s
 * simply not in tbl at all, KWAMBIG if abbreviations are enabled and more
 * than one entry in tbl would match s, else the ordinal (index) into tbl
 * at which s matched.
 */
int
keyword (register char *tbl[], register char *s)
{
	register int i;
	register char *kentry;
	int	cand, len;

	i = 0;
	cand = KWBAD;
	len = strlen (s);

	if (abbrev()) {
	    for (kentry = tbl[0]; kentry; kentry = tbl[++i])
		if (!strncmp (s, kentry, len)) {
		    if (kentry[len] == '\0')
			return (i); 		/* exact hit */
		    if (cand == KWBAD)
			cand = i;
		    else
			cand = KWAMBIG; 	/* might still hit exact */
		}

	} else for (kentry = tbl[0]; kentry; kentry = tbl[++i])
	    if (!strcmp (s, kentry))
		return (i);

	return (cand);
}


/* Given a, possibly abbreviated, function name to run, look it up and
 * run it if found. it gets nargs arguments from the operand stack.
 */
void
intrfunc (char *fname, int nargs)
{
	static char *ifnames[] = {
	    "abs",       "access",    "atan2",     "cos", 	"defpac",
	    "defpar",    "deftask",   "envget",    "exp",       "frac",
	    "int",       "log",       "log10",     "nscan",     "max",
	    "min",       "mod",       "nint",      "osfn",      "radix",
	    "real",      "sin",       "sqrt",      "str",       "substr",
	    "tan",       "mktemp",    "stridx",    "strlen",	"imaccess",
	    "defvar",	 "strldx",    "strstr",    "strlwr",    "strupr",    
	    "isindef",   "strlstr",
	    NULL
	};
	static int optbl[] = {
	 UNOP|OP_ABS,      UNOP|OP_ACCESS,   BINOP|OP_ATAN2,    UNOP|OP_COS,
	 UNOP|OP_DEFPAC,   UNOP|OP_DEFPAR,    UNOP|OP_DEFTASK,  UNOP|OP_ENVGET,
	 UNOP|OP_EXP,      UNOP|OP_FRAC,      UNOP|OP_INT,      UNOP|OP_LOG,
	 UNOP|OP_LOG10,  MULTOP|OP_NSCAN,   MULTOP|OP_MAX,    MULTOP|OP_MIN,
	BINOP|OP_MOD,      UNOP|OP_NINT,      UNOP|OP_OSFN,    BINOP|OP_RADIX,
	 UNOP|OP_REAL,     UNOP|OP_SIN,       UNOP|OP_SQRT,     UNOP|OP_STR,
       MULTOP|OP_SUBSTR,   UNOP|OP_TAN,       UNOP|OP_MKTEMP,  BINOP|OP_STRIDX,
	 UNOP|OP_STRLEN,   UNOP|OP_IMACCESS,  UNOP|OP_DEFVAR,  BINOP|OP_STRLDX,
	BINOP|OP_STRSTR,   UNOP|OP_STRLWR,    UNOP|OP_STRUPR,   UNOP|OP_ISINDEF,
	BINOP|OP_STRLSTR,
	};
	int	index, op;
	int	i, n, subi[2];
	char	sbuf[SZ_LINE+1];
	struct	operand o;

	index = keyword (ifnames, fname);
	if (index == KWBAD)
	    cl_error (E_UERR, "unknown function `%s'", fname);
	if (index == KWAMBIG)
	    cl_error (E_UERR, "ambiguous function `%s'", fname);

	op = optbl[index];

	/* if do this by shifting the cases and op to the right OP_BITS, this
	 * will compile as a jump table. not worth it until it gets larger.
	 */
	switch (op & ~OP_MASK) {
	case UNOP:
	    if (nargs != 1)
		cl_error (E_UERR, e_onearg, ifnames[index]);
	    unop (op & OP_MASK);
	    break;

	case BINOP:
	    if (nargs != 2)
		cl_error (E_UERR, e_twoargs, ifnames[index]);
	    binop (op & OP_MASK);
	    break;

	case MULTOP:
	    /* right now, this is just for min, max, nscan, and substr.
	     * this will have to be smarted up if add other functions.
	     */

	    switch (op & OP_MASK) {
	    case OP_NSCAN:
		if (nargs > 0)
		    cl_error (E_UERR, "nscan has no arguments");
		o.o_type = OT_INT;
		o.o_val.v_i = get_nscanval();
		pushop (&o);
		break;
		
	    case OP_MAX:
	    case OP_MIN:
		if (nargs <= 0)
		    cl_error (E_UERR, e_geonearg, ifnames[index]);
		/* just leave top op if its the only one.
		 */
		if (nargs > 1) {
		    op &= OP_MASK;	/* avoid masking for every loop	*/
		    while (--nargs)
			binop (op);
		}
		break;

	    case OP_SUBSTR:
		if (nargs != 3)
		    cl_error (E_UERR, "substr requires 3 arguments");

		for (n=1;  n >= 0;  n--) {	/* get indices		*/
		    opcast (OT_INT);
		    o = popop();
		    subi[n] = o.o_val.v_i;
		}

		opcast (OT_STRING);		/* get string arg	*/
		o = popop();

		if (subi[1] >= subi[0]) {
		    n = subi[1] - subi[0] + 1;
		    strncpy (sbuf, &o.o_val.v_s[subi[0]-1], n);
		} else {
		    /* Reverse the string. */
		    n = subi[0] - subi[1] + 1;
		    for (i = 0; i < n; i++)
			sbuf[i] = o.o_val.v_s[subi[0]-i-1];
		}
		sbuf[n] = '\0';

		o.o_val.v_s = sbuf;
		pushop (&o);
		break;

	    default:
		goto err;
	    }
	    break;

	default:
err:	    cl_error (E_IERR, e_badsw, op, "intrfunc()");
	}
}


/* Convert string s to sexagesimal operand, of type OT_REAL.  Set opundef()
 * if conversion is bad somehow.  Allow both h:m and h:m:s forms.
 */
struct operand 
sexa (char *s)
{
	struct operand o;
	int	n, sign;
	int	hr, minutes;
	float	sec;
	extern double atof();

	o.o_type = OT_REAL;
	sign = (*s == '-') ? (s++, -1) : 1;

	minutes = 0;
	sec = 0.;
	n = sscanf (s, "%d:%d:%hf", &hr, &minutes, &sec);
	if (n < 1 || minutes < 0 || sec < 0)
	    setopundef (&o);
	else
	    o.o_val.v_r = sign * (atof (s));
	    /*  Old evaluation producing roundoff errors.
	    o.o_val.v_r = sign*(hr + ((float)minutes)/60. + sec/3600.);
	    */

	return (o);
}

/* Convert a sexagesimal real back to an index range.
 */
void
sexa_to_index (double r, int *i1, int *i2)
{
	int	sgn;

	if (r < 0) {
	    sgn = -1;
	    r = -r;
	} else
	    sgn = 1;

	*i1 = (int) r;				/* add a little for round-off*/
	*i2 = (int) (60.0e0 * (r - *i1) + .001);
	*i1 =  sgn * *i1;
}


/* ADDPIPE -- Generate a new pipe file name and push it onto the pipe stack.
 *   The strategy is to generate a unique pipefile name of the form "pipeXXX",
 *   where XXX is an integer of 5 digits or less which is what is saved on the
 *   pipe stack.  Return a pointer to the name of the new pipefile.
 */
char *
addpipe (void)
{
	static	int pipecode = 0;
	char	*pipefile();

	if (pipecode == 0)
	    pipecode = c_getpid();

	/* Get unique pipefile name described by a simple integer.
	 */
	do {
	   /*
	    * There seems to be a problem with this code in the VMS compiler.
	    * It has been changed to a form which will work for UNIX and VMS.
	    *
	    *	    pipecode = (pipecode++ % MAX_PIPECODE);
	    */
	    pipecode %= MAX_PIPECODE;

	   /* There can be applications where multiple CL are spawned in
	    * relatively short order so that the PIDs are close.  Incrementing
	    * the least significant digits can result in duplications.  So
	    * instead we use the lower digits as the "unique" part and
	    * increment the higer digits.
	    *
	    *  pipecode++;
	    */
	    pipecode += 1000;

	} while (c_access (pipefile(pipecode),0,0) == YES);

	pipetable[nextpipe++] = pipecode;
	if (nextpipe >= MAXPIPES)
	    cl_error (E_UERR, "Too many pipes");

	return (pipefile (pipecode));
}


/* GETPIPE -- Get the name of the last pipefile.
 */
char *
getpipe (void)
{
	char	*pipefile();

	if (nextpipe == 0)
	    cl_error (E_IERR, "Pipestack underflow");
	return (pipefile (pipetable[nextpipe-1]));
}


/* DELPIPES -- Delete N pipefiles (the actual file may not have been created
 * yet), and pop N pipes off the pipe stack.  If N is zero, all pipefiles are
 * deleted and the pipestack is cleared (i.e., during error recovery).
 */
void
delpipes (register int npipes)
{
	register int pipe;
	char	*pipefile();

	if (npipes == 0) {
	    while (nextpipe > 0)
		c_delete (pipefile (pipetable[--nextpipe]));
	} else {
	    while (npipes-- > 0) {
		if ((pipe = --nextpipe) < 0)
		    cl_error (E_IERR, "Pipestack underflow");
		c_delete (pipefile (pipetable[pipe]));
	    }
	}
}


/* PIPEFILE -- Given the pipecode, format a pipefile name in static internal
 * buffer and return pointer to pipefile name to caller.
 */
char *
pipefile (int pipecode)
{
	static	char fname[SZ_PIPEFILENAME+1];
	char	*dir;
	char	*envget();

	/* Put pipefiles in 'pipes' or 'uparm' if defined, else use tmp.  Do
	 * not put pipe files in current directory or pipe commands will fail
	 * when used in someone elses directory.
	 */
	if (envget ("pipes") != NULL)
	    dir = "pipes$";
	else if (envget ("uparm") != NULL)
	    dir = "uparm$";
	else
	    dir = "tmp$";
	sprintf (fname, "%spipe%d", dir, pipecode);

	return (fname);
}


/* LOOPINCR -- increments the loop counter and stores the destination
 * address for NEXT statements.  It should be called just before the
 * destination is compiled.
 */
void
loopincr (void)
{
	if (nestlevel >= MAX_LOOP)
	    cl_error (E_UERR, "Nesting too deeply.");

	brkdest[nestlevel] = 0;
	nextdest[nestlevel] = pc;
	nestlevel++;
}


/* LOOPDECR -- decrements the loop counter, and if the break destination
 * has been set it resolves the GOTO statement which has been made
 * the target of BREAK's.
 */
void
loopdecr (void)
{
	int	p_goto;

	p_goto = brkdest[--nestlevel];
	if (p_goto != 0)
	    coderef(p_goto)->c_args = pc - p_goto - 3;
}


/* SETSWITCH -- creates the jumptable which will be used in the SWITCH.
 * On entry to setswitch the stack contains a pointer to the SWITCH
 * operand, and pointers to the first and last operands of each
 * CASE and DEFAULT block, i.e. the CASE and DEFAULT operands and the
 * GOTO operands which terminate each block.
 * The jumptable is created at the location of the current pc.
 */
void
setswitch (void)
{
	int	code, jmp, njump, assgn, oper, delta;

	/* First get the size of the jump table by reading 
	 * backwards on the stack until we find the switch
	 * statement.
	 */
	oper = topcs;
	code = coderef(stack[oper])->c_opcode;
	njump = 2;

	while (code != SWITCH) {
	    if (code == CASE)
		njump++;
	    else if (code != GOTO  &&  code != DEFAULT)
		cl_error (E_UERR, "Corrupt stack in SWITCH analysis.");

	    oper++;
	    code = coderef(stack[oper])->c_opcode;
	}

	assgn = stack[oper];

	/* To create the jump table we read the control stack
	 * to get the addresses of each of the case statements
	 * and the default statement.  The values associated with
	 * each case statement are stored in that operand.  The
	 * addresses are popped and transferred to the jump table.
	 * The first location in the jump table is reserved for
	 * the DEFAULT statement and is 0 if this is not defined.
	 * We know the size of the jump table, so as we pop off
	 * the goto statements at the end of the CASE blocks
	 * we can fill in the addresses.
	 */
	jmp = pc + 1;
	oper = pop();
	code = coderef(oper)->c_opcode;
	stack[pc] = 0;

	while (code != SWITCH) {

	    switch (code) {
	    case DEFAULT:
		stack[pc] = oper-assgn;
		break;

	    case CASE:
		stack[jmp++] = oper-assgn;
		break;

	    case GOTO:
		delta = pc + njump - oper - 3;
		coderef(oper)->c_args = delta;
		break;

	    default:
		cl_error (E_UERR, "Corrupt stack in SWITCH analysis.");
	    }

	    oper = pop();
	    code = coderef(oper)->c_opcode;
	}

	stack[jmp] = 0;			/* Fill in terminator. */

	/* Put address of jump table in ASSIGN operand. 
	 */
	coderef(oper)->c_args = pc - oper;
	pc += njump;

	/* Fill in address of GOTO following ASSIGN. 
	 */
	oper += 3;
	coderef(oper)->c_args = pc - oper - 3;
}


/* IN_SWITCH -- determines whether a CASE or DEFAULT block is
 * legal at the current location.
 */
int
in_switch (void)
{
	int 	oper, code, oper2, code2, status;

	oper = pop();
	code = coderef(oper)->c_opcode;
	status = 1;

	switch (code) {
	    case SWITCH:
		push (oper);
		break;

	    case GOTO:
		/* Previous operand must be DEFAULT or CASE.
		 */
		oper2 = pop();
		code2 = coderef(oper2)->c_opcode;
		if (code2 != CASE  &&  code2 != DEFAULT)
		    status = 0;
		push (oper2);
		push (oper);
		break;

	    default:
		status = 0;
	    }

	return (status);
}


/* CASESET -- Fill in the values for which the current case block is to be
 * executed.
 */
void
caseset (memel *parg, int ncaseval)
{
	struct	operand	*o;
	static	char *badcase = "Invalid case constant.";
	int	ival;

	for (ival = 0; ival < ncaseval; ival++) {

	    o = (struct operand *) pop();

	    if (o->o_type == OT_STRING) {
		/* Only chars, not full strings.
		 */
		if (*o->o_val.v_s == 0)
		    cl_error (E_UERR, badcase);
		if (*(o->o_val.v_s + 1) != 0)
		    cl_error (E_UERR, badcase);

		*parg++ = (int) *o->o_val.v_s;

	    } else if (o->o_type == OT_INT) {
	        *parg++ = o->o_val.v_i;

	    } else
		cl_error (E_UERR, badcase);
	}
}


/* SETLABEL -- called when a label is first seen. It allocates
 * space for the label on the dictionary and also copies the
 * label name onto the dictionary.  The label is placed at the
 * top of a linked list.
 */
struct label *
setlabel (struct operand *name)
{
	struct	label	*p;

	p = (struct label *) memneed (sizeof(struct label));
	p->l_name = comdstr (name->o_val.v_s);

	if (label1 == NULL)
	    p->l_next = NULL;
	else
	    p->l_next = label1;

	label1 = p;
	return (p);
}


/* GETLABEL -- returns the label struct corresponding to the string
 * name, or NULL if the label has not been defined.
 */
struct label *
getlabel (struct operand *name)
{
	struct	label	*l;

	l = label1;
	while (l != NULL) {
	    if (!strcmp (name->o_val.v_s, l->l_name))
		return (l);
	    l = l->l_next;
	}

	return (NULL);
}


/* SETIGOTO -- maintains the list of indirect goto's.
 * Note that an indirect GOTO is identical in format to a
 * normal GOTO.  The argument, instead of pointing to the destination
 * is used as the list pointer.  When the destination is defined,
 * the GOTO is taken out of the indirect list.
 */
void
setigoto (int loc)
{
	if (igoto1 < 0)
	    coderef(loc)->c_args = -1;
	else
	    coderef(loc)->c_args = igoto1;

	igoto1 = loc;
}


/* UNSETIGOTO -- takes a GOTO out of the indirect list so that
 * the target may be put in the argument.
 */
void
unsetigoto (int loc)
{
	int 	last, curr;

	last = NULL;
	curr = igoto1;

	while (curr != loc) {
	    last = curr;
	    curr = coderef(curr)->c_args;
	}

	if (last == NULL)
	    igoto1 = coderef(curr)->c_args;
	else
	    coderef(last)->c_args = coderef(curr)->c_args;
}


/* MAKE_IMLOOP -- compiles the meta-code for the indexing of arrays in
 * implicit array loops e.g. a[*,5].
 */
int
make_imloop (int i1, int i2)
{
	int 	mode;

	if (n_oarr) {
	    /* Array limits already defined, check for agreement. 
	     */
	    if (i1 != oarr_beg[i_oarr] || i2 != oarr_end[i_oarr])
		cl_error (E_UERR, "Inconsistent open refs.\n");
	    mode = -1;
	} else {
	    oarr_beg[i_oarr] = i1;
	    oarr_end[i_oarr] = i2;
	    if (i_oarr)
		mode = -1;
	    else
		/* This is the PUSHINDEX which will
		 * initialize the loop variables.
		 */
		mode = 0;
	}
	i_oarr++;

	return (mode);
}


/* Y_TYPEDEF -- Convert a type specifier keyword into a datatype code.
 */
int
y_typedef (char *key)
{
	if (strcmp (key, "string") == 0 || strcmp (key, "char") == 0)
	    return (V_STRING);
	else if (strcmp (key, "int") == 0)
	    return (V_INT);
	else if (strcmp (key, "real") == 0)
	    return (V_REAL);
	else if (strcmp (key, "bool") == 0)
	    return (V_BOOL);
	else if (strcmp (key, "file") == 0)
	    return (V_FILE);
	else if (strcmp (key, "gcur") == 0)
	    return (V_GCUR);
	else if (strcmp (key, "imcur") == 0)
	    return (V_IMCUR);
	else if (strcmp (key, "ukey") == 0)
	    return (V_UKEY);
	else if (strcmp (key, "pset") == 0)
	    return (V_PSET);
	else if (strcmp (key, "struct") == 0)
	    return (V_STRUCT);
	else
	    cl_error (E_UERR, "illegal type specifier `%s'", key);
	/*NOTREACHED*/
	return (0);
}


/* P_POSITION -- Called when we get a syntax error in the parser.  Print
 * the current cmdblk and point to the offending token.
 */
void
p_position (void)
{
	register int i;

	eprintf ("**: %s    ", cmdblk);		/* '\n' in cmdblk */

	for (i=0; i < err_cmdblk-cmdblk; i++)
	    eprintf ("%c", ((cmdblk[i] == '\t') ? '\t' : ' ') );

	eprintf ("^\n");
}
