/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include "xpp.h"
#include "../../bootProto.h"

#define	import_spp
#include <iraf.h>

/*
 * C code for the first pass of the IRAF subset preprocessor (SPP).
 * The decision to initially organize the SPP compiler into two passes was
 * made to permit maximum use of the existing raftor preprocessor, which is
 * the basis for the second pass of the SPP.  Eventually the two passes
 * should be combined into a single program.  Most of the operations performed
 * by the first pass (XPP) should be performed AFTER macro substitution,
 * rather than before as is the case in the current implementation, which
 * processes macros in the second pass (RPP).
 *
 * Beware that this is not a very good program which was not carefully
 * designed and which was never intended to have a long lifetime.  The next
 * step is to replace the two passes by a single program which is functionally
 * very similar, but which is more carefully engineered and which is written
 * in the SPP language calling IRAF file i/o.  Eventually a true compiler
 * will be written, providing many new features, i.e., structures and pointers,
 * automatic storage class, mapped arrays, enhanced i/o support, and good
 * compile time error checking.  This compiler will also feature a table driven
 * code generator (generating primitive Fortran statements), which will provide
 * greater machine independence.
 */


extern	char *vfn2osfn();

/* Escape sequence characters and their binary equivalents.
 */
char	*esc_ch  = "ntfr\\\"'";
char	*esc_val = "\n\t\f\r\\\"\'";

/* External and internal data stuctures.  We need access to the LEX i/o
 * buffers because we use the LEX i/o macros, which provide pushback,
 * because we must change the streams to process includes, and so on.
 * These definitions are VERY Lex dependent.
 */
extern	char	yytext[];		/* LEX character buffer		*/
extern	int	yyleng;			/* length of string in yytext	*/
extern	FILE	*yyin, *yyout;		/* LEX input, output files	*/

extern	char	yytchar, *yysptr, yysbuf[];
extern	int	yylineno;

#define U(x) x

extern int   yy_input(void);
extern void  yy_unput(char ch);
extern void  d_codegen (register FILE *fp);
extern void  d_runtime (char *text);

int	context = GLOBAL;		/* lexical context variable	*/
extern	int hbindefs, foreigndefs;
char	*machdefs[] = { "mach.h", "config.h", "" };

/* The task structure is used for TASK declarations.  Since this is a
 * throwaway program we do not bother with dynamic storage allocation,
 * which would remove the limit on the number of tasks in a task statment.
 */
struct task {
	char	*task_name;		/* logical task name		*/
	char	*proc_name;		/* name of procedure		*/
	short	name_offset;		/* offset of name in dictionary	*/
};

/* The string structure is used for STRING declarations and for inline
 * strings.  Strings are stored in a fixed size, statically allocated
 * string buffer.
 */
struct string {
	char	*str_name;		/* name of string		*/
	char	*str_text;		/* ptr to text of string	*/
	short	str_length;		/* length of string		*/
};

struct	task	task_list[MAX_TASKS];
struct	string	string_list[MAX_STRINGS];

FILE	*istk[MAX_INCLUDE];		/* stack for input file descriptors   */
int	linenum[MAX_INCLUDE];		/* line numbers in files 	      */
char	fname[MAX_INCLUDE][SZ_PATHNAME];/* file names 			      */
int	istkptr = 0; 			/* istk pointer			      */

char	obuf[SZ_OBUF];			/* buffer for body of procedure       */
char	dbuf[SZ_DBUF];			/* buffer for misc proc. decls.	      */
char	sbuf[SZ_SBUF];			/* string buffer 		      */
char	*sp = sbuf;			/* string buffer pointer	      */
char	*op = obuf;			/* pointer in output buffer	      */
char	*dp = dbuf;			/* pointer in decls buffer	      */
int	nstrings = 0;			/* number of strings so far	      */
int	strloopdecl;			/* data dummy do index declared?      */

int	ntasks = 0;			/* number of tasks in interpreter     */
int	str_idnum = 0;			/* for generating unique string names */
int	nbrace = 0;			/* must be zero when "end" is reached */
int	nswitch = 0;			/* number switch stmts in procedure   */
int	errflag;
int	errhand = NO;			/* set if proc employs error handler  */
int	errchk = NO;			/* set if proc employs error checking */


void  skipnl (void);
void  setcontext (int new_context);
void  pushcontext (int new_context);
int   popcontext (void);
void  hashtbl (void);
int   findkw (void);
void  mapident (void);
void  str_enter (void);
char *str_fetch (register char *strname);
void  macro_redef (void);
void  setline (void);
void  output (char ch);

void  do_type (int type);
void  do_char (void);
void  skip_helpblock (void);
int   parse_task_statement (void);
int   get_task (char *task_name, char *proc_name, int maxch);
int   get_name (char *outstr, int maxch);
int   nextch (void);
void  put_dictionary (void);
void  put_interpreter (void);
void  outstr (char *string);
void  begin_code (void);
void  end_code (void);
void  init_strings (void);
void  write_string_data_statement (struct string *s);
void  do_string (char delim, int strtype);
void  do_hollerith (void);
void  sbuf_check (void);

char *str_uniqid (void);
void  traverse (char delim);
void  error (int errcode, char *errmsg);
void  xpp_warn (char *warnmsg);
long  accum (int base, char **strp);

int   charcon (char *string);
void  int_constant (char *string, int base);
void  hms (char *number);



/* SKIPNL -- Skip to newline, e.g., when a comment is encountered.
 */
void
skipnl (void)
{
	int c;
	while ((c=yy_input()) != '\n')
	    ;
	yy_unput ('\n');
}


/*
 * CONTEXT -- Package for setting, saving, and restoring the lexical context.
 * The action of the preprocessor in some cases depends upon the context, i.e.,
 * what type of statement we are processing, whether we are in global space,
 * within a procedure, etc.
 */

#define	MAX_CONTEXT	5	/* max nesting of context	*/

int	cntxstk[MAX_CONTEXT];	/* for saving context		*/
int	cntxsp = 0;		/* save stack pointer		*/


/* SETCONTEXT -- Set the context.  Clears any saved context.
 */
void
setcontext (int new_context)
{
	context = new_context;
	cntxsp = 0;
}


/* PUSHCONTEXT -- Push a temporary context.
 */
void
pushcontext (int new_context)
{
	cntxstk[cntxsp++] = context;
	context = new_context;

	if (cntxsp > MAX_CONTEXT)
	    error (XPP_COMPERR, "save context stack overflow");
}


/* POPCONTEXT -- Pop the former context.  If the current context is PROCSTMT
 * (just finished compiling a procedure statement) then set the context to DECL
 * to indicate that we are entering the declarations section of a procedure.
 */
int
popcontext (void)
{
	if (context & PROCSTMT) {
	    context = DECL;
	    if (cntxsp > 0)
		--cntxsp;
	} else if (cntxsp > 0)
	    context = cntxstk[--cntxsp];

	return (context);
}


/* Keyword table.  The simple hashing scheme requires that the keywords appear
 * in the table in sorted order.
 */
#define LEN_KWTBL	18

struct {
	char	*keyw;		/* keyword name string			*/
	short	opcode;		/* opcode from above definitions	*/
	short	nelem;		/* number of table elements to skip if
				 * to get to next character class.
				 */
} kwtbl[] = {
    { "FALSE",	    XTY_FALSE,	  0 },
    { "TRUE",	    XTY_TRUE,	  0 },
    { "bool",	    XTY_BOOL,	  0 },
    { "char",	    XTY_CHAR,	  1 },
    { "complex",    XTY_COMPLEX,  0 },
    { "double",	    XTY_DOUBLE,	  0 },
    { "error",	    XTY_ERROR,	  1 },
    { "extern",	    XTY_EXTERN,	  0 },
    { "false",	    XTY_FALSE,	  0 },
    { "iferr",	    XTY_IFERR,	  2 },
    { "ifnoerr",    XTY_IFNOERR,  1 },
    { "int",	    XTY_INT,	  0 },
    { "long",	    XTY_LONG,	  0 },
    { "pointer",    XTY_POINTER,  1 },
    { "procedure",  XTY_PROC,	  0 },
    { "real",	    XTY_REAL,	  0 },
    { "short",	    XTY_SHORT,	  0 },
    { "true",	    XTY_TRUE,	  0 },
};

/* short kwindex[30];		simple alphabetic hash index		*/
/* #define CINDEX(ch)		(isupper(ch)?ch-'A':ch-'a')		*/

#define MAXCH 128
short	kwindex[MAXCH];		/* simple alphabetic hash index		*/
#define CINDEX(ch)		(ch)


/* HASHTBL -- Hash the keyword table.  Initializes the "kwindex" hash table.
 * For each character in the alphabet, the index gives the index into the
 * sorted keyword table.  If there is no keyword name beginning with the index
 * character, the index entry is set to -1.
 */
void
hashtbl (void)
{
	int	i, j;

	for (i=j=0;  i <= MAXCH;  i++) {
	    if (i == CINDEX (kwtbl[j].keyw[0])) {
		kwindex[i] = j;
		j = min (LEN_KWTBL-1, j + kwtbl[j].nelem + 1);
	    } else
		kwindex[i] = -1;
	}
}


/* FINDKW -- Lookup an indentifier in the keyword table.  Return the opcode
 * of the keyword, or ERR if no match.
 */
int
findkw (void)
{
	register char ch, *p, *q;
	int	i, ilimit;

	if (kwindex[0] == 0)
	    hashtbl();

	i = CINDEX (yytext[0]);
	if (i < 0 || i >= MAXCH || (i = kwindex[i]) < 0)
	    return (ERR);
	ilimit = i + kwtbl[i].nelem;

	for (;  i <= ilimit;  i++) {
	    p = kwtbl[i].keyw + 1;
	    q = yytext + 1;

	    for (;  *p != EOS;  q++, p++) {
		ch = *q;
		/* 5DEC95 - Don't case convert keywords.
		if (isupper (ch))
		    ch = tolower (ch);
		 */
		if (*p != ch)
		    break;
	    }
	    if (*p == EOS && *q == EOS)
		return (kwtbl[i].opcode);
	}
	return (ERR);
}
		

/* MAPIDENT -- Lookup an identifier in the keyword table.  If the identifier is
 * not a keyword, output it as is.  If a datatype keyword, the action depends
 * on whether we are in a procedure body or not (i.e., whether the keyword
 * begins a declaration or is a type coercion function).  Most of the other
 * keywords are mapped into special x$.. identifiers for further processing
 * by the second pass.
 */
void
mapident (void)
{
	int     i, findkw();
	char	*str_fetch();
	register char *ip, *op;

	/* If not keyword and not defined string, output as is.  The first
	 * char must be upper case for the name to be recognized as that of
	 * a defined string.  If we are processing a "define" macro expansion
	 * is disabled.
	 */
	if ((i = findkw()) == ERR) {
	    if (!isupper(yytext[0]) || (context & DEFSTMT) ||
		(ip = str_fetch (yytext)) == NULL) {

		outstr (yytext);
		return;

	    } else {
		yyleng = 0;
		for (op=yytext;  (*op++ = *ip++) != EOS;  )
		    yyleng++;
		do_string ('"', STR_DEFINE);
		return;
	    }
	}

	/* If datatype keyword, call do_type. */
	if (i <= XTY_POINTER) {
	    do_type (i);
	    return;
	}
	
	switch (i) {
	case XTY_TRUE:
	    outstr (".true.");
	    break;
	case XTY_FALSE:
	    outstr (".false.");
	    break;
	case XTY_IFERR:
	case XTY_IFNOERR:
	    outstr (yytext);
	    errhand = YES;
	    errchk = YES;
	    break;
	case XTY_ERROR:
	    outstr (yytext);
	    errchk = YES;
	    break;

	case XTY_EXTERN:
	    /* UNREACHABLE (due to decl.c additions).
	     */
	    outstr ("x$extn");
	    break;

	default:
	    error (XPP_COMPERR, "Keyword lookup error");
	}
}


char	st_buf[SZ_STBUF];
char	*st_next = st_buf;

struct st_def {
	char	*st_name;
	char	*st_value;
} st_list[MAX_DEFSTR];

int	st_nstr = 0;

/* STR_ENTER -- Enter a defined string into the string table.  The string
 * table is a kludge to provide the capability to define strings in SPP.
 * The problem is that XPP handles strings but RPP handles macros, hence
 * strings cannot be defined.  We get around this by recognizing defines
 * of the form  'define NAME "..."'.  If a macro with a quoted value is
 * encounted we are called to enter the name and the string into the
 * table.  LOOKUP, above, subsequently searches the table for defined
 * strings.  The name must be upper case or the table will not be searched.
 *
 * N.B.: we are called by the lexical analyser with 'define name "' in
 * yytext.  The next yy_input() will return the first char of the string.
 */
void
str_enter (void)
{
	register char	*ip, *op, ch;
	register struct st_def *s;
	register int	n;
	char	name[SZ_FNAME+1];


	/* Skip to the first char of the name string.
	 */
	ip = yytext;
	while (isspace (*ip))
	    ip++;
	while (!isspace (*ip))
	    ip++;
	while (isspace (*ip))
	    ip++;

	/* Do not accept statement unless the name is upper case.
	 */
	if (!isupper (*ip)) {
	    outstr (yytext);
	    return;
	}

	/* Extract macro name. */
	for (op=name;  (isalnum(*ip) || *ip == '_');  )
	    *op++ = *ip++;
	*op = EOS;

	/* Check for a redefinition. */
	for (n=st_nstr, s=st_list, ch=name[0];  --n >= 0;  s++) {
	    if (*(s->st_name) == ch)
		if (strcmp (s->st_name, name) == 0)
		    break;
	}

	/* Make a new entry?. */
	if (n < 0) {
	    s = &st_list[st_nstr++];
	    if (st_nstr >= MAX_DEFSTR)
		error (XPP_COMPERR, "Too many defined strings");

	    /* Put defined NAME in string buffer.  */
	    for (s->st_name = st_next, (ip=name);  (*st_next++ = *ip++);  )
		;
	}

	/* Put value in string buffer.
	 */
	s->st_value = st_next;
	traverse ('"');
	for (ip=yytext;  (*st_next++ = *ip++) != EOS;  )
	    ;
	*st_next++ = EOS;

	if (st_next - st_buf >= SZ_STBUF)
	    error (XPP_COMPERR, "Too many defined strings");
}


/* STR_FETCH -- Search the defined string table for the named string
 * parameter and return a pointer to the string if found, NULL otherwise.
 */
char *
str_fetch (register char *strname)
{
	register struct st_def *s = st_list;
	register int n = st_nstr;
	register char ch = strname[0];

	while (--n >= 0) {
	    if (*(s->st_name) == ch)
		if (strcmp (s->st_name, strname) == 0)
		    return (s->st_value);
	    s++;
	}
	
	return (NULL);
}


/*  MACRO_REDEF --  Redefine the macro to automatically add a P2<T> macro
 *  to struct definitions.
 */
void
macro_redef (void)
{
	register int	nb=0;
	register char	*ip, *op, ch;
	char	name[SZ_FNAME];
	char	value[SZ_LINE];


	outstr ("define\t");
	memset (name, 0, SZ_FNAME);
	memset (value, 0, SZ_LINE);

	/* Skip to the first char of the name string.
	 */
	ip = yytext;
	while (isspace (*ip))
	    ip++;
	while (!isspace (*ip))
	    ip++;
	while (isspace (*ip))
	    ip++;

	/* Extract macro name. */
	for (op=name;  (isalnum(*ip) || *ip == '_');  )
	    *op++ = *ip++;
	*op = EOS;
	outstr (name);
	outstr ("\t");


	/*  Modify value.
	 */
	op = value;
        while ( (ch = yy_input()) != EOF ) {
	    if (ch == '\n') {
		break;
	    } else if (ch == '#') {		/* eat a comment	*/
		while ((ch = yy_input()) != '\n')
		    ;
		break;


	    } else {
		if (ch == '[') {
		    nb++; 
		    if (nb > 1) *op++ = '(';
		} else if (ch == ']') {
		    nb--; 
		    if (nb <= 0)
		        break;
		    else
		        *op++ = ')';
		} else if (nb >= 1)
		    *op++ = ch;
	    }
	}

	outstr ("Memr(");
	if (strcmp (value, "$1") == 0) {
#if defined(MACH64) && defined(AUTO_P2R)
	    char *emsg[SZ_LINE];
	    int strict = 0;
#endif

	    /*  A macro such as "Memr[$1]" which is typically used as a 
	     *  shorthand for an array allocated as TY_REAL and not a part
	     *  of a struct, however it might also be the first element of
	     *  a struct.  In this case, print a warning so it can be checked
	     *  manually and just pass it through.
	     */
#if defined(MACH64) && defined(AUTO_P2R)
	    memset (emsg, 0, SZ_LINE);
	    sprintf (emsg, 
		"Error in %s: line %d: ambiguous Memr for '%s' needs P2R/P2P", 
		fname[istkptr], linenum[istkptr], name);
	    if (strict)
	        error (XPP_COMPERR, emsg);
	    else
		fprintf (stderr, "%s\n", emsg);
#endif
	    outstr (value);

	} else if (strncmp ("Mem", value, 3) == 0 || isupper (value[0])) {
	    /*  In this case we assume a complex macro using some other
	     *  Mem element or an upper-case macro.  These are again used
	     *  typically as a shorthand and use pointers directly, so pass
	     *  it through unchanged.
	     */
	    outstr (value);

	} else {
	    /*  Assume it's part of a struct, e.g. "Memr[$1+N]".  
	     *
	     *  FIXME --  We should really be more careful to check the syntax.
	    fprintf (stderr, "INFO %s line %d: ", 
		fname[istkptr], linenum[istkptr]);
	    fprintf (stderr, "adding P2R macro for '%s'\n", name);
	     */
#if defined(MACH64) && defined(AUTO_P2R)
	    if (value[0] == '$') {
	        outstr ("P2R(");
	        outstr (value);
	        outstr (")");
	    } else
	        outstr (value);
#else
	    outstr (value);
#endif
	}
	outstr (")\n");

	linenum[istkptr]++;
}


/* SETLINE -- Set the file line number.  Used by the first pass to set
 * line number after processing an include file and in various other
 * places.  Necessary to get correct line numbers in error messages from
 * the second pass.
 */
void
setline (void)
{
	char	msg[20];

	if (istkptr == 0) {			/* not in include file */
	    sprintf (msg, "#!# %d\n", linenum[istkptr] - 1);
	    outstr (msg);
	}
}


/* OUTPUT -- Output a character.  If we are processing the body of a procedure
 * or a data statement, put the character into the output buffer.  Otherwise
 * put the character to the output file.
 *
 * NOTE -- the redirection logic shown below is duplicated in OUTSTR.
 */
void
output (char ch)
{
	if (context & (BODY|DATASTMT)) {
	    /* In body of procedure or in a data statement (which is output
	     * just preceding the body).
	     */
	    *op++ = ch;
	    if (op >= &obuf[SZ_OBUF]) {
		error (XPP_COMPERR, "Output buffer overflow");
		_exit (1);
	    }
	} else if (context & DECL) {
	    /* Output of a miscellaneous declaration in the declarations
	     * section.
	     */
	    *dp++ = ch;
	    if (dp >= &dbuf[SZ_DBUF]) {
		error (XPP_COMPERR, "Declarations buffer overflow");
		_exit (1);
	    }
	} else {
	    /* Outside of a procedure.
	     */
	    putc (ch, yyout);
	}
}


/* Datatype keywords for declarations.  The special x$.. keywords are 
 * for communication with the second pass.  Note that this table is machine
 * dependent, since it maps char into type short.
 */
char *type_decl[] = RPP_TYPES;


/* Intrinsic functions used for type coercion.  These mappings are machine
 * dependent (MACHDEP).  If your machine has INTEGER*2 and INTEGER*4, and
 * integer cannot be passed as an argument when a short or long is expected,
 * and your compiler has INT2 and INT4 type coercion intrinsic functions,
 * you should use those here instead of INT (which happens to work for a VAX).
 * If you cannot pass an int when a short is expected (i.e., IBM), and you
 * do not have an INT2 intrinsic function, you should provide an external
 * INTEGER*2 function called "int2" and use that for type coercion.  Note
 * that it will then be necessary to have the preprocessor automatically
 * generate a declaration for the function.  This nonsense will all go away
 * when we set up a proper table driven code generator!!
 */
char	*intrinsic_function[] = {
	"",			/* table is one-indexed		*/
	"(0 != ",		/* bool(expr)			*/
	"int",			/* char(expr)			*/
	"int",			/* short(expr)			*/
	"int",			/* int(expr)			*/
	"int",			/* long(expr)			*/
	"real",			/* real(expr)			*/
	"dble",			/* double(expr)			*/
	"cmplx",		/* complex(expr)		*/
	"int"			/* pointer(expr)		*/
};


/* DO_TYPE -- Process a datatype keyword.  The type of processing depends
 * on whether we are called when processing a declaration or an expression.
 * In expressions, the datatype keyword is the type coercion intrinsic
 * function.  DEFINE statements are a special case; we treat them as
 * expressions, since macros containing datatype keywords are used in
 * expressions more than in declarations.  This is a kludge until the problem
 * is properly resolved by processing macros BEFORE code generation.
 * In the current implementation, macros are handled by the second pass (RPP).
 */
void
do_type (int type)
{
	char    ch;

	if (context & (BODY|DEFSTMT)) {
	    switch (type) {
	    case XTY_BOOL:
		for (ch=yy_input();  ch == ' ' || ch == '\t';  ch=yy_input())
		    ;
		if (ch != '(')
		    error (XPP_SYNTAX, "Illegal boolean expr");
		outstr (intrinsic_function[type]);
		return;

	    case XTY_CHAR:
	    case XTY_SHORT:
	    case XTY_INT:
	    case XTY_LONG:
	    case XTY_REAL:
	    case XTY_DOUBLE:
	    case XTY_COMPLEX:
	    case XTY_POINTER:
		outstr (intrinsic_function[type]);
		return;

	    default:
		error (XPP_SYNTAX, "Illegal type coercion");
	    }

	} else {
	    /* UNREACHABLE when in declarations section of a procedure.
	     */
	    fprintf (yyout, "%s", type_decl[type]);
	}
}


/* DO_CHAR -- Process a char array declaration.  Add "+1" to the first
 * dimension to allow space for the EOS.  Called after LEX has recognized
 * "char name[".  If we reach the closing ']', convert it into a right paren
 * for the second pass.
 */
void
do_char (void)
{
	char	ch;

	for (ch=yy_input();  ch != ',' && ch != ']';  ch=yy_input())
	    if (ch == '\n' || ch == EOS) {
		error (XPP_SYNTAX, "Missing comma or ']' in char declaration");
		yy_unput ('\n');
		return;
	    } else
		output (ch);

	outstr ("+1");
	if (ch == ']')
	    output (')');
	else
	    output (ch);
}


/* SKIP_HELPBLOCK -- Skip over a help block (documentation section).
 */
void
skip_helpblock (void)
{
	char   ch;


	/*  fgets() no longer works with FLEX 
	while (fgets (yytext, SZ_LINE, yyin) != NULL) {
	    if (istkptr == 0)
		linenum[istkptr]++;

	    if (yytext[0] == '.' && (yytext[1] == 'e' || yytext[1] == 'E')) {
		yytext[8] = EOS;
		if (strcmp (&yytext[1], "endhelp") == 0 ||
		    strcmp (&yytext[1], "ENDHELP") == 0)
			break;
	    }
	}
	*/

	while ( (ch = yy_input()) != EOF ) {
	    if (ch == '.') {		/* check for ".endhelp"		*/
		ch = yy_input();
		if (ch == 'e' || ch == 'E') {
		    for (ch = yy_input() ; ch != '\n' && ch != EOS;  ch=yy_input())
	    	        ;
		    break;
		} else
		    for (ch = yy_input() ; ch != '\n' && ch != EOS;  ch=yy_input())
	    	        ;
	
	    } else if (ch == '\n') {	/* skip line			*/
		;
	    } else {
		for (ch=yy_input();  ch != '\n' && ch != EOS;  ch=yy_input())
	    	    ;
	    }
	    if (istkptr == 0)
	        linenum[istkptr]++;
        }
}


/* PARSE_TASK_STATEMENT -- Parse the task statement, building up a list
 *   of task_name/procedure_name structures in the "task_list" array.
 *
 *	task	task1, task2, task3=proc3, task4, ...
 *
 * Task names are placed in the string buffer as one big string, with EOS
 *   delimiters between the names.  This "dictionary" string is converted
 *   into a data statement at "end_code" time, along with any other strings
 *   in the runtask procedure.  The procedure names, which may differ from
 *   the task names, are saved in the upper half of the output buffer.  We can
 *   do this because we know that the runtask procedure is small and will not
 *   come close to filling up the output buffer, which buffers only the body
 *   of the procedure currently being processed.
 * N.B.: Upon entry, the input is left positioned to just past the "task"
 *   keyword.
 */
int
parse_task_statement (void)
{
	register struct task *tp;
	register char ch, *ip;
	char	task_name[SZ_FNAME], proc_name[SZ_FNAME];
	int	name_offset;

	/* Set global pointers to where we put task and proc name strings.
	 */
	sp = sbuf;
	op = &obuf[SZ_OBUF/2];
	name_offset = 1;

	for (ntasks=0;  ntasks < MAX_TASKS;  ntasks++) {
	    /* Process "taskname" or "taskname=procname".  There must be
	     * at least one task name in the declaration.
	     */
	    if (get_task (task_name, proc_name, SZ_FNAME) == ERR)
		return (ERR);
	    
	    /* Set up the task declaration structure, and copy name strings
	     * into the string buffers.
	     */
	    tp = &task_list[ntasks];
	    tp->task_name = sp;
	    tp->proc_name = op;
	    tp->name_offset = name_offset;
	    name_offset += strlen (task_name) + 1;
	    
	    for (ip=task_name;  (*sp++ = *ip++) != EOS;  )
		if (sp >= &sbuf[SZ_SBUF])
		    goto err;
	    for (ip=proc_name;  (*op++ = *ip++) != EOS;  )
		if (op >= &obuf[SZ_OBUF])
		    goto err;

	    /* If the next character is a comma, skip it and a newline if
	     * one follows and continue processing.  If the next character is
	     * a newline, we are done.  Any other character is an error.
	     * Note that nextch skips whitespace and comments.
	     */
	    ch = nextch();
	    if (ch == ',') {
		if ((ch = nextch()) != '\n')
		    yy_unput (ch);
	    } else if (ch == '\n') {
		linenum[istkptr]++;
		ntasks++;			/* end of task statement */	
		break;
	    } else
		return (ERR);
	}

	if (ntasks >= MAX_TASKS) {
err:	    error (XPP_COMPERR, "too many tasks in task statement");
	    return (ERR);
	}

	/* Set up the task name dictionary string so that it gets output
	 * as a data statement when the runtask procedure is output.
	 */
	string_list[0].str_name = "dict";
	string_list[0].str_text = sbuf;
	string_list[0].str_length = (sp - sbuf);
	nstrings = 1;

	/* Leave the output buffer pointer pointing to the first half of
	 * the buffer.
	 */
	op = obuf;
	return (OK);
}


/* GET_TASK -- Process a single task declaration of the form "taskname" or
 * "taskname = procname".
 */
int
get_task (char *task_name, char *proc_name, int maxch)
{
	register char ch;

	/* Get task name.
	 */
	if (get_name (task_name, maxch) == ERR)
	    return (ERR);
	
	/* Get proc name if given, otherwise the procedure name is assumed
	 * to be the same as the task name.
	 */
	if ((ch = nextch()) == '=') {
	    if (get_name (proc_name, maxch) == ERR)
		return (ERR);
	} else {
	    yy_unput (ch);
	    strncpy (proc_name, task_name, maxch);
	}

	return (XOK);
}


/* GET_NAME -- Extract identifier from input, placing in the output string.
 * ERR is returned if the output string overflows, or if the token is not
 * a legal identifier.
 */
int
get_name (char *outstr, int maxch)
{
	register char ch, *op;
	register int nchars;

	yy_unput ((ch = nextch()));	/* skip leading whitespace	*/

	for (nchars=0, op=outstr;  nchars < maxch;  nchars++) {
	    ch = yy_input();
	    if (isalpha(ch)) {
		if (isupper(ch))
		    *op++ = tolower(ch);
		else
		    *op++ = ch;
	    } else if ((isdigit(ch) && nchars > 0) || ch == '_' || ch == '$') {
		*op++ = ch;
	    } else {
		*op++ = EOS;
		yy_unput (ch);
		return (nchars > 0 ? XOK : ERR);
	    }
	}

	return (ERR);
}


/* NEXTCH -- Get next nonwhite character from the input stream.  Ignore
 * comments.  Newline is not considered whitespace.
 */
int
nextch (void)
{
	register char ch;

	while ((ch = yy_input()) != EOF) {
	    if (ch == '#') {			/* discard comment */
		while ((ch = yy_input()) != '\n')
		    ;
		return (ch);
	    } else if (ch != ' ' && ch != '\t')
		return (ch);
	}
	return (EOF);
}


/* PUT_DICTIONARY -- We are called when the keyword TN$DECL is encountered,
 * i.e., while processing "sysruk.x".  This should only happen after the
 * task statement has been successfully processed.  Our function is to replace
 * the TN$DECL macro by the declarations for the DP and DICT structures.
 * DP is an integer array giving the offsets of the task name strings in DICT,
 * the dictionary string buffer.
 */
#define	NDP_PERLINE		8	/* num DP data elements per line */

void
put_dictionary (void)
{
	register struct task *tp;
	char	buf[SZ_LINE];
	int	i, j, offset;

	/* Discard anything found on line after the TN$DECL, which is only
	 * recognized as the first token on the line.
	 */
	while (yy_input() != '\n')
	    ;
	yy_unput ('\n');

	/* Output the data statements required to initialize the DP array.
	 * These statements are spooled into the output buffer and not output
	 * until all declarations have been processed, since the Fortran std
	 * requires that data statements follow declarations.
	 */
	pushcontext (DATASTMT);
	tp = task_list;
	
	for (j=0;  j <= ntasks;  j += NDP_PERLINE) {
	    if (!strloopdecl++) {
		pushcontext (DECL);
		sprintf (buf, "%s\tiyy\n", type_decl[TY_INT]);
		outstr (buf);
		popcontext();
	    }

	    sprintf (buf, "data\t(dp(iyy),iyy=%2d,%2d)\t/",
		j+1, min (j+NDP_PERLINE, ntasks+1));
	    outstr (buf);

	    for (i=j;  i < j+NDP_PERLINE && i <= ntasks;  i++) {
		offset = (tp++)->name_offset;
		if (i >= ntasks)
		    sprintf (buf, "%2d/\n", XEOS);
		else if (i == j + NDP_PERLINE - 1)
		    sprintf (buf, "%4d/\n", offset==EOS ? XEOS: offset);
		else
		    sprintf (buf, "%4d,", offset==EOS ? XEOS: offset);
		outstr (buf);
	    }
	}

	popcontext();

	/* Output type declarations for the DP and DICT arrays.  The string
	 * descriptor for string 0 (dict) was prepared when the TASK statement
	 * was processed.
	 */
	sprintf (buf, "%s\tdp(%d)\n", type_decl[XTY_INT], ntasks + 1);
	outstr (buf);
	sprintf (buf, "%s\tdict(%d)\n", type_decl[XTY_CHAR],
	    string_list[0].str_length);
	outstr (buf);
}


/* PUT_INTERPRETER -- Output the statements necessary to scan the dictionary
 * for a task and call the associated procedure.  We are called when the
 * keyword TN$INTERP is encountered in the input stream.
 */
void
put_interpreter (void)
{
	char	lbuf[SZ_LINE];
	int	i;

	while (yy_input() != '\n')		/* discard rest of line */
	    ;
	yy_unput ('\n');

	for (i=0;  i < ntasks;  i++) {
	    sprintf (lbuf, "\tif (streq (task, dict(dp(%d)))) {\n", i+1);
		outstr (lbuf);
	    sprintf (lbuf, "\t    call %s\n", task_list[i].proc_name);
		outstr (lbuf);
	    sprintf (lbuf, "\t    return (OK)\n");
		outstr (lbuf);
	    sprintf (lbuf, "\t}\n");
		outstr (lbuf);
	}
}


/* OUTSTR -- Output a string.  Depending on the context, the string will
 * either go direct to the output file, or will be buffered in the output
 * buffer.
 */
void
outstr (char *string)
{
	register char *ip;


	if (context & (BODY|DATASTMT)) {
	    /* In body of procedure or in a data statement (which is output
	     * just preceding the body).
	     */
	    for (ip=string;  (*op++ = *ip++) != EOS;  )
		;
	    if (--op >= &obuf[SZ_OBUF]) {
		error (XPP_COMPERR, "Output buffer overflow");
		_exit (1);
	    }
	} else if (context & DECL) {
	    /* Output of a miscellaneous declaration in the declarations
	     * section.
	     */
	    for (ip=string;  (*dp++ = *ip++) != EOS;  )
		;
	    if (--dp >= &dbuf[SZ_DBUF]) {
		error (XPP_COMPERR, "Declarations buffer overflow");
		_exit (1);
	    }
	} else {
	    /* Outside of a procedure.
	     */
	    fputs (string, yyout);
	}
}


/* BEGIN_CODE -- Code that gets executed when the keyword BEGIN is encountered,
 * i.e., when we begin processing the executable part of a procedure
 * declaration.
 */
void
begin_code (void)
{
	char	text[1024];

	/* If we are already processing the body of a procedure, we probably
	 * have a missing END.
	 */
	if (context & BODY)
	    xpp_warn ("Unmatched BEGIN statement");

	/* Set context flag noting that we are processing the body of a 
	 * procedure.  Output the BEGIN statement, for the benefit of the
	 * second pass (RPP), which needs to know where the procedure body
	 * begins.
	 */
	setcontext (BODY);
	d_runtime (text);  outstr (text);
	outstr ("begin\n");
	linenum[istkptr]++;

	/* Initialization. */
	nbrace = 0;
	nswitch = 0;
	str_idnum = 1;
	errhand = NO;
	errchk = NO;
}


/* END_CODE -- Code that gets executed when the keyword END is encountered
 * in the input.  If error checking is used in the procedure, we must declare
 * the boolean function XERPOP.  If any switches are employed, we must declare
 * the switch variables.  Next we format and output data statements for any
 * strings encountered while processing the procedure body.  If the procedure
 * being processed is sys_runtask, the task name dictionary string is also
 * output.  Finally, we output the spooled procedure body, followed by and END
 * statement for the benefit of the second pass.
 */
void
end_code (void)
{
	int	i;

	/* If the END keyword is encountered outside of the body of a
	 * procedure, we leave it alone.
	 */
	if (!(context & BODY)) {
	    outstr (yytext);
	    return;
	}

	/* Output argument and local variable declarations (see decl.c).
	 * Note d_enter may have been called during processing of the body
	 * of a procedure to make entries in the symbol table for intrinsic
	 * functions, switch variables, etc. (this is not currently done).
	 */
	d_codegen (yyout);

	setcontext (GLOBAL);

	/* Output declarations for error checking and switches.  All variables
	 * and functions must be declared.
	 */
	if (errhand)
	    fprintf (yyout, "x$bool xerpop\n");
	if (errchk)
	    fprintf (yyout, "errchk error, erract\n");
	errhand = NO;
	errchk = NO;

	if (nswitch) {			/* declare switch variables */
	    fprintf (yyout, "%s\t", type_decl[XTY_INT]);
	    for (i=1;  i < nswitch;  i++)
		fprintf (yyout, "SW%04d,", i);
	    fprintf (yyout, "SW%04d\n", i);
	}

	/* Output any miscellaneous declarations.  These include ERRCHK and
	 * COMMON declarations - anything not a std type declaration or a
	 * data statement declaration.
	 */
	*dp++ = EOS;
	fputs (dbuf, yyout); fflush (yyout);
{ int i; for (i=0; i < SZ_DBUF; ) dbuf[i++] = '\0'; }
	dp = dbuf;

	/* Output the SAVE statement, which must come after all declarations
	 * and before any DATA statements.
	 */
	fputs ("save\n", yyout);

	/* Output data statements to initialize character strings, followed
	 * by any runtime procedure entry initialization statments, followed
	 * by the spooled text in the output buffer, followed by the END.
	 * Clear the string and output buffers.  Any user data statements
	 * will already have been moved into the output buffer, and they
	 * will come out at the end of the declarations section regardless
	 * of where they were given in the declarations section.  Data stmts
	 * are not permitted in the procedure body.
	 */
	init_strings();
	*op++ = EOS;
	fputs (obuf, yyout); fflush (yyout);
{ int i; for (i=0; i < SZ_OBUF; ) obuf[i++] = '\0'; }
	fputs ("end\n", yyout); fflush (yyout);

	op = obuf;
	*op = EOS;
	sp = sbuf;

	if (nbrace != 0) {
	    error (XPP_SYNTAX, "Unmatched brace");
	    nbrace = 0;
	}
}


#define BIG_STRING	9
#define NPERLINE	8

/* INIT_STRINGS -- Output data statements to initialize all strings in a
 * procedure ("string" declarations, inline strings, and the runtask
 * dictionary).  Strings are implemented as integer arrays, using the
 * smallest integer datatype provided by the host Fortran compiler, usually
 * INTEGER*2 (XTY_CHAR).
 */
void
init_strings (void)
{
	register int str;

	if (nstrings)
	    for (str=0;  str < nstrings && !strloopdecl;  str++)
		if (string_list[str].str_length >= BIG_STRING) {
		    fprintf (yyout, "%s\tiyy\n", type_decl[XTY_INT]);
		    strloopdecl++;
		}

	for (str=0;  str < nstrings;  str++)
	    write_string_data_statement (&string_list[str]);

	sp = sbuf;			/* clear string buffer		*/
	nstrings = 0;
	strloopdecl = 0;
}


/* WRITE_STRING_DATA_STATEMENT -- Output data statement to initialize a single
 * string.  If short string, output a simple whole-array data statement
 * that fits all on one line.  Large strings are initialized with multiple
 * data statements, each of which initializes a section of the string
 * using a dummy subscript.  This is thought to be more portable than
 * a single large data statement with continuation, because the number of
 * continuation cards permitted in a data statement depends on the compiler.
 * The loop variable in an implied do loop in a data statement must be declared
 * on some compilers (crazy but true).  Determine if we will be generating any
 * implied dos and declare the variable if so.
 */
void
write_string_data_statement (struct string *s)
{
	register int i, len;
	register char *ip;
	char    ch, *name;
	int	j;

	name = s->str_name;
	ip = s->str_text;
	len = s->str_length;

	if (len < BIG_STRING) {
	    fprintf (yyout, "data\t%s\t/", name);
	    for (i=0;  i < len-1;  i++) {
		if ((ch = *ip++) == EOS)
		    fprintf (yyout, "%3d,", XEOS);
		else
		    fprintf (yyout, "%3d,", ch);
	    }
	    fprintf (yyout, "%2d/\n", XEOS);

	} else {
	    for (j = 0;  j < len;  j += NPERLINE) {
		fprintf (yyout, "data\t(%s(iyy),iyy=%2d,%2d)\t/",
		    name, j+1, min(j+NPERLINE, len));
		for (i=j;  i < j+NPERLINE;  i++) {
		    if (i >= len-1) {
			fprintf (yyout, "%2d/\n", XEOS);
			return;
		    } else if (i == j+NPERLINE-1) {
			fprintf (yyout, "%3d/\n", ip[i]==EOS ? XEOS: ip[i]);
		    } else
			fprintf (yyout, "%3d,", ip[i]==EOS ? XEOS: ip[i]);
		}
	    }
	}
}


/* DO_STRING -- Process a STRING declaration or inline string.  Add a new
 * string descriptor to the string list, copy text of string into sbuf,
 * save name of string array in sbuf.  If inline string, manufacture the
 * name of the string array.
 */
void
do_string (
  char	delim,				/* char which delimits string	*/
  int	strtype 			/* string type			*/
)
{
	register char ch, *ip;
	register struct string *s;
	int	readstr = 1;
	char    *str_uniqid();

	/* If we run out of space for string storage, print error message,
	 * dump string decls out early, clear buffer and continue processing.
	 */
	if (nstrings >= MAX_STRINGS) {
	    error (XPP_COMPERR, "Too many strings in procedure");
	    init_strings();
	}

	s = &string_list[nstrings];

	switch (strtype) {

	case STR_INLINE:
	case STR_DEFINE:
	    /* Inline strings are implemented as Fortran arrays; generate a
	     * dummy name for the array and set up the descriptor.
	     * Defined strings are inline strings, but the name of the text of
	     * the string is already in yytext when we are called.
	     */
	    s->str_name = sp;
	    for (ip = str_uniqid();  (*sp++ = *ip++) != EOS;  )
		;
	    sbuf_check();
	    break;

	case STR_DECL:
	    /* String declaration.  Read in name of string, used as name of
	     * Fortran array.
	     */
	    ch = nextch();			/* skip whitespace	*/
	    if (!isalpha (ch))
		goto sterr;
	    s->str_name = sp;
	    *sp++ = ch;

	    /* Get rest of string name identifier. */
	    while ((ch = yy_input()) != EOF) {
		if (isalnum(ch) || ch == '_') {
		    *sp++ = ch;
	    	    sbuf_check();
		} else if (ch == '\n') {
sterr:		    error (XPP_SYNTAX, "String declaration syntax");
		    while (yy_input() != '\n')
			;
		    yy_unput ('\n');
		    return;
		} else {
		    *sp++ = EOS;
		    break;
		}
	    }

	    /* Advance to the ' or " string delimiter, in preparation for
	     * processing the string itself.  If syntax error occurs, skip
	     * to newline to avoid spurious error messages.  If the string
	     * is not quoted the string value field is taken to be the name
	     * of a string DEFINE.
	     */
	    delim = nextch();

	    if (!(delim == '"' || delim == '\'')) {
		register char *ip, *op;
		int	ch;
		char	*str_fetch();

		/* Fetch name of defined macro into yytext.
		 */
		op = yytext;
		*op++ = delim;
		while ((ch = yy_input()) != EOF)
		    if (isalnum(ch) || ch == '_')
			*op++ = ch;
		    else
			break;
		yy_unput (ch);
		*op = EOS;

		/* Fetch body of string into yytext.
		 */
	    	if ((ip = str_fetch (yytext)) != NULL) {
		    yyleng = 0;
		    for (op=yytext;  (*op++ = *ip++) != EOS;  )
			yyleng++;
		    readstr = 0;
		} else {
		    error (XPP_SYNTAX,
			"Undefined macro referenced in string declaration");
		}
	    }

	    break;
	}

	/* Get the text of the string.  Process escape sequences.  String may
	 * not span multiple lines.  In the case of a defined string, the text
	 * of the string will already be in yytext.
	 */
	s->str_text = sp;
	if (readstr && strtype != STR_DEFINE)
	    traverse (delim);		    /* process string into yytext */
	strcpy (sp, yytext);
	sp += yyleng + 1;
	s->str_length = yyleng + 1;
	sbuf_check();
	
	/* Output array declaration for string.  We want the declaration to
	 * go into the miscellaneous declarations buffer, so toggle the
	 * the context to DECL before calling OUTSTR.
	 */
	{
	    char lbuf[SZ_LINE];

	    pushcontext (DECL);
	    sprintf (lbuf, "%s\t%s(%d)\n", type_decl[XTY_CHAR], s->str_name,
		s->str_length);
	    outstr (lbuf);
	    popcontext();
	}
	
	/* If inline string, replace the quoted string by the name of the
	 * string variable.  This text goes into the output buffer, rather
	 * than directly to the output file as is the case with the declaration
	 * above.
	 */
	if (strtype == STR_INLINE || strtype == STR_DEFINE)
	    outstr (s->str_name);

	if (++nstrings >= MAX_STRINGS)
	    error (XPP_COMPERR, "Too many strings in procedure");
}


/* DO_HOLLERITH -- Process and output a Fortran string.  If the output
 * compiler is Fortran 77, we output a quoted string; otherwise we output
 * a hollerith string.  Fortran (packed) strings appear in the SPP source
 * as in the statement 'call_f77_sub (arg, *"any string", arg)'.  Escape
 * sequences are not recognized.
 */
void
do_hollerith (void)
{
	register char *op;
	char	strbuf[SZ_LINE], outbuf[SZ_LINE];
	int	len;

	/* Read the string into strbuf. */
	for (op=strbuf, len=0;  (*op = yy_input()) != '"';  op++, len++)
	    if (*op == '\n' || *op == EOF)
		break;
	if (*op == '\n')
	    error (XPP_COMPERR, "Packed string not delimited");
	else
	    *op = EOS;				/* delete delimiter */

#ifdef F77
	sprintf (outbuf, "\'%s\'", strbuf);
#else
	sprintf (outbuf, "%dH%s", i, strbuf);
#endif

	outstr (outbuf);
}


/* SBUF_CHECK -- Check to see that the string buffer has not overflowed.
 * It is a fatal error if it does.
 */
void
sbuf_check (void)
{
	if (sp >= &sbuf[SZ_SBUF]) {
	    error (XPP_COMPERR, "String buffer overflow");
	    _exit (1);
	}
}


/* STR_UNIQID -- Generate a unit identifier name for an inline string.
 */
char *
str_uniqid (void)
{
	static  char id[] = "ST0000";

	sprintf (&id[2], "%04d", str_idnum++);
	return (id);
}


/* TRAVERSE -- Called by the lexical analyzer when a quoted string has
 * been recognized.  Characters are input and deposited in yytext (the
 * lexical analyzer token buffer) until the trailing quote is seen.
 * Strings may not span lines unless the newline is delimited.  The
 * recognized escape sequences are converted upon input; all others are
 * left alone, presumably to later be converted by other code.
 * Quotes may be included in the string by escaping them, or by means of
 * the double quote convention.
 */
void
traverse (char delim)
{
	register char *op, *cp, ch;
	char	*index();


	for (op=yytext;  (*op = yy_input()) != EOF;  op++) {
	    if (*op == delim) {
		if ((*op = yy_input()) == EOF)
		    break;
		if (*op == delim)
		    continue;		/* double quote convention; keep one */
		else {
		    yy_unput (*op);
		    break;			/* normal exit		*/
		}

	    } else if (*op == '\n') {		/* error recovery exit	*/
		yy_unput ('\n');
		xpp_warn ("Newline while processing string");
		break;

	    } else if (*op == '\\') {
		if ((*op = yy_input()) == EOF) {
		    break;
		} else if (*op == '\n') {
		    --op;			/* explicit continuation */
		    continue;
		} else if ((cp = index (esc_ch, *op)) != NULL) {
		    *op = esc_val[cp-esc_ch];
		} else if (isdigit (*op)) {	/* '\0DD' octal constant */
		    *op -= '0';
		    while (isdigit (ch = yy_input()))
			*op = (*op * 8) + (ch - '0');
		    yy_unput (ch);
		} else {
		    ch = *op;			/* unknown escape sequence, */
		    *op++ = '\\';		/* leave it alone.	    */
		    *op = ch;
		}
	    }
	}

	*op = EOS;
	yyleng = (op - yytext);
}


/* ERROR -- Output an error message and set exit flag so that no linking occurs.
 * Do not abort compiler, however, because it is better to keep going and
 * find all the errors in a single compilation.
 */
void
error (int errcode, char *errmsg)
{
	fprintf (stderr, "Error on line %d of %s: %s\n", linenum[istkptr],
	    fname[istkptr], errmsg);
	fflush (stderr);
	errflag |= errcode;
}


/* WARN -- Output a warning message.  Do not set exit flag since this is only
 * a warning message; linking should occur if there are not any more serious
 * errors.
 */
void
xpp_warn (char *warnmsg)
{
	fprintf (stderr, "Warning on line %d of %s: %s\n", linenum[istkptr],
	    fname[istkptr], warnmsg);
	fflush (stderr);
}


/* ACCUM -- Code for conversion of numeric constants to decimal.  Convert a
 * character string to a binary integer constant, doing the conversion in the
 * indicated base.
 */
long
accum (int base, char **strp)
{
	register char *ip;
	long    sum;
	char    digit;

	sum = 0;
	ip = *strp;

	switch (base) {
	case OCTAL:
	case DECIMAL:
	    for (digit = *ip++;  isdigit (digit);  digit = *ip++)
		sum = sum * base + (digit - '0');
	    *strp = ip - 1;
	    break;
	case HEX:
	    while ((digit = *ip++) != EOF) {
		if (isdigit (digit))
		   sum = sum * base + (digit - '0');
		else if (digit >= 'a' && digit <= 'f')
		   sum = sum * base + (digit - 'a' + 10);
		else if (digit >= 'A' && digit <= 'F')
		   sum = sum * base + (digit - 'A' + 10);
		else {
		    *strp = ip;
		    break;
		}
	    }
	    break;
	default:
	    error (XPP_COMPERR, "Accum: unknown numeric base");
	    return (ERR);
	}

	return (sum);
}


/* CHARCON -- Convert a character constant to a binary integer value.
 * The regular escape sequences are recognized; numeric values are assumed
 * to be octal.
 */
int
charcon (char *string)
{
	register char *ip, ch;
	char	*cc, *index();
	char    *nump;

	ip = string + 1;		/* skip leading apostrophe	*/
	ch = *ip++;

	/* Handle '\c' and '\0dd' notations.
	 */
	if (ch == '\\') {
	    if ((cc = index (esc_ch, *ip)) != NULL) {
		return (esc_val[cc-esc_ch]);
	    } else if (isdigit (*ip)) {
		nump = ip;
		return (accum (OCTAL, &nump));
	    } else
		return (ch);
	} else {
	    /* Regular characters, i.e., 'c'; just return ASCII value of char.
	     */
	    return (ch);
	}
}


/* INT_CONSTANT -- Called to decode an integer constant, i.e., a decimal, hex,
 * octal, or sexagesimal number, or a character constant.  The numeric string
 * is converted in the indicated base and replaced by its decimal value.
 */
void
int_constant (char *string, int base)
{
	char    decimal_constant[SZ_NUMBUF], *p;
	long    accum(), value;
	int     i;

	p = string;
	i = strlen (string);

	switch (base) {
	case DECIMAL:
	    value = accum (10, &p);
	    break;
	case SEXAG:
	    value = accum (10, &p);
	    break;
	case OCTAL:
	    value = accum (8, &p);
	    break;
	case HEX:
	    value = accum (16, &p);
	    break;

	case CHARCON:
	    while ((p[i] = yy_input()) != EOF) {
		if (p[i] == '\n') {
	    	    error (XPP_SYNTAX, "Undelimited character constant");
	    	    return;
		} else if (p[i] == '\\') {
	    	    p[++i] = yy_input();
	    	    i++;
	    	    continue;
		} else if (p[i] == '\'')
	    	    break;
		i += 1;
	    }
	    value = charcon (p);
	    break;

	default:
	    error (XPP_COMPERR, "Unknown numeric base for integer conversion");
	    value = ERR;
	}

	/* Output the decimal value of the integer constant.  We are simply
	 * replacing the SPP constant by a decimal constant.
	 */
	sprintf (decimal_constant, "%ld", value);
	outstr (decimal_constant);
}


/* HMS -- Convert number in HMS format into a decimal constant, and output
 * in that form.  Successive : separated fields are scaled to 1/60 th of
 * the preceeding field.  Thus "12:30" is equivalent to "12.5".  Some care
 * is taken to preserve the precision of the number.
 */
void
hms (char *number)
{
	char	cvalue[SZ_NUMBUF], *ip;
	int	bvalue, ndigits;
	long	scale = 10000000;
	long	units = 1;
	long	value = 0;

	for (ndigits=0, ip=number;  *ip;  ip++)
	    if (isdigit (*ip))
		ndigits++;

	/* Get the unscaled base value part of the number. */
	ip = number;
	bvalue = accum (DECIMAL, &ip);

	/* Convert any sexagesimal encoded fields.  */
	while (*ip == ':') {
	    ip++;
	    units *= 60;
	    value += (accum (DECIMAL, &ip) * scale / units);
	}

	/* Convert the fractional part of the number, if any.
	 */
	if (*ip++ == '.')
	    while (isdigit (*ip)) {
		units *= 10;
		value += (*ip++ - '0') * scale / units;
	    }

	/* Format the output number. */
	if (ndigits > MIN_REALPREC)
	    sprintf (cvalue, "%d.%ldD0", bvalue, value);
	else
	    sprintf (cvalue, "%d.%ld", bvalue, value);
	cvalue[ndigits+1] = '\0';

	/* Print the translated number. */
	outstr (cvalue);
}


/*
 *  Revision history (when i remembered) --
 *
 * 14-Dec-82:	Changed hms conversion, to produce degrees or hours,
 *		rather than seconds (lex pattern, add hms, delete ':'
 *		action from accum).
 *
 * 10-Mar-83	Broke C code and Lex code into separate files.
 *		Added support for error handling.
 *		Added additional type coercion functions.
 *
 * 20-Mar-83	Modified processing of TASK stmt to use file inclusion
 *		to read the RUNTASK file, making it possible to maintain
 *		the IRAF main as a .x file, rather than as a .r file.
 *
 * Dec-83	Fixed bug in processing of TASK stmt which prevented
 *		compilation of processes with many tasks.  Added many
 *		comments and cleaned up the code a bit.
 */
