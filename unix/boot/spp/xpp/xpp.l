%{

#include <stdio.h>
#include <ctype.h>
#include "xpp.h"
#include "../../bootProto.h"
#include "xppProto.h"

#define import_spp
#include <iraf.h>


#include "xpp.h"

/*
 * Lexical definition for the first pass of the IRAF subset preprocessor.
 * This program is a horrible kludge but will suffice until there is time
 * to build something better.
 */

#undef	output		/* undefine LEX output macro -- we use proc	*/
#undef	ECHO		/* ditto echo					*/
#define ECHO		outstr (yytext)

#define	OCTAL		8
#define	HEX		16
#define CHARCON		1

#ifdef YYLMAX
#undef YYLMAX
#endif
#define YYLMAX		YY_BUF_SIZE

YY_BUFFER_STATE include_stack[MAX_INCLUDE];


extern  FILE *istk[];
extern  char fname[MAX_INCLUDE][SZ_PATHNAME];
extern  char *machdefs[];
extern  int hbindefs, foreigndefs;

extern	int linenum[];			/* line numbers in files	*/
extern	int istkptr;	 		/* istk pointer			*/
extern	int str_idnum;			/* for ST0000 string names	*/
extern	int nbrace;			/* count of braces		*/
extern	int nswitch;			/* number of "switch" stmts	*/
extern	int errflag;			/* set if compiler error	*/
extern	int errchk;			/* sef if error checking	*/
extern	int context;			/* lexical context flags	*/
extern  int ntasks;
static	int dtype;			/* set if typed procedure	*/

extern  char *vfn2osfn(char *vfn, int new);
extern  void skipnl (void);


void  typespec (int typecode);
void  process_task_statement (void);

void  do_include (void);
int   yywrap (void);
int   yy_input (void);
void  yy_unput (int ch);


%}

D	[0-9]
O	[0-7]
S	[ 0-6]{D}
X	[0-9A-F]
W	[ \t]
NI	[^a-zA-Z0-9_]

%a 5000
%o 9000
%k 500

%%

^"bool"/{NI}			typespec (XTY_BOOL);
^"char"/{NI}			typespec (XTY_CHAR);
^"short"/{NI}			typespec (XTY_SHORT);
^"int"/{NI}			typespec (XTY_INT);
^"long"/{NI}			typespec (XTY_LONG);
^"real"/{NI}			typespec (XTY_REAL);
^"double"/{NI}			typespec (XTY_DOUBLE);
^"complex"/{NI}			typespec (XTY_COMPLEX);
^"pointer"/{NI}			typespec (XTY_POINTER);
^"extern"/{NI}			typespec (XTY_EXTERN);

^{W}*"procedure"/{NI}		{
				    /* Subroutine declaration. */
				    pushcontext (PROCSTMT);
				    d_gettok (yytext, YYLMAX-1);
				    d_newproc (yytext, 0);
				}

"procedure"/{NI}		{
				    /* Function declaration. */
				    pushcontext (PROCSTMT);
				    d_gettok (yytext, YYLMAX-1);
				    d_newproc (yytext, dtype);
				    setline();
				}

^{W}*"task"/{NI}		{   if (context & BODY)
					ECHO;
				    else {
					process_task_statement();
					setline();
				    }
				}
^{W}*"TN$DECL"			    put_dictionary();
^{W}*"TN$INTERP"		    put_interpreter();
^".""help"			{
				    skip_helpblock(); 
				    setline();
				}
^{W}*"begin"/{NI}		{
				    begin_code();
				    setline();
				}
^{W}*"define"{W}+[A-Z0-9_]+{W}+Memr {
				    macro_redef();
				    setline();
				}
^{W}*"define"{W}+[A-Z0-9_]+{W}+\"	{
				    str_enter();
				}
^{W}*("(")?"define"/{NI}	{
				    pushcontext (DEFSTMT);
				    ECHO;
				}
^{W}*"end"/{NI}			{
				    end_code();
				    setline();
				}
^{W}*"string"/{NI}		{
				    (context & BODY) ? ECHO
					: do_string ('"', STR_DECL);
				}
^{W}*"data"/{NI}		{
				    if (!(context & BODY))
					pushcontext (DATASTMT);
				    ECHO;
				}

"switch"/{NI}			{
				    ECHO;
				    if (context & BODY)
					nswitch++;
				}

"#"				    skipnl();
^"%"[^\n]*			    ECHO;

^{W}*"include"{W}*(\"|<)	    do_include();

[a-zA-Z][a-zA-Z0-9_$]* 		    mapident();

{D}+":"{S}(":"{S})?("."{D}*)?	    hms (yytext);
{O}+("B"|"b") 			    int_constant (yytext, OCTAL);
{X}+("X"|"x") 			    int_constant (yytext, HEX);
\'				    int_constant (yytext, CHARCON);

"()"				{
				    if (context & (BODY|PROCSTMT))
					ECHO;
				}

"&&"				    output ('&');
"||"				    output ('|');

"{"				{
				    ECHO;
				    nbrace++;
				}
"}"				{
				    ECHO;
				    nbrace--;
				}
"["				    output ('(');
"]"				    output (')');

\*\"				    do_hollerith();

\"				{
				    if (context & BODY)
					do_string ('"', STR_INLINE);
				    else
					ECHO; 
				}

(","|";"){W}*("#"[^\n]*)?"\n"	{
				    /* If statement is continued do not pop
				     * the context.
				     */
				    ECHO;
				    linenum[istkptr]++;
				}

"\n"				{
				    /* End of newline and end of statement.
				     */
				    ECHO;
				    linenum[istkptr]++;
				    popcontext();
				}

%%


/* TYPESPEC -- Context dependent processing of a type specifier.  If in the
 * declarations section, process a declarations statement.  If in procedure
 * body or in a define statement, map the type specifier identifer and output
 * the mapped value (intrinsic function name).  Otherwise we must be in global
 * space, and the type spec begins a function declaration; save the datatype
 * code for d_newproc().
 */
void
typespec (int	typecode)
{
	if (context & DECL)
	    d_declaration (typecode);
	else if (context & (BODY|DEFSTMT))
	    mapident();
	else
	    dtype = typecode;
}



/* PROCESS_TASK_STATEMENT -- Parse the TASK statement.  The task statement
 * is replaced by the "sys_runtask" procedure (sysruk), which is called by
 * the IRAF main to run a task, or to print the dictionary (cmd "?").
 * The source for the basic sys_runtask procedure is in "lib$sysruk.x".
 * We process the task statement into some internal tables, then open the
 * sysruk.x file as an include file.  Special macros therein are
 * replaced by the taskname dictionary as processing continues.
 */
void
process_task_statement(void)
{
        char    ch;

        if (ntasks > 0) {               /* only one task statement permitted */
            error (XPP_SYNTAX, "Only one TASK statement permitted per file");
            return;
        }

        /* Process the task statement into the TASK_LIST structure.
         */
        if (parse_task_statement() == ERR) {
            error (XPP_SYNTAX, "Syntax error in TASK statement");
            while ((ch = input()) != EOF && ch != '\n')
                ;
            unput ('\n');
            return;
        }

        /* Open RUNTASK ("lib$sysruk.x") as an include file.
         */
        istk[istkptr] = yyin;
        if (++istkptr >= MAX_INCLUDE) {
            istkptr--;
            error (XPP_COMPERR, "Maximum include nesting exceeded");
            return;
        }

        strcpy (fname[istkptr], IRAFLIB);
        strcat (fname[istkptr], RUNTASK);
        if ((yyin = fopen (vfn2osfn (fname[istkptr],0), "r")) == NULL) {
            yyin = istk[--istkptr];
            error (XPP_SYNTAX, "Cannot read lib$sysruk.x");
            return;
        }

        linenum[istkptr] = 1;

        /* Put the newline back so that LEX "^..." matches will work on
         * first line of the include file.
         */
        unput ('\n');

	yypush_buffer_state(yy_create_buffer( yyin, YY_BUF_SIZE ));
        BEGIN(INITIAL);
}


/* DO_INCLUDE -- Process an include statement, i.e., eat up the include
 * statement, push the current input file on a stack, and open the new file.
 * System include files are referenced as "<file>", other files as "file".
 */
void
do_include(void)
{
	char    *p, delim;
	char    hfile[SZ_FNAME+1], *op;
	int	root_len;


	/* Push current input file status on the input file stack istk.
	 */
	istk[istkptr] = yyin;
	if (++istkptr >= MAX_INCLUDE) {
	    --istkptr;
	    error (XPP_COMPERR, "Maximum include nesting exceeded");
	    return;
	}

	/* If filespec "<file>", call os_sysfile to get the pathname of the
	 * system include file.
	 */
	if (yytext[yyleng-1] == '<') {

	    for (op=hfile;  (*op = input()) != EOF;  op++)
		if (*op == '\n') {
		    --istkptr;
		    error (XPP_SYNTAX, "missing > delim in include statement");
		    return;
		} else if (*op == '>')
		    break;

	    *op = EOS;

	    if (os_sysfile (hfile, fname[istkptr], SZ_PATHNAME) == ERR) {
		--istkptr;
		error (XPP_COMPERR, "cannot find include file");
		return;
	    }

	} else {
	    /* Prepend pathname leading to the file in which the current
	     * include statement was found.  Compiler may not have been run
	     * from the directory containing the source and include file.
	     */
	    if (!hbindefs) {
	        if ((p = strrchr (fname[istkptr-1], '/')) == NULL)
		    root_len = 0;
	        else
		    root_len = p - fname[istkptr-1] + 1;
	        strncpy (fname[istkptr], fname[istkptr-1], root_len);

	    } else {
	        if ((p = vfn2osfn (HBIN_INCLUDES, 0))) {
		    root_len = strlen (p);
	            strncpy (fname[istkptr], p, root_len);
	        } else {
		    --istkptr;
		    error (XPP_COMPERR, "cannot find hbin$ directory");
		    return;
	        }
	    }
	    fname[istkptr][root_len] = EOS;

	    delim = '"';

	    /* Advance to end of whatever is in the file name string.
	     */
	    for (p=fname[istkptr];  *p != EOS;  p++)
		;
	    /* Concatenate name of referenced file.
	     */
	    while ((*p = input()) != delim) {
		if (*p == '\n' || *p == EOF) {
		    --istkptr;
		    error (XPP_SYNTAX, "bad include file name");
		    return;
		}
		p++;
	    }
	    *p = EOS;
	}

	/* If the foreign defs option is in effect, the machine dependent defs
	 * for a foreign machine are given by a substitute "iraf.h" file named
	 * on the command line.  This foreign machine header file includes
	 * not only the iraf.h for the foreign machine, but the equivalent of
	 * all the files named in the array of strings "machdefs".  Ignore any
	 * attempts to include any of these files since they have already been
	 * included in the foreign definitions header file.
	 */
	if (foreigndefs) {
	    char	sysfile[SZ_PATHNAME];
	    char	**files;

	    /*
	    for (files=machdefs;  *files != NULL;  files++) {
	    */
	    for (files=machdefs;  **files;  files++) {
		memset (sysfile, 0, SZ_PATHNAME);
		strcpy (sysfile, HOSTLIB);
		strcat (sysfile, *files);
		if (strcmp (sysfile, fname[istkptr]) == 0) {
		    --istkptr;
		    return;
		}
	    }
	}

	if ((yyin = fopen (vfn2osfn(fname[istkptr],0), "r")) == NULL) {
	    yyin = istk[--istkptr];
	    error (XPP_SYNTAX, "Cannot open include file");
	    return;
	}

	/* Keep track of the line number within the include file.  */
	linenum[istkptr] = 1;

	/* Put the newline back so that LEX "^..." matches will work on
	 * first line of include file.
	 */
	unput ('\n');
	linenum[istkptr-1] -= 1;

	yypush_buffer_state(yy_create_buffer( yyin, YY_BUF_SIZE ));
        BEGIN(INITIAL);
}


/* YYWRAP -- Called by LEX when end of file is reached.  If input stack is
 * not empty, close off include file and continue on in old file.  Return 
 * nonzero when the stack is empty, i.e., when we reach the end of the
 * main file.
 */
int
yywrap(void)
{
	/* The last line of a file is not necessarily newline terminated.
	 * Output a newline just in case.
	 */
	fprintf (yyout, "\n");

	if (istkptr <= 0) {
	    /* ALL DONE with main file.
	     */
	    return (1);

	} else {
	    /* End of include file.  Pop old input file and set line number
	     * for error messages.
	     */
	    fclose (yyin);
	    /* yyin = istk[--istkptr]; */
	    istkptr--;

            yypop_buffer_state ();
	    if ( !YY_CURRENT_BUFFER )
	    	yyterminate ();

	    if (istkptr == 0)
		setline();
	    return (0);
	}
}



/* YY_INPUT -- Get a character from the input stream.
 */
int
yy_input (void)
{
	return (input());
}


/* YY_UNPUT -- Put a character back into the input stream.
 */
void
yy_unput (char	ch)
{
	unput(ch);
}
