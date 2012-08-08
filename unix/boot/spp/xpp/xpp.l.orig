%{

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

extern	int linenum[];			/* line numbers in files	*/
extern	int istkptr;	 		/* istk pointer			*/
extern	int str_idnum;			/* for ST0000 string names	*/
extern	int nbrace;			/* count of braces		*/
extern	int nswitch;			/* number of "switch" stmts	*/
extern	int errflag;			/* set if compiler error	*/
extern	int errchk;			/* sef if error checking	*/
extern	int context;			/* lexical context flags	*/
static	int dtype;			/* set if typed procedure	*/

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
^{W}*"define"{W}+[A-Z0-9_]+{W}+\"	{
				    str_enter();
				}
^{W}*("(")?"define"/{NI}	{
				    pushcontext (DEFSTMT);
				    ECHO;
				}
^{W}*"end"/{NI}			{
				    end_code();
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
typespec (typecode)
int	typecode;
{
	if (context & DECL)
	    d_declaration (typecode);
	else if (context & (BODY|DEFSTMT))
	    mapident();
	else
	    dtype = typecode;
}
