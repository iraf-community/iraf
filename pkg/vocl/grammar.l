comment		"#"

D		[0-9]
H		[0-9a-fA-F]
A		[a-zA-Z]

%%

[ \t]+		/* groups of blanks and tabs, while significant as delimiters,
		 * are otherwise ignored.
		 */ ;

","[ \t]*\n	{	/* trailing ',' implies continuation */
			return (',');
		}

"\\"[ \t]*\n	{	/* trailing '\' completely absorbed */
		}
^[ \t]*"!".*	{
			/* Host os command escape.  Remove everything up through
			 * '!'.  Let clsystem decide what to do with null cmd.
			 * Must precede the "!" YOP_NOT spec in this file.
			 */
			register char *cp;
			for (cp = yytext;  *cp++ != '!';  )
			    ;
			yylval = addconst (cp, OT_STRING);
			return (Y_OSESC);
		}


"|&"		return (Y_ALLPIPE);	/* pipe all, even stderr	   */
">>"		return (Y_APPEND);	/* append all but stderr	   */
">>&"		return (Y_ALLAPPEND);	/* append all, even stderr	   */
">&"		return (Y_ALLREDIR);	/* redirect all, even stderr	   */
(">"|">>")("G"|"I"|"P")+ {
			yylval = addconst (yytext, OT_STRING);
			return (Y_GSREDIR);
		}

"<="		return (YOP_LE);	/* operators...			   */
">="		return (YOP_GE);
"=="		return (YOP_EQ);
"!="		return (YOP_NE);
"**"		return (YOP_POW);
"||"		return (YOP_OR);
"&&"		return (YOP_AND);
"!"		return (YOP_NOT);
"+="		return (YOP_AOADD);
"-="		return (YOP_AOSUB);
"*="		return (YOP_AOMUL);
"/="		return (YOP_AODIV);
"//="		return (YOP_AOCAT);
"//"		return (YOP_CONCAT);

"}"		{ if (dobrace) {
			dobrace = NO;
			return (*yytext);
		  } else {
			dobrace = YES;
			unput (*yytext);
			return (';');
		  }
		}


"^"		return (*yytext);	/* debug: print stack		     */
"/"		return (*yytext);	/* debug: single step		     */

"?"		return (crackident (yytext));	/* current package help	     */
"??"		return (crackident (yytext));	/* all tasks help	     */

"&"		{	extern bracelevel;
			if (bracelevel) {
	    eprintf ("ERROR: background not allowed within statement block\n");
			    return ('#');
			} else {
			    yyleng = 0;
			    while ((yytext[yyleng]=input()) != '\n')
				yyleng++;
			    yytext[yyleng] = '\0';
			    bkg_init (yytext);
			    return (Y_NEWLINE);
			}
		}

({A}|"$"|"_")({A}|"$"|{D}|"_"|".")*	{
			/* crackident() sets yylval and returns token value.
			 */
			return (crackident (yytext));
		}

{D}+(([bB])|({H}*[xX]))?	{
			/* must precede OT_REAL as integers also match there */
			yylval = addconst (yytext, OT_INT);
			return (Y_CONSTANT);
		}
(({D}+)|(({D}*"."{D}+)|({D}+"."{D}*)))([eEdD][+-]?{D}+)? {
			yylval = addconst (yytext, OT_REAL);
		 	return (Y_CONSTANT);
		}

{D}+":"{D}+(":"{D}*("."{D}*)?)? {
			/* sexagesimal format */
			yylval = addconst (yytext, OT_REAL);
			return (Y_CONSTANT);
		}

(\")|(\')	{	/* Quoted string.  call traverse() to read the
			 * string into yytext.
			 */
			traverse (*yytext);
			yylval = addconst (yytext, OT_STRING);
			return (Y_CONSTANT);
		}

\n		return (Y_NEWLINE);

{comment}	{	/* Ignore a comment. */
			while (input() != '\n')
			    ;
			unput ('\n');
		}

.		return (*yytext);

%%

#include "errs.h"

/* See gram.c for the various support functions, such as addconst()
 * and crackident().  Traverse is included here since it directly
 * references input, unput, yytext, etc.
 */

/* TRAVERSE -- Called by the lexical analyzer when a quoted string has
 * been recognized.  Characters are input and deposited in yytext (the
 * lexical analyzer token buffer) until the trailing quote is seen.
 * Strings may not span lines unless the newline is delimited.  The
 * recognized escape sequences are converted upon input; all others are
 * left alone, presumably to later be converted by other code.
 * Quotes may be included in the string by escaping them, or by means of
 * the double quote convention.
 */
traverse (delim)
char	delim;
{
	register char *op, *cp, ch;
	static	char *esc_ch  = "ntfr\\\"'";
	static	char *esc_val = "\n\t\f\r\\\"\'";
	char	*index();

	for (op=yytext;  (*op = input()) != EOF;  op++) {
	    if (*op == delim) {
		if ((*op = input()) == EOF)
		    break;
		if (*op == delim)
		    continue;	/* double quote convention; keep one */
		else {
		    unput (*op);
		    break;			/* normal exit	*/
		}

	    } else if (*op == '\n') {		/* error recovery exit	*/
		*op = '\0';
		cl_error (E_UERR, "Newline while processing string");
		break;

	    } else if (*op == '\\') {
		if ((*op = input()) == EOF) {
		    break;
		} else if (*op == '\n') {
		    --op;			/* explicit continuation */
		    while ((ch = input()) && isspace(ch) || ch == '#') {
			if (ch == '#')
			    while ((ch = input()) && ch != '\n')
				;
		    }
		    unput (ch);
		    continue;
		} else if ((cp = index (esc_ch, *op)) != NULL) {
		    *op = esc_val[cp-esc_ch];
		} else if (isdigit (*op)) {	/* '\0DD' octal constant   */
		    *op -= '0';
		    while (isdigit (ch = input()))
			*op = (*op * 8) + (ch - '0');
		    unput (ch);
		} else {
		    ch = *op;			/* unknown escape sequence, */
		    *op++ = '\\';		/* leave it alone.	    */
		    *op = ch;
		}
	    }
	}

	*op = '\0';
	yyleng = (op - yytext);
}
