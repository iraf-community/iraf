%{

#include <ctype.h>

/*
 * GENERIC -- This filter takes a file containing a generic operator as input
 * and generates as output either a set of files, one for each of the data
 * types in the generic family, or a single file wherein the generic section
 * has been duplicated for each case.
 */

#undef	output
extern	char	*type_string;
extern	char	xtype_string[];
extern	char	type_char;

%}

W	[ \t]

%%

PIXEL				outstr (type_string);
XPIXEL				outstr (xtype_string);
INDEF				output_indef (type_char);
INDEF(S|I|L|R|D|X)		ECHO;
SZ_PIXEL			output_upper ("SZ_");
TY_PIXEL			output_upper ("TY_");
$PIXEL				outstr ("PIXEL");
$INDEF				outstr ("INDEF");

[A-Z][A-Z_]*PIXEL		{
					yytext[strlen(yytext)-5] = '\0';
					output_upper (yytext);
				}

"$t"				{	if (isupper (type_char))
					    output (tolower (type_char));
					else
					    output (type_char);
				}
"$T"				{	if (islower (type_char))
					    output (toupper (type_char));
					else
					    output (type_char);
				}

"$/"				pass_through();
[0-9]+("$f"|"$F")		make_float (type_char);

{W}*"$if"			do_if();
{W}*"$else"			do_else();
{W}*"$endif"			do_endif(); 
{W}*"$for"			do_for(); 
{W}*"$endfor"			do_endfor(); 
{W}*"$IF"			do_if();
{W}*"$ELSE"			do_else();
{W}*"$ENDIF"			do_endif(); 
{W}*"$FOR"			do_for(); 
{W}*"$ENDFOR"			do_endfor(); 

"$$"				output ('$');
"/*"				copy_comment();
\"				copy_string();

^\#if				ECHO;
^\#else				ECHO;
^\#endif			ECHO;
^\#include			ECHO;

\#				copy_line();
^\%				copy_line();

%%


/* LEX_INPUT -- Make input() callable as a function from the .c code.
 */
lex_input()
{
	return (input());
}


/* LEX_UNPUT -- Make unput() callable as a function from the .c code.
 */
lex_unput (ch)
int	ch;
{
	unput (ch);
}
