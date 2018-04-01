/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define	import_libc
#define	import_ctype
#define	import_xnames
#define	import_lexnum
#include <iraf.h>

#include "proto.h"


extern	int	cldebug;

/*
 * NOTE: This file is #included in the parser and inherits the parser global
 * declarations.
 */

#define	LEXDEBUG	1
#define	newtoken	(yyleng==0)

int	_lexmodes;		/* nonzero enables mode switching	*/
int	lexdebug=0;		/* debug lexical analyzer		*/
int	lexcol=0;		/* nchars since \n or ;			*/
int	pbtoken;		/* push back token			*/
int	newarg;			/* whitespace argument delimiter seen	*/
int	lhs;			/* "left hand side" switch for []	*/

/* YYLEX -- Return the next token from the input stream.  Two separate lexical
 * analyzers are provided, the "command mode" lexical analyzer for interactive
 * command entry, and the "compute mode" analyzer for more sophisticated
 * applications.  The nesting level of parentheses and braces is used to switch
 * between the two modes.  When the paren level is nonzero compute mode is in
 * effect.  Mode switching may be defeated by setting the external variable
 * _lexmodes to zero.  A single parser accepts input from both lexical
 * analyzers.
 */
int 
yylex (void)
{
	register int	token;

	if (_lexmodes && parenlevel == 0 && bracelevel < PBRACE) {
	    while (!(token = lexicon()))
		if (yywrap())
		    break;
	} else
	    token = lex_yylex();

	if (!lexdebug)
	    return (token);

#if LEXDEBUG
	switch (token) {
	case Y_CONSTANT:
	    eprintf ("CONSTANT ");
	    fprop (stderr, reference (operand, yylval));
	    eprintf ("\n");
	    break;
	case Y_IDENT:
	    eprintf ("IDENT ");
	    fprop (stderr, reference (operand, yylval));
	    eprintf ("\n");
	    break;
	case Y_OSESC:
	    eprintf ("Y_OSESC ");
	    fprop (stderr, reference (operand, yylval));
	    eprintf ("\n");
	    break;
	case Y_APPEND:
	    eprintf ("Y_APPEND\n");
	    break;
	case Y_ALLAPPEND:
	    eprintf ("Y_ALLAPPEND\n");
	    break;
	case Y_ALLREDIR:
	    eprintf ("Y_ALLREDIR\n");
	    break;
	case Y_GSREDIR:
	    eprintf ("Y_GSREDIR\n");
	    break;
	case Y_ALLPIPE:
	    eprintf ("Y_ALLPIPE\n");
	    break;
	case Y_NEWLINE:
	    eprintf ("NEWLINE\n");
	    break;
	default:
	    eprintf ("`%c'\n", token);
	    break;
	}
#endif

	return (token);
}


/* LEXICON -- Simple "conversational mode" lexical analyser.  Lexical analysis
 * in the CL is carried out by a dual mode lexical analyser.  In conversational
 * mode there are few tokens and few special characters; arguments are
 * delimited by whitespace and may contain nonalphanumeric characters.  Few
 * strings have to be quoted.  In computational mode the arithmetic operators
 * are recognized and arguments must be delimited by commas.  Computational
 * mode is in effect whenever the parenlevel is nonzero.
 *
 * The two modes are implemented with two separate lexical analyzers.  Gettok
 * implements conversational mode, while computational mode is implemented with
 * a LEX finite state automaton.  Gettok recognizes the following special chars:
 *
 *	[ \t]				argument delimiter
 *	["']				string
 *	\n				newline
 *	\				single character escape
 *	!				os escape
 *	#				comment
 *	&				spawn background job
 *	(				lparen
 *	+				plus (switch)
 *	-				minus (switch)
 *	;				eost
 *	=				equals
 *	+=				add and set
 *	-=				subtract and set
 *	*=				multiply and set
 *	/=				divide and set
 *	<				redirin
 *	>				redir
 *	>&				allredir
 *	>>				append
 *	>>&				allappend
 *	>(G|I|P|)+			graphics stream redirection
 *	{				lbrace
 *	|				pipe
 *	|&				allpipe
 *	}				rbrace
 *	[				beginning of index list
 *	]				end of index list
 *
 * The history metacharacter ^ is processed before input is passed to the
 * lexical analyser.  Any sequence of nonwhite characters that does not form
 * one of the recognized tokens is returned as a string.
 */
int 
lexicon (void)
{
	char	*bkgerr = "ERROR: cannot submit background job inside {}\n";
	register int	ch, cch;
	register int	token;
	int	stringtok, identifier, setlevel;
	int	clswitch;
	char	*op, *index();

	/* Return pushed back token if any.
	 */
	if (pbtoken) {
	    token = pbtoken;
	    pbtoken = 0;
	    return (token);
	}

	/* Skip leading whitespace.  If whitespace is seen and we are in an
	 * argument list (according to the parser) set flag to output the
	 * comma argument delimiter if the next token begins an argument.
	 * If whitespace or = is seen (except whitespace at the beginning of
	 * a command) then set LHS to false, turning [] off as conversational
	 * mode metacharacters (they will be automatically turned on when
	 * compute mode is entered in an expression).
	 */
	while (ch = input())
	    if (ch == ' ' || ch == '\t') {
space:		if (lexcol > 0)
		    lhs = 0;
		if (inarglist)
		    newarg++;
	    } else if (ch == '\\') {
		if ((ch = input()) != '\n') {
		    unput (ch);
		    break;
		} else
		    goto space;
	    } else
		break;
	

	/* Start new token.
	 */
	if (ch) {
	    unput (ch);
	    yyleng = 0;
	    if (!inarglist)
		newarg = 0;
	} else
	    return (0);


	/* Identify and accumulate next token.  Simple tokens are returned as
	 * integer constants, more complex tokens as operand structures in
	 * yylval.
	 */
	while (ch = input()) {
	    lexcol++;

	    switch (ch) {
	    case '&':
		/* An ampersand triggers bkg execution in command mode, unless
		 * it occurs in a token such as >& or >>&, in which case we
		 * never get here.
		 */
		if (!newtoken) {
		    unput (ch);
		    goto tokout_;
		} else {
		    while (ch = input()) {
			if (ch == ' ' || ch == '\t')
			    continue;
			else {
			    char   bkgmsg[SZ_LINE+1];
			    int    n = SZ_LINE;

			    op = bkgmsg;
			    unput (ch);
			    if (bracelevel) {
				eprintf (bkgerr);
				return ('#');
			    }

			    while (--n >= 0 && (*op = input()) != '\n')
				op++;
			    *op = EOS;
			    bkg_init (bkgmsg);
			    return (Y_NEWLINE);
			}
		    }
		    return (0);
		}

	    case ';':
	    case '\n':
		lexcol = 0;
		lhs = 1;
		goto etok_;

	    case '\t':
	    case ' ':
		if (lexcol > 0)
		    lhs = 0;
		goto etok_;

	    case '[':
	    case ']':
		/* [] are recognized as command mode metacharacters only
		 * on the left hand side of an assignment statement.
		 */
		if (!lhs)
		    goto deposit_;
		/* Fall through */

	    case '{':
	    case '}':
		/* We want to distinguish here between the use of {} for
		 * the set selection operator in template strings, and the
		 * conventional compound statement operator.  The distinction
		 * is that { is recognized as a token only if occurs at the
		 * beginning of a token, and } is recognized as a separate
		 * token when inside a token only if it matches a { in the
		 * same token.  Hence, alpha{xxx} is a single token in command
		 * mode, whereas {xxx} is 3 tokens, the same as { xxx },
		 * and xxx} is the same as xxx }.  Usage is completely
		 * unambiguous if the { or } is preceded by a space.
		 */
		if (newtoken)
		    return (ch);
		if (stringtok) {
		    if (ch == '{')
			setlevel++;
		    else if (setlevel == 0)
			goto etok_;		/* } does not match { */
		    else
			--setlevel;
		    goto deposit_;
		}
		/* fall through */

	    case '=':
etok_:		if (!newtoken) {
		    unput (ch);
		    goto tokout_;
		} else if (ch == '\n') {
		    return (Y_NEWLINE);
		} else if (ch == '=') {
		    token = ch;
		    lhs = 0;
		    goto eatwhite_;
		} else
		    return (ch);

	    case '?':
		/* ?, ?? menu commands, recognized only at beginning of stmt */
		if (lexcol > 1) {
		    goto deposit_;
		} else if (ch = input()) {
		    if (ch == '?')
			return (crackident ("??"));
		    else {
			unput (ch);
			return (crackident ("?"));
		    }
		} else
		    return (0);

	    case '+':
	    case '-':
		/* Plus and minus are recognized as the switch operators for
		 * boolean parameters only if encountered while accumulating
		 * a token and if followed by an argument delimiter, i.e.,
		 * space, tab, newline, or semicolon.  If found at the beginning
		 * of a token they are returned as a separate token and will be
		 * interpreted by the parser as unary plus or minus.
		 */
		if (newtoken) {
		    if (newarg) {
			cch = input();
			if (cch == 0)
			    return (0);
			unput (cch);

			if (ch == '-' && isdigit (cch)) {
			    unput (ch);
			    newarg = 0;
			    return (',');
			} else {
			    /* Not number; treat +- as a string char.
			     */
			    goto deposit_;
			}

		    } else {
			cch = input();
			if (cch == 0)
			    return (0);

			if (cch == '=') {
			    if (ch == '+')
				return (YOP_AOADD);
			    else
				return (YOP_AOSUB);
			} else if (isdigit (cch)) {
			    unput (cch);
			    return (ch);
			} else {
			    unput (cch);
			    goto deposit_;
			}
		    }

		} else if (cch = input()) {
		    clswitch = (isspace (cch) || cch == ';');
		    if (cch == '=') {
			unput(cch);
			unput (ch);
			goto tokout_;
		    }
		    unput (cch);
		    if (clswitch) {
			pbtoken = ch;
			goto tokout_;
		    } else
			goto deposit_;
		} else
		    return (0);

	    case '"':
	    case '\'':
		if (!newtoken) {
		    unput (ch);
		    goto tokout_;
		} else if (newarg) {
		    unput (ch);
		    newarg = 0;
		    return (',');
		} else {
		    traverse (ch);
		    yylval = addconst (yytext, OT_STRING);
		    return (Y_CONSTANT);
		}

	    case '\\':
		if (ch = input()) {
		    if (ch == '\n')
			continue;
		    else if (index ("&;=+-\"'\\#><()|", ch) != NULL)
			goto deposit_;		/* put ch in string */
		    else
			goto escape_;		/* put \ch in string */
		} else
		    return (0);

	    case '!':
		/* OS escape is only recognized when the ! occurs as the first
		 * token in a statement.
		 */
		if (lexcol > 1)
		    goto deposit_;

		/* Accumulate command.  Newline may be escaped to enter a long
		 * command, but all other escapes are passed on unmodified.
		 */
		while ((ch = input()) && ch != '\n') {
		    if (ch == '\\')
			if (ch = input()) {
			    if (ch == '\n')
				continue;
			    else
				yytext[yyleng++] = '\\';
			} else
			    break;
		    yytext[yyleng++] = ch;
		}
		if (ch)
		    unput (ch);

		yytext[yyleng] = '\0';
		yylval = addconst (yytext, OT_STRING);
		return (Y_OSESC);

	    case '#':
		/* Discard the comment line. */
		while ((ch = input()) && ch != '\n')
		    ;
		if (ch) {
		    unput (ch);
		    continue;
		} else
		    return (0);

	    case '>':
	    case '<':
	    case '(':
		/* These characters are alike in that they all begin a new
		 * argument when found in an argument list.
		 */
		if (!newtoken) {
		    unput (ch);
		    goto tokout_;
		} else if (newarg) {
		    unput (ch);
		    newarg = 0;
		    return (',');
		} else if (ch == '<') {
		    token = ch;
		    goto eatwhite_;

		} else if (ch == '>') {
		    ch = input();
		    if (ch == 0) {
			return ('>');

		    } else if (ch == '>') {
			ch = input();
			if (ch == 0) {
			    return (Y_APPEND);
			} else if (ch == 'G' || ch == 'I' || ch == 'P') {
			    op = yytext;
			    *op++ = '>';
			    *op++ = '>';
			    *op++ = ch;
			    goto gsredir_;
			} else if (ch == '&') {
			    token = Y_ALLAPPEND;
			    goto eatwhite_;
			} else {
			    unput (ch);
			    token = Y_APPEND;
			    goto eatwhite_;
			}

		    } else if (ch == 'G' || ch == 'I' || ch == 'P') {
			/* Graphics stream redirection.
			 */
			op = yytext;
			*op++ = '>';
			*op++ = ch;
gsredir_:
			ch = input();
			while (ch == 'G' || ch == 'I' || ch == 'P') {
			    *op++ = ch;
			    ch = input();
			}
			unput (ch);
			*op = EOS;

			yylval = addconst (yytext, OT_STRING);
			token = Y_GSREDIR;
			goto eatwhite_;

		    } else if (ch == '&') {
			token = Y_ALLREDIR;
			goto eatwhite_;
		    } else {
			unput (ch);
			token = '>';
			goto eatwhite_;
		    }

		} else
		    return ('(');

	    case '|':
		if (!newtoken) {
		    unput (ch);
		    goto tokout_;
		} else if (ch = input()) {
		    if (ch == '&')
			return (Y_ALLPIPE);
		    else {
			unput (ch);
			return ('|');
		    }
		} else
		    return (0);

	    case '*':
	    case '/':
		cch = input();
		if (cch == 0)
		    return (0);

		if (newtoken) {
		    if (cch == '=')
			return ((ch=='*') ? YOP_AOMUL:YOP_AODIV);
		    else {
			unput (cch);
			goto deposit_;
		    }
		} else {
		    if (cch == '=') {
			unput (cch);
			unput (ch);
			goto tokout_;
		    } else {
			unput (cch);
			goto deposit_;
		    }
		}

	    /* The following cases are included to force the compiler
	     * to compile the case as an ASCII jump table.
	     */
	    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	    case 'g': case 'h': case 'i': case 'j': case 'k': case 'l':
	    case 'm': case 'n': case 'o': case 'p': case 'q': case 'r':
	    case 's': case 't': case 'u': case 'v': case 'w': case 'x':
	    case 'y': case 'z':
	    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	    case 'G': case 'H': case 'I': case 'J': case 'K': case 'L':
	    case 'M': case 'N': case 'O': case 'P': case 'Q': case 'R':
	    case 'S': case 'T': case 'U': case 'V': case 'W': case 'X':
	    case 'Y': case 'Z':
		/* fall through to default */

	    default:
		goto deposit_;
escape_:	
		/* Deposit a character preceded by the escape character.
		 */
		if (!newarg) {
		    unput (ch);
		    ch = '\\';
		}
deposit_:
		/* If the last token returned was a string argument and we
		 * are starting a second, a delimiter token must be returned
		 * to delimit the two arguments.  Check for chars not legal
		 * in an identifier so that we can know whether to return
		 * CONSTANT or call crackident() which returns IDENT if not
		 * a reserved keyword.
		 */
		if (newtoken) {
		    identifier = 1;
		    stringtok  = 1;
		    setlevel   = 0;
		    if (newarg) {
			unput (ch);
			newarg = 0;
			return (',');
		    }
		}

		yytext[yyleng++] = ch;
		if (ch == '\\')
		    yytext[yyleng++] = ch = input();
		else if (!(isalnum(ch) || ch == '_' || ch == '$' || ch == '.'))
		    identifier = 0;
	    }
	}

tokout_:
	yytext[yyleng] = '\0';

	if (isdigit (yytext[0]) || yytext[0] == '.' && isdigit (yytext[1])) {
	    int	token, toklen;

	    token = c_lexnum (yytext, &toklen);
	    if (token != LEX_NONNUM && toklen == yyleng) {
		switch (token) {
		case LEX_REAL:
		    yylval = addconst (yytext, OT_REAL);
		    break;
		default:
		    yylval = addconst (yytext, OT_INT);
		    break;
		}
		return (Y_CONSTANT);
	    }
	}

	if (identifier)
	    return (crackident (yytext));
	else {
	    yylval = addconst (yytext, OT_STRING);
	    return (Y_CONSTANT);
	}

eatwhite_:
	/* Control transfers here after a token has been identified which is
	 * followed by an associated argument (e.g. > file or < file).  Our
	 * function is to discard any whitespace following the current token
	 * in order to make whitespace optional in the input at this point.
	 * This makes "> file" (for example) equivalent to ">file".
	 */
	newarg = 0;
        while ((ch = input()) && (ch == ' ' || ch == '\t'))
	    ;
	if (ch) {
	    unput (ch);
	    return (token);
	} else
	    return (0);
}


/* LEXINIT -- Initialize the internal state variables of the lexical analyzer,
 * e.g. when processing is interrupted by an interrupt.
 */
int 
lexinit (void)
{
	if (lexmodes() && !lex_cpumodeset (currentask->t_in)) {
	    lexcol = 0;
	    newarg = 0;
	    pbtoken = 0;
	    lhs = 1;
	    _lexmodes = 1;
	} else
	    _lexmodes = 0;
}
