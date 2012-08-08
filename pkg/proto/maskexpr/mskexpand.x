include <ctotok.h>
include <ctype.h>
include "gettok.h"

# Some definitions.

# Default symbol table size limits.
define	DEF_LENINDEX	97
define	DEF_LENSTAB	1024
define	DEF_LENSBUF	8192

# Expression database symbol.
define	LEN_SYM		2
define	SYM_TEXT	Memi[$1]
define	SYM_NARGS	Memi[$1+1]

# Argument list symbol
define	LEN_ARGSYM	1
define	ARGNO		Memi[$1]


# ME_GETEXPRDB -- Read the expression database into a symbol table.  The
# input file has the following structure:
#
#	<symbol>['(' arg-list ')'][':'|'=']	replacement-text
#		
# Symbols must be at the beginning of a line.  The expression text is
# terminated by a nonempty, noncomment line with no leading whitespace.

pointer procedure me_getexprdb (fname)

char	fname[ARB]		#I file to be read

pointer	sym, sp, lbuf, st, a_st, ip, symname, tokbuf, text
int	tok, fd, line, nargs, op, token, buflen, offset, stpos, n
pointer	stopen(), stenter()
int	open(), getlline(), ctotok(), stpstr()
errchk	open, getlline, stopen, stenter, me_puttok

define	skip_ 91

begin
	call smark (sp)
	call salloc (lbuf, SZ_COMMAND, TY_CHAR)
	call salloc (text, SZ_COMMAND, TY_CHAR)
	call salloc (tokbuf, SZ_COMMAND, TY_CHAR)
	call salloc (symname, SZ_FNAME, TY_CHAR)

	fd = open (fname, READ_ONLY, TEXT_FILE)
	st = stopen ("imexpr", DEF_LENINDEX, DEF_LENSTAB, DEF_LENSBUF)
	a_st = stopen ("args", DEF_LENINDEX, DEF_LENSTAB, DEF_LENSBUF)
	line = 0

	while (getlline (fd, Memc[lbuf], SZ_COMMAND) != EOF) {
	    line = line + 1
	    ip = lbuf

	    # Skip comments and blank lines.
	    while (IS_WHITE(Memc[ip]))
		ip = ip + 1
	    if (Memc[ip] == '\n' || Memc[ip] == '#')
		next
	
	    # Get symbol name.
	    if (ctotok (Memc,ip,Memc[symname],SZ_FNAME) != TOK_IDENTIFIER) {
		call eprintf ("exprdb: expected identifier at line %d\n")
		    call pargi (line)
skip_		while (getlline (fd, Memc[lbuf], SZ_COMMAND) != EOF) {
		    line = line + 1
		    if (Memc[lbuf] == '\n')
			break
		}
	    }

	    call stmark (a_st, stpos)

	    # Check for the optional argument-symbol list.  Allow only a
	    # single space between the symbol name and its argument list,
	    # otherwise we can't tell the difference between an argument
	    # list and the parenthesized expression which follows.

	    if (Memc[ip] == ' ')
		ip = ip + 1

	    if (Memc[ip] == '(') {
		ip = ip + 1
		n = 0
		repeat {
		    tok = ctotok (Memc, ip, Memc[tokbuf], SZ_FNAME)
		    if (tok == TOK_IDENTIFIER) {
			sym = stenter (a_st, Memc[tokbuf], LEN_ARGSYM)
			n = n + 1
			ARGNO(sym) = n
		    } else if (Memc[tokbuf] == ',') {
			;
		    } else if (Memc[tokbuf] != ')') {
			call eprintf ("exprdb: bad arglist at line %d\n")
			    call pargi (line)
			call stfree (a_st, stpos)
			goto skip_
		    }
		} until (Memc[tokbuf] == ')')
	    }

	    # Check for the optional ":" or "=".
	    while (IS_WHITE(Memc[ip]))
		ip = ip + 1
	    if (Memc[ip] == ':' || Memc[ip] == '=')
		ip = ip + 1

	    # Accumulate the expression text.
	    buflen = SZ_COMMAND
	    op = 1
	    
	    repeat {
		repeat {
		    token = ctotok (Memc, ip, Memc[tokbuf], SZ_COMMAND)
		    if (Memc[tokbuf] == '#')
			break
		    else if (token != TOK_EOS && token != TOK_NEWLINE)
			call me_puttok (a_st, text, op, buflen, Memc[tokbuf])
		} until (token == TOK_EOS)

		if (getlline (fd, Memc[lbuf], SZ_COMMAND) == EOF)
		    break
		else
		    line = line + 1

		for (ip=lbuf;  IS_WHITE(Memc[ip]);  ip=ip+1)
		    ;
		if (ip == lbuf) {
		    call ungetline (fd, Memc[lbuf])
		    line = line - 1
		    break
		}
	    }

	    # Free any argument list symbols.
	    call stfree (a_st, stpos)

	    # Scan the expression text and count the number of $N arguments.
	    nargs = 0
	    for (ip=text;  Memc[ip] != EOS;  ip=ip+1)
		if (Memc[ip] == '$' && IS_DIGIT(Memc[ip+1])) {
		    nargs = max (nargs, TO_INTEG(Memc[ip+1]))
		    ip = ip + 1
		}

	    # Enter symbol in table.
	    sym = stenter (st, Memc[symname], LEN_SYM)
	    offset = stpstr (st, Memc[text], 0)
	    SYM_TEXT(sym) = offset
	    SYM_NARGS(sym) = nargs
	}

	call stclose (a_st)
	call sfree (sp)

	return (st)
end


# ME_PUTTOK -- Append a token string to a text buffer.

procedure me_puttok (a_st, text, op, buflen, token)

pointer	a_st			#I argument-symbol table
pointer	text			#U text buffer
int	op			#U output pointer
int	buflen			#U buffer length, chars
char	token[ARB]		#I token string

pointer	sym
int	ip, ch1, ch2
pointer	stfind()
errchk	realloc

begin
	# Replace any symbolic arguments by "$N".
	if (a_st != NULL && IS_ALPHA(token[1])) {
	    sym = stfind (a_st, token)
	    if (sym != NULL) {
		token[1] = '$'
		token[2] = TO_DIGIT(ARGNO(sym))
		token[3] = EOS
	    }
	}

	# Append the token string to the text buffer.
	for (ip=1;  token[ip] != EOS;  ip=ip+1) {
	    if (op + 1 > buflen) {
		buflen = buflen + SZ_COMMAND
		call realloc (text, buflen, TY_CHAR)
	    }

	    # The following is necessary because ctotok parses tokens such as
	    # "$N", "==", "!=", etc.  as two tokens.  We need to rejoin these
	    # characters to make one token.

	    if (op > 1 && token[ip+1] == EOS) {
		ch1 = Memc[text+op-3]
		ch2 = token[ip]

		if (ch1 == '$' && IS_DIGIT(ch2))
		    op = op - 1
		else if (ch1 == '*' && ch2 == '*')
		    op = op - 1
		else if (ch1 == '/' && ch2 == '/')
		    op = op - 1
		else if (ch1 == '<' && ch2 == '=')
		    op = op - 1
		else if (ch1 == '>' && ch2 == '=')
		    op = op - 1
		else if (ch1 == '=' && ch2 == '=')
		    op = op - 1
		else if (ch1 == '!' && ch2 == '=')
		    op = op - 1
		else if (ch1 == '?' && ch2 == '=')
		    op = op - 1
		else if (ch1 == '&' && ch2 == '&')
		    op = op - 1
		else if (ch1 == '|' && ch2 == '|')
		    op = op - 1
	    }

	    Memc[text+op-1] = token[ip]
	    op = op + 1
	}

	# Append a space to ensure that tokens are delimited.
	Memc[text+op-1] = ' '
	op = op + 1

	Memc[text+op-1] = EOS
end


# ME_EXPANDTEXT -- Scan an expression, performing macro substitution on the
# contents and returning a fully expanded string.

pointer procedure me_expandtext (st, expr)

pointer	st			#I symbol table (macros)
char	expr[ARB]		#I input expression

pointer	buf, gt
int	buflen, nchars
int	locpr(), gt_expand()
pointer	gt_opentext()
extern	me_gsym()

begin
	buflen = SZ_COMMAND
	call malloc (buf, buflen, TY_CHAR)

	gt = gt_opentext (expr, locpr(me_gsym), st, 0, GT_NOFILE)
	nchars = gt_expand (gt, buf, buflen)
	call gt_close (gt)

	return (buf)
end
