include	<lexnum.h>
include	<ctype.h>
include	<mach.h>
include	<math.h>
include	<evvexpr.h>


# Parser definitions.
define  CONSTANT        257
define  IDENTIFIER      258
define  NEWLINE         259
define  YYEOS           260
define  PLUS            261
define  MINUS           262
define  STAR            263
define  SLASH           264
define  EXPON           265
define  CONCAT          266
define  QUEST           267
define  COLON           268
define  LT              269
define  GT              270
define  LE              271
define  EQ              272
define  NE              273
define  SE              274
define  LAND            275
define  LOR             276
define  LNOT            277
define  BAND            278
define  BOR             279
define  BXOR            280
define  BNOT            281
define  AT              282
define  GE              283
define  UMINUS          284


# XVV_GETTOK -- Lexical analyzer for EVVEXPR.  Returns the token code as the
# function value.  If the token is an operand (identifier or constant) the
# operand value is returned in OUT.

int procedure xvv_gettok (ip, out)

pointer	ip			#I pointer into input string (expression)
pointer	out			#I pointer to yacc YYLVAL token value operand

char	ch
long	lval
double	dval
pointer	ip_start
char	numbuf[MAX_DIGITS]
int	nchars, token, junk, dtype
int	stridx(), stridxs(), lexnum(), gctod(), gctol()
define	ident_ 91

begin
	while (IS_WHITE(Memc[ip]))
	    ip = ip + 1

	ch = Memc[ip]
	switch (ch) {
	case 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',      'J', 'K', 'L', 'M',
	     'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', 
	     'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
	     'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z': 

	    # Return an identifier.
ident_
	    ip_start = ip
	    while (IS_ALNUM(ch) || stridx (ch, "_.$@#%&;[]\\^{}~") > 0) {
		ip = ip + 1
		ch = Memc[ip]
	    }

	    nchars = ip - ip_start
	    call xvv_initop (out, nchars, TY_CHAR)
	    call strcpy (Memc[ip_start], O_VALC(out), nchars)

	    return (IDENTIFIER)

	case 'I', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
	    # Return a numeric constant.  The character I vectors here so
	    # that we can check for INDEF, a legal number.

	    token = lexnum (Memc, ip, nchars)
	    switch (token) {
	    case LEX_OCTAL:
		junk = gctol (Memc, ip, lval, 8)
		call xvv_initop (out, 0, TY_INT)
		O_VALI(out) = lval
	    case LEX_DECIMAL:
		junk = gctol (Memc, ip, lval, 10)
		call xvv_initop (out, 0, TY_INT)
		O_VALI(out) = lval
	    case LEX_HEX:
		junk = gctol (Memc, ip, lval, 16)
		call xvv_initop (out, 0, TY_INT)
		O_VALI(out) = lval

	    case LEX_REAL:
		ip_start = ip
		nchars = gctod (Memc, ip, dval)
		call strcpy (Memc[ip], numbuf, min(nchars,MAX_DIGITS))

		dtype = TY_REAL
		if (stridxs ("dD", numbuf) > 0 || nchars > NDIGITS_RP+3)
		    dtype = TY_DOUBLE

		call xvv_initop (out, 0, dtype)
		if (dtype == TY_REAL) {
		    if (IS_INDEFD (dval))
			O_VALR(out) = INDEFR
		    else
			O_VALR(out) = dval
		} else {
		    if (IS_INDEFD (dval))
			O_VALD(out) = INDEFD
		    else
			O_VALD(out) = dval
		}
	    default:
		goto ident_
	    }

	    return (CONSTANT)

	case '\'', '"':
	    # Return a string constant.

	    ip_start = ip + 1
	    for (ip=ip+1;  Memc[ip] != ch && Memc[ip] != EOS;  ip=ip+1)
		;

	    nchars = ip - ip_start
	    if (Memc[ip] == EOS)
		call xvv_error ("missing closing quote in string constant")
	    else
		ip = ip + 1

	    call xvv_initop (out, nchars, TY_CHAR)
	    call strcpy (Memc[ip_start], O_VALC(out), nchars)

	    return (CONSTANT)

	case '+':
	    token = PLUS
	case '-':
	    token = MINUS
	case '*':
	    if (Memc[ip+1] == '*') {
		ip = ip + 1
		token = EXPON
	    } else
		token = STAR
	case '/':
	    if (Memc[ip+1] == '/') {
		ip = ip + 1
		token = CONCAT
	    } else
		token = SLASH

	case '?':
	    if (Memc[ip+1] == '=') {
		ip = ip + 1
		token = SE
	    } else
		token = QUEST

	case ':':
	    token = COLON

	case '@':
	    token = AT

	case '<':
	    if (Memc[ip+1] == '=') {
		ip = ip + 1
		token = LE
	    } else
		token = LT
	case '>':
	    if (Memc[ip+1] == '=') {
		ip = ip + 1
		token = GE
	    } else
		token = GT
	case '!':
	    if (Memc[ip+1] == '=') {
		ip = ip + 1
		token = NE
	    } else
		token = LNOT
	case '=':
	    if (Memc[ip+1] == '=') {
		ip = ip + 1
		token = EQ
	    } else
		token = EQ
	case '&':
	    if (Memc[ip+1] == '&') {
		ip = ip + 1
		token = LAND
	    } else
		token = BAND
	case '|':
	    if (Memc[ip+1] == '|') {
		ip = ip + 1
		token = LOR
	    } else
		token = BOR

	case '^':
	    token = BXOR
	case '~':
	    token = BNOT

	case '(', ')', ',':
	    token = ch

	default:
	    if (ch == '\n')
		token = NEWLINE
	    else if (ch == EOS)
		token = YYEOS
	    else {
		# Anything we don't understand is assumed to be an identifier.
		goto ident_
	    }
	}

	ip = ip + 1
	return (token)
end
