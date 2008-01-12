include <ctype.h>
include <lexnum.h>
include	"../lib/lexer.h"
include "../lib/prtoken.h"


# PR_LEXER - Lexical analizer for the parser of the configuration file.

int procedure pr_lexer (fd, yylval)

int	fd			# input file descriptor
pointer	yylval			# YYLVAL token pointer

char	key[SZ_LINE]
int	tok			# next token
int	ip, n

include	"lexer.com"

bool	strne()
int	ctor()
int	strdic(), strlen()
int	lexnum()
int	getline()

begin
	# Stay scanning the input file until a valid or
	# error token is found
	repeat {

	    # Skip whitespaces and blank lines
	    while (line[pos] == '\n' || IS_WHITE (line[pos])) {

		# If next character is a newline count
		# lines, and read a new line from file
		if (line[pos] == '\n') {
		    nlines = nlines + 1
		    pos = 1
		    if (getline (fd, line) == EOF) {
			call strcpy ("EOF", LEX_ID (yylval), SZ_LINE)
			return (EOFILE)
		    }
		} else
		    pos = pos + 1
	    }

	    # Test first valid character
	    if (IS_ALPHA (line[pos])) {	# identifier or keyword

		# Read identifier
		for (ip=1; IS_ALNUM (line[pos]); ip=ip+1) {
		    id[ip] = line[pos]
		    pos = pos + 1
		}
	        id[ip] = EOS

		# Check for keyword. Abreviations of keywords are allowed
		# to up to a certain number of characters, but the
		# identifier returned by the lexer should contain the full
		# keyword name.

		n = strdic (id, key, SZ_LINE, KEYWORDS)
		if (n > 0 && strlen (id) >= ABBREVIATE) {
		    call strcpy (key, id, SZ_LINE)
		    switch (n) {
		    case K_CATALOG:
			tok = CATSECT
			break
		    case K_OBSERVATION:
			tok = OBSSECT
			break
		    case K_EXTINCTION:
			tok = EXTSECT
			break
		    case K_TRANSFORMATION:
			tok = TRNSECT
			break
		    case K_FIT:
			tok = FITID
			break
		    case K_CONSTANT:
			tok = CONSTID
			break
		    case K_DELTA:
			tok = DELTAID
			break
		    case K_ERROR:
			tok = ERRORID
			break
		    case K_WEIGHT:
			tok = WEIGHTID
			break
		    case K_MIN:
			tok = MINID
			break
		    case K_MAX:
			tok = MAXID
			break
		    case K_SET:
			tok = SETID
			break
		    case K_DERIVATIVE:
			tok = DERIVID
			break
		    case K_PLOT:
			tok = PLOTID
			break
		    default:
			call error (0, "pr_lexxer: Unknown keyword token")
		    }
		}

		# Check for function. Anything abbreviated,
		# or not matching is and identifier.

		n = strdic (id, key, SZ_LINE, FUNCTIONS)
		if (n == 0) {
		    tok = IDENTIFIER
		    break
		} else if (strne (id, key)) {
		    tok = IDENTIFIER
		    break
		}
		switch (n) {
		case K_ABS:		# absolute value
		    tok = F_ABS
		    break
		case K_ACOS:		# arc cosine
		    tok = F_ACOS
		    break
		case K_ASIN:		# arc sine
		    tok = F_ASIN
		    break
		case K_ATAN:		# arc tangent
		    tok = F_ATAN
		    break
		case K_COS:		# cosine
		    tok = F_COS
		    break
		case K_EXP:		# exponential
		    tok = F_EXP
		    break
		case K_LOG:		# natural logarithm
		    tok = F_LOG
		    break
		case K_LOG10:		# decimal logarithm
		    tok = F_LOG10
		    break
		case K_SIN:		# sine
		    tok = F_SIN
		    break
		case K_SQRT:		# square root
		    tok = F_SQRT
		    break
		case K_TAN:		# tangent
		    tok = F_TAN
		    break
		default:
			call error (0, "pr_lexer: Unknown identifier")
		}

	    } else if (IS_DIGIT (line[pos]) || line[pos] == '.') {	# number

		# Process number
		switch (lexnum (line, pos, n)) {
		case LEX_DECIMAL:
		    tok = INUMBER
		case LEX_REAL:
		    tok = RNUMBER
		default:
		    tok = ERR
		}

		# Copy whatever was processed
		# to the identifier
		do ip = 1, n
		    id[ip] = line[pos + ip - 1]
		id[n + 1] = EOS

		# Advance to next token and
		# break the loop
		pos = pos + n
		break

	    } else if (line[pos] == '(') {	# left parenthesis

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy ("(", id, SZ_LINE)
		tok = LPAR
		pos = pos + 1
		break

	    } else if (line[pos] == ')') {	# right parenthesis

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy (")", id, SZ_LINE)
		tok = RPAR
		pos = pos + 1
		break

	    } else if (line[pos] == '+') {	# plus

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy ("+", id, SZ_LINE)
		tok = PLUS
		pos = pos + 1
		break

	    } else if (line[pos] == '-') {	# minus

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy ("-", id, SZ_LINE)
		tok = MINUS
		pos = pos + 1
		break

	    } else if (line[pos] == '*') {	# star and double star

		# Advance to next character to see if 
		# it's another star
		pos = pos + 1
		if (line[pos] == '*') {

		    # Copy input character to identifier, set
		    # token, and advance to next character before
		    # breaking the loop
		    call strcpy ("**", id, SZ_LINE)
		    tok = EXPON
		    pos = pos + 1
		    break

		} else {

		    # Copy input characters to identifier, set
		    # token, and break the loop
		    call strcpy ("*", id, SZ_LINE)
		    tok = STAR
		    break
		}

	    } else if (line[pos] == '/') {	# slash

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy ("/", id, SZ_LINE)
		tok = SLASH
		pos = pos + 1
		break

	    } else if (line[pos] == '=') {	# equal

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy ("=", id, SZ_LINE)
		tok = EQUAL
		pos = pos + 1
		break

	    } else if (line[pos] == ',') {	# comma

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy (",", id, SZ_LINE)
		tok = COMMA
		pos = pos + 1
		break

	    } else if (line[pos] == ':') {	# colon

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy (":", id, SZ_LINE)
		tok = COLON
		pos = pos + 1
		break

	    } else if (line[pos] == ';') {	# semicolon

		# Copy input character to identifier, set
		# token, and advance to next character before
		# breaking the loop
		call strcpy (";", id, SZ_LINE)
		tok = SEMICOLON
		pos = pos + 1
		break

	    } else if (line[pos] == '#') {	# comment

		# Skip current line
		pos = strlen (line)

	    } else {	# none of the above

		# All characters not included in the previous
		# categories are treated as errors
		id[1] = line[pos]
		id[2] = EOS
	        tok = ERR

		# Advance to next character before
		# breaking the loop
		pos = pos + 1
		break
	    }

	} # repeat

	# Update yylval structure
	LEX_TOK (yylval) = tok
	call strcpy (id, LEX_ID (yylval), SZ_LINE)
	if (tok == INUMBER || tok == RNUMBER) {
	    ip = 1
	    n = ctor (LEX_ID (yylval), ip, LEX_VAL (yylval))
	} else
	    LEX_VAL (yylval) = INDEFR

	# Debug
	#call eprintf ("(tok=%d) (id=%s) (rval=%g)\n")
	    #call pargi (LEX_TOK (yylval))
	    #call pargstr (LEX_ID (yylval))
	    #call pargi (LEX_VAL (yylval))

	# Return token value
	return (tok)
end
