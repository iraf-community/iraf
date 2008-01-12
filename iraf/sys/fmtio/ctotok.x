# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctotok.h>
include <ctype.h>
include	<chars.h>
include	<lexnum.h>

.help CTOTOK 2 "String Utilities"
.ih ___________________________________________________________________________
NAME
CTOTOK -- Return next token from input text.
.ih
USAGE
token = ctotok (string, ip, outstr, maxch)
.ih
PARAMETERS
The integer value returned by CTOTOK is a code identifying the type of token
matched.  The predefined tokens recognized by CTOTOK (defined in <ctotok.h>)
are the following:
.ls
.nf
TOK_IDENTIFER		[a-zA-Z][a-zA-Z0-9_$.]*
TOK_NUMBER		[0-9][-+0-9.:xXa-fA-F]*
TOK_OPERATOR		[-+*/!@#$%^&=`~<>?|]+
TOK_PUNCTUATION		[,:;(){}] or "[", "]"
TOK_STRING		"..."
TOK_CHARCON		'.'
TOK_EOS			end of string
TOK_NEWLINE		end of line
TOK_UNKNOWN		control characters
.fi
.le
.ls string
The EOS delimited character string from which the next token is to be
extracted.
.le
.ls ip
On input, contains the index of the first character to be scanned
(initially 1).  On output, left pointing at the first character after
the current token, unless EOS was reached.  IP should normally be left
alone in successive calls to CTOTOK.
.le
.ls outstr
String to receive the extracted token value.
.le
.ls maxch
Capacity of the "outstr" buffer.
.le
.ih
DESCRIPTION
CTOTOK is useful for many simple parsing tasks.  For example, it is used
by the HELP utility to parse the ".help" directive, which consists of
a list of keywords (delimited by commas), followed by two strings or
identifiers.

CTOTOK selects the type of token to be extracted based on the token
class membership of the first nonwhitespace character encountered.
Characters are copied to the output string until a character not belonging
to the current class is encountered (or until MAXCH characters have been
output).  Whitespace is always a token delimiter.  The integer code for the
corresponding token is returned as the function value.

An identifier is a letter followed by any number of letters, digits, or
one of the characters [_.$].  A number is any legal integer, octal,
hexadecimal, sexagesimal, or floating point number.  All legal numbers are
matched: however, many illegal numbers (e.g. "99.33.22") are matched as well.
The numeric conversion routines may be used to verify that a number token
is actually a legal number, as well as to convert the number to binary.

An operator is one or more operator characters, or any of the characters
[_.$], not occurring as part of an identifier, but occurring instead as the
first character of an operator.  Note that a string of operator characters
is considered a single token, whereas punctuation characters are returned
as separate tokens.  Strings are enclosed by either single or double quotes,
and all escape sequences are recognized and processed.
Control characters and DEL match the "unknown" token.
.ih
SEE ALSO
tokens(1), strmatch(), patmatch()
.endhelp ______________________________________________________________________

define	TABLESIZE	95
define	NUMCHSIZE	6
define	OFFSET		' '


# CTOTOK -- Character string to token.  The token is returned in OUTSTR and the
# token type code is returned as the function value.

int procedure ctotok (str, ip, outstr, maxch)

char	str[ARB]		# input string
int	ip			# pointer into input string
char	outstr[ARB]		# buffer to receive token
int	maxch			# max chars in output buffer

int	currclass
char	class[TABLESIZE]
int	op, ch, i, junk, nchars
int	ctowrd(), lexnum(), cctoc()
include	"tokdata.inc"

begin
	while (IS_WHITE (str[ip]))
	    ip = ip + 1

	ch = str[ip]
	i  = max(1, min(TABLESIZE, ch - OFFSET))
	op = 1

	if (ch == EOS) {				# select class (token)
	    outstr[1] = EOS
	    return (TOK_EOS)

	} else if (ch == NEWLINE) {			# end of line
	    outstr[1] = ch
	    outstr[2] = EOS
	    ip = ip + 1
	    return (TOK_NEWLINE)

	} else if (ch <= OFFSET) {			# control characters
	    while (op <= maxch && ch != EOS && ch <= OFFSET) {
		outstr[op] = ch
		op = op + 1
		ip = ip + 1
		ch = str[ip]
	    }
	    outstr[op] = EOS
	    return (TOK_UNKNOWN)

	} else if (ch == DQUOTE) {			# string constant
	    junk = ctowrd (str, ip, outstr, maxch)
	    return (TOK_STRING)

	} else if (ch == SQUOTE || ch == ESCAPE) {
	    nchars = cctoc (str, ip, junk)
	    call strcpy (str[ip-nchars], outstr, nchars)
	    return (TOK_CHARCON)

	} else if (lexnum (str, ip, nchars) != LEX_NONNUM) {
	    call strcpy (str[ip], outstr, nchars)
	    ip = ip + nchars
	    return (TOK_NUMBER)

	} else if (class[i] == TOK_IDENTIFIER && !IS_ALPHA (ch)) {
	    currclass = TOK_OPERATOR

	} else if (class[i] == TOK_PUNCTUATION) {	# only one at a time
	    outstr[1] = ch
	    outstr[2] = EOS
	    ip = ip + 1
	    return (TOK_PUNCTUATION)

	} else
	    currclass = class[i]

	repeat {					# copy token to output
	    outstr[op] = ch
	    op = op + 1
	    ip = ip + 1
	    ch = str[ip]
	    i  = max(1, min(TABLESIZE, ch - OFFSET))
	} until (ch == EOS || ch <= OFFSET || class[i] != currclass)

	outstr[op] = EOS
	return (currclass)
end
