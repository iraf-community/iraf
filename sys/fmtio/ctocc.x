# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

define	OCTAL		8

# CTOCC -- Convert a character into a printable character constant.
# Printable characters are output as is.  The standard control characters
# (newline, tab, etc.) are output as escape sequences (\n, \t, etc.).
# Other control characters are output in the form '^X'.  Characters which
# are neither printable nor standard control characters are output as
# octal constants of the form '\DDD'.  Note that the ouput string is not
# enclosed in ticks ('\n', etc.), because the generated character constant
# might appear in a quoted string (or someplace other than an explicit
# character constant).

int procedure ctocc (ch, outstr, maxch)

char	ch			# character to be output
char	outstr[ARB]		# output string
int	maxch			# max chars out

int	op, n
int	stridx()
define	output 	{outstr[op]=$1;op=op+1;if(op>maxch)goto overflow_}
define	overflow_ 99
include	"escchars.inc"

begin
	op = 1

	if (maxch > 0) {
	    if (IS_PRINT(ch)) {				# output char as is
		output (ch)
	    } else if (IS_CNTRL (ch)) {
		n = stridx (ch, mapped_chars)
		if (n > 0) {				# '\c'
		    output ('\\')
		    output (escape_chars[n])
		} else {
		    output ('^')			# control chars
		    output (ch + 'A' - 1)
		}

	    } else {					# '\nnn' 
		# Always output 3 digits so that strings like \0405 (a blank
		# followed by a `5') can be interpreted during the reverse
		# encoding operation.

		output ('\\')
		output (TO_DIGIT (mod (ch / 0100B, 010B)))
		output (TO_DIGIT (mod (ch / 0010B, 010B)))
		output (TO_DIGIT (mod (ch / 0001B, 010B)))
	    }
	}

	outstr[op] = EOS
	return (op-1)

overflow_
	outstr[1] = '?'					# no room, print '?'
	outstr[2] = EOS
	return (1)
end
