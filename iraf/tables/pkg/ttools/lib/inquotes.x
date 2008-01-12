include <chars.h>

# inquotes -- Put quotes around string
# This procedure examines the input/output string for blanks, tabs and
# double quotes.  If any of these is found, the string will be enclosed in
# double quotes (unless it already begins with "), and embedded quotes will
# be escaped with the '\' character.  If the input string is null then it
# will be replaced with a pair of adjacent double quotes.  If maxch is not
# large enough to include the extra characters, however, the string will not
# be modified.  The input and output strings may be the same.
#
# If there are trailing blanks but no embedded blanks, tabs or quotes, 
# then the input will be copied unmodified to the output.  (6/17/92)
# 
# The reason for enclosing a string in quotes is so that it may be read
# later using ctowrd, and the entire string will be taken as one "word".
#
# Phil Hodge, 21-Jul-1987  Subroutine created.
# Phil Hodge, 11-Aug-1987  Add outstr to calling sequence.
# Phil Hodge, 17-Jun-1992  Also check for tabs; ignore trailing whitespace.
# Phil Hodge, 13-Jan-1995  Include show_trailing argument in calling sequence.

procedure inquotes (instr, outstr, maxch, show_trailing)

char	instr[ARB]		# i: the string to be enclosed in quotes
char	outstr[ARB]		# o: copy of instr, possibly enclosed in quotes
int	maxch			# i: maximum length of string outstr
int	show_trailing		# i: YES means show trailing blanks
#--
bool	must_fix		# true if str contains blanks and/or quotes
int	non_blank_len		# length of instr up to last non-blank char
int	inlen			# same as non_blank_len
int	outlen			# length of outstr on output
int	numquotes		# a count of the number of embedded quotes
int	ip, op			# counters for input & output locations
int	strlen()

begin
	# Find the length of the string ...
	if (show_trailing == YES) {
	    # ... including trailing blanks.
	    non_blank_len = strlen (instr)
	} else {
	    # ... up to the last non-blank character.
	    non_blank_len = 0			# initial value
	    do ip = 1, maxch {
		if (instr[ip] == EOS)
		    break
		if (instr[ip] != BLANK)		# else ignore blank
		    non_blank_len = ip
	    }
	}

	# Replace a null or completely blank string with "".
	if (instr[1] == EOS || non_blank_len < 1) {
	    if (maxch >= 2)
		call strcpy ("\"\"", outstr, maxch)
	    else				# can't fix it
		call strcpy (instr, outstr, maxch)
	    return
	}

	inlen = non_blank_len
	numquotes = 0				# initial values
	must_fix = false

	# Run through the input string, but only go as far as the last
	# non-blank character so we don't include trailing blanks.
	do ip = 1, non_blank_len {
	    if (instr[ip] == EOS) {
		break
	    } else if (instr[ip] == BLANK) {
		must_fix = true
	    } else if (instr[ip] == TAB) {
		must_fix = true
	    } else if (instr[ip] == DQUOTE) {
		if (ip == 1) {
		    call strcpy (instr, outstr, maxch)
		    return		# begins with ", so don't "fix" it
		}
		if (instr[ip-1] != ESCAPE) {
		    must_fix = true
		    numquotes = numquotes + 1
		}
	    }
	}

	outlen = inlen + numquotes + 2
	if (outlen > maxch || !must_fix) {
	    call strcpy (instr, outstr, maxch)
	    return		    # can't fix it or don't need to
	}

	# Work from the end toward the beginning in case instr = outstr.
	outstr[outlen+1] = EOS
	outstr[outlen] = DQUOTE
	op = outlen - 1

	if (numquotes > 0) {
	    # There are quotes within the string.
	    do ip = inlen, 1, -1 {
		outstr[op] = instr[ip]
		if (instr[ip] == DQUOTE) {
		    if (instr[ip-1] != ESCAPE) {
			op = op - 1
			outstr[op] = ESCAPE
		    }
		}
		op = op - 1
	    }
	} else {
	    # No embedded quotes.
	    do ip = inlen, 1, -1 {
		outstr[op] = instr[ip]
		op = op - 1
	    }
	}
	outstr[1] = DQUOTE
	if (op != 1)
	    call error (1, "miscount in inquotes")
end
