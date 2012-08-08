# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>

# CTOWRD -- Break whitespace delimited token or quoted string out of input
# stream.  If string, process escape sequences.  The number of characters
# converted from the input string, excluding whitespace, is returned as the
# function value.

int procedure ctowrd (str, ip, outstr, maxch)

char	str[ARB]			# input string
int	ip				# pointer into input string
char	outstr[ARB]			# receives extracted word
int	maxch

char	cch
int	ch, junk, op
int	ip_start, delim, i
define	qsput_ 91
define	wsput_ 92
int	cctoc()

begin
	while (IS_WHITE(str[ip]))
	    ip = ip + 1
	ip_start = ip

	delim = str[ip]
	if (delim == DQUOTE || delim == SQUOTE) {
	    # Extract a quoted string.
	    op = 1
	    ip = ip + 1
	    do i = 1, ARB {
		ch = str[ip]
		if (ch == EOS) {
		    break
		} else if (ch == ESCAPE) {
		    ch = str[ip+1]
		    if (ch == delim) {
			ip = ip + 2
			goto qsput_
		    } else {
			junk = cctoc (str, ip, cch)
			ch = cch
			goto qsput_
		    }
		} else if (ch == delim) {
		    ip = ip + 1
		    break
		} else {
	   	    ip = ip + 1
qsput_		    if (op <= maxch) {
			outstr[op] = ch
			op = op + 1
		    }
		}
	    }
	} else {
	    # Extract a whitespace delimited string.
	    op = 1
	    do i = 1, ARB {
		ch = str[ip]
		if (IS_WHITE(ch) || ch == '\n' || ch == EOS) {
		    break
		} else if (ch == ESCAPE) {
		    junk = cctoc (str, ip, cch)
		    ch = cch
		    goto wsput_
		} else {
		    ip = ip + 1
wsput_		    if (op <= maxch) {
			outstr[op] = ch
			op = op + 1
		    }
		}
	    }
	}

	outstr[op] = EOS
	return (ip - ip_start)
end
