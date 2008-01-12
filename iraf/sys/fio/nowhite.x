# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# NOWHITE -- Return the input string minus any whitespace or newlines,
# returning a count of the number of nonwhite characters as the function value.

int procedure nowhite (in, out, maxch)

char	in[ARB]			# input string
char	out[ARB]		# output string
int	maxch			# max chars out

int	ch
int	ip, op

begin
	op = 1
	do ip = 1, ARB {
	    ch = in[ip]
	    if (ch <= ' ') {
		if (ch == EOS)
		    break
		else if (IS_WHITE(ch) || ch == '\n')
		    next
	    }
	    if (op > maxch)
		break
	    out[op] = ch
	    op = op + 1
	}

	out[op] = EOS
	return (op - 1)
end
