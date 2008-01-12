# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<chars.h>

# GTYGETS -- Get the string value of a capability.  Process all termcap escapes.
# These are:
# 
# 	\E		ascii esc (escape)
# 	^X		control-X (i.e., ^C=03B, ^Z=032B, etc.)
# 	\[nrtbf]	newline, return, tab, backspace, formfeed
# 	\ddd		octal value of character
# 	\^		the character ^
# 	\\		the character \
# 
# The character ':' may not be placed directly in a capability string; it
# should be given as \072 instead.  The null character is represented as \200;
# all characters are masked to 7 bits upon output by TTYPUTS, hence \200
# is sent to the terminal as NUL.

int procedure gtygets (tty, cap, outstr, maxch)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character capability name
char	outstr[ARB]		# receives cap string
int	maxch			# size of outstr

char	ch
pointer	ip
int	op, junk, temp
int	gty_find_capability(), cctoc()

begin
	op = 1

	if (gty_find_capability (tty, cap, ip) == YES) {
	    # Skip the '=' which follows the two character capability name.
	    if (Memc[ip] == '=')
		ip = ip + 1

	    # Extract the string, processing all escapes.
	    for (ch=Memc[ip];  ch != ':';  ch=Memc[ip]) {
		if (ch == '^') {
		    ip = ip + 1
		    temp = Memc[ip]
		    ch = mod (temp, 40B)
		} else if (ch == '\\') {
		    switch (Memc[ip+1]) {
		    case 'E':
			ip = ip + 1
			ch = ESC
		    case '^', ':', '\\':
			ip = ip + 1
			ch = Memc[ip]
		    default:
			junk = cctoc (Memc, ip, ch)
			ip = ip - 1
		    }
		}

		outstr[op] = ch
		op = op + 1
		ip = ip + 1
		if (op >= maxch)
		    break
	    }
	}

	outstr[op] = EOS
	return (op-1)
end
