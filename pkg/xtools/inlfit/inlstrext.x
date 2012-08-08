include	<ctype.h>

# INLSTREXT - Extract a word (delimited substring) from a string.
# The input string is scanned from the given initial value until one
# of the delimiters is found. The delimiters are not included in the
# output word.
# Leading white spaces in a word may be optionally skipped. White 
# spaces are skipped before looking at the delimiters string, so it's
# possible to remove leading white spaces and use them as delimiters
# at the same time.
# The value returned is the number of characters in the output string.
# Upon return, the pointer is located at the begining of the next word.

int procedure inlstrext (str, ip, dict, skip, outstr, maxch)

char	str[ARB]			# input string
int	ip				# pointer into input string
char	dict[ARB]			# dictionary of delimiters
int	skip				# skip leading white spaces ?
char	outstr[ARB]			# extracted word
int	maxch				# max number of chars

int	op
int	stridx()

begin
	# Skip leading white spaces
	if (skip == YES) {
	    while (IS_WHITE (str[ip]))
		ip = ip + 1
    	}

	# Process input string
	for (op=1;  str[ip] != EOS && op <= maxch;  op=op+1)
	    if (stridx (str[ip], dict) == 0) {
		outstr[op] = str[ip]
		ip = ip + 1
	    } else {
		repeat {
		    ip = ip + 1
		} until (stridx (str[ip], dict) == 0 || str[ip] == EOS)
		break
	    }

	outstr[op] = EOS
	return (op - 1)
end
