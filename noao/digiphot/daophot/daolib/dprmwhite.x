include <ctype.h>

# DP_RMWHITE -- Remove whitespace from a string.

procedure dp_rmwhite (instr, outstr, maxch)

char	instr[ARB]		# the input string
char	outstr[ARB]		# the output string, may be the same as instr
int	maxch			# maximum number of characters in outstr

int	ip, op

begin
	op = 1
	for (ip = 1; (instr[ip] != EOS) && (op <= maxch); ip = ip + 1) {
	    if (IS_WHITE(instr[ip]))
		next
	    outstr[op] = instr[ip]
	    op = op + 1
	}
	outstr[op] = EOS
end
