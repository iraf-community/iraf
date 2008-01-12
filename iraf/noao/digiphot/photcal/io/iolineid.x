include	<ctype.h>


# IO_LINEID - Get the line identification string from input line and advance
# pointer to next non-white character. Convert matching string to uppercase,
# and keep characters in the [A..Z, 0..9] set, but return both the original
# and compressed matching strings. The first one is intended for output to
# the user, and the second for internal use to avoid typos.

int procedure io_lineid (line, ip, uid, cid, maxch)

char	line[ARB]		# input line
int	ip			# input pointer
char	uid[ARB]		# user's (original) line identification string
char	cid[ARB]		# compressed line identification string
int	maxch			# output chars

int	i, op

begin
	# Discard the leading whitespaces.
	while (IS_WHITE (line[ip]) && line[ip] != EOS)
	    ip = ip + 1

	# Get the line identifier.
	for (op = 1; !IS_WHITE (line[ip]) && line[ip] != EOS && op <= maxch;
	     op = op + 1) {
	    uid[op] = line[ip]
	    ip = ip + 1
	}
	uid[op] = EOS

	# Copy the orignal identifier into the compressed identifier,
	# and convert the latter to upper case.
	call strcpy (uid, cid, maxch)
	call strupr (cid)

	# Take out all characters not belonging to the [A-Z,0-9,+,-,_] set.
	op = 1
	for (i = 1; cid[i] != EOS; i = i + 1) {
	    if (IS_UPPER (cid[i]) || IS_DIGIT (cid[i]) || (cid[i] == '+') ||
	        (cid[i] == '-') || (cid[i] == '_')) {
		cid[op] = cid[i]
		op = op + 1
	    }
	}
	cid[op] = EOS

	# Return number of characters in compressed identifier
	return (op - 1)
end
