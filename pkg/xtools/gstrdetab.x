# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GSTRDETAB -- Procedure to remove tabs from a line of text.

int procedure gstrdetab (line, outline, maxch, tabs)

char   line[ARB], outline[ARB]
int    maxch, tabs[ARB]

int    ip, op

begin
	ip = 1
	op = 1

	while (line[ip] != EOS && op <= maxch) {
	    if (line[ip] == '\t') {
		repeat {
		    outline[op] = ' '
		    op = op + 1
		} until (tabs[op] == YES || op > maxch)
		ip = ip + 1
	    } else {
		outline[op] = line [ip]
		ip = ip + 1
		op = op + 1
	    }
	}

	outline[op] = EOS
	return (op-1)
end
