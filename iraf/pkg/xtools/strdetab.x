# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRDETAB -- Procedure to remove tabs from a line of text and replace with
# blanks.

procedure strdetab (line, outline, maxch, tabsize)

char	line[ARB], outline [ARB]
int	maxch, tabsize

int	ip, op
int	modi()

begin
	op=1
	ip=1

	while (line[ip] != EOS && op <= maxch) {
	    if (line[ip] == '\t') {
		repeat {
		    outline[op] = ' '
		    op = op + 1
		} until ((modi(op, tabsize) == 1) || (op > maxch)) 
	        ip = ip + 1
	    } else {
		outline[op] = line[ip]
		op = op + 1
		ip = ip + 1
	    }
	}

	outline[op] = EOS
end
