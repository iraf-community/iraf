# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRDETAB -- Procedure to remove tabs from a line of text and replace with
# blanks.

procedure strdetab (line, outline, maxch, tabsize)

int	ip, op, maxch, tabsize
char	line[ARB], outline [ARB]

begin
	op=1
	ip=1

	while (line[ip] != EOS && op <= maxch) {
	    if (line[ip] == '\t') {
		repeat {
		    outline[op] = ' '
		    op = op + 1
		} until ((mod (op, tabsize) == 1) || (op > maxch)) 
	        ip = ip + 1
	    } else {
		outline[op] = line[ip]
		op = op + 1
		ip = ip + 1
	    }
	}

	outline[op] = EOS
end


# STRENTAB -- Procedure to replace blanks with tabs and blanks.

procedure strentab (line, outline, maxch, tabsize)

int	maxch, tabsize
char	line[ARB], outline[ARB]
int	ip, op, ltab

begin
	op = 1
	ip = 1

	repeat {
	    ltab = ip
	    while (line[ltab] == ' ' && op <= maxch) {
		ltab = ltab + 1
		if (mod(ltab, tabsize) == 1) {
		    outline[op] = '\t'
		    ip = ltab
		    op = op + 1
		}
	    }
	    for (; ip < ltab && op <= maxch; ip = ip + 1) {
		outline[op] = ' '
		op = op + 1
	    }
	    if (line[ip] == EOS || op >= maxch+1)
		break
	    outline[op] = line[ip]
	    op = op + 1
	    ip = ip + 1
	} until (line[ip] == EOS || op >= maxch+1)

	outline[op] = EOS
end
