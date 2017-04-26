# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# STRENTAB -- Procedure to replace blanks with tabs and blanks.

procedure strentab (line, outline, maxch, tabsize)

int   maxch, tabsize
char  line[ARB], outline[ARB]

int   ip, op, ltab

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
