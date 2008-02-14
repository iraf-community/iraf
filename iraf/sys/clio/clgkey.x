# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<clset.h>

# CLGKEY -- Return the next keystroke value from a list structured `ukey' type
# parameter.

int procedure clgkey (param, key, strval, maxch)

char	param[ARB]		# parameter to be read
int	key			# keystroke value of cursor event
char	strval[ARB]		# string value, if any
int	maxch

char	ch
int	nitems, op
pointer	sp, buf, ip
int	cctoc(), clglstr()
int	clstati(), rdukey()
define	quit_ 91

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Flush any buffered text output.
	call flush (STDERR)
	call flush (STDOUT)

	# Read the keyboard in raw mode.
	if (clstati (CL_PRTYPE) == PR_CONNECTED) {
	    if (clglstr (param, Memc[buf], SZ_LINE) == EOF) {
		call sfree (sp)
		return (EOF)
	    }
	} else {
	    if (rdukey (Memc[buf], SZ_LINE) == EOF) {
		call sfree (sp)
		return (EOF)
	    }
	}

	ip = buf
	nitems = 0
	if (cctoc (Memc, ip, ch) == 0)
	    goto quit_
	key = ch
	nitems = nitems + 1

	while (IS_WHITE (Memc[ip]))
	    ip = ip + 1
	if (Memc[ip] != '\n' && Memc[ip] != EOS) {
	    op = 1
	    while (op <= maxch && Memc[ip] != '\n' && Memc[ip] != EOS) {
		strval[op] = Memc[ip]
		op = op + 1
		ip = ip + 1
	    }
	    strval[op] = EOS
	    nitems = nitems + 1
	}

quit_
	call sfree (sp)
	return (nitems)
end
