# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PLR_PRINT -- Print a range list on the given output stream.

procedure plr_printl (rl, fd, label, firstcol, maxcol)

long	rl[3,ARB]		#I range list
int	fd			#I output file
char	label[ARB]		#I line label
int	firstcol		#I first column for output
int	maxcol			#I width of formatted output

pointer	sp, buf
int	col, rn, r_len, x, n, pv
int	strlen()

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	# Output the line label and advance to the first column.  If the label
	# extends beyond the first column, start a new line.

	call putline (fd, label)
	col = strlen (label) + 1
	if (col > firstcol)
	    call pl_debugout (fd, "", col, firstcol, maxcol)

	r_len = RL_LEN(rl)

	# Decode the range list proper.
	do rn = RL_FIRST, r_len {
	    x  = RL_X(rl,rn)
	    n  = RL_N(rl,rn)
	    pv = RL_V(rl,rn)

	    if (n == 1) {
		call sprintf (Memc[buf], SZ_LINE, "%d(%d)")
		    call pargi (x)
		    call pargi (pv)
	    } else {
		call sprintf (Memc[buf], SZ_LINE, "%d-%d(%d)")
		    call pargi (x)
		    call pargi (x+n-1)
		    call pargi (pv)
	    }

	    call pl_debugout (fd, Memc[buf], col, firstcol, maxcol)
	}

	call pl_debugout (fd, "", col, firstcol, maxcol)
	call sfree (sp)
end
