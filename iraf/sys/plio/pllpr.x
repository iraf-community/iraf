# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PLL_PRINTS -- Print a line list on the given output stream.

procedure pll_prints (ll, fd, label, firstcol, maxcols)

short	ll[ARB]			#I line list
int	fd			#I output file
char	label[ARB]		#I line label
int	firstcol		#I first column for output
int	maxcols			#I width of formatted output

pointer	sp, buf
bool	skipword
int	opcode, data
int	ll_len, ll_first, col, ip, pv, x
int	strlen()

begin
	call smark (sp)
	call salloc (buf, SZ_FNAME, TY_CHAR)

	# Support old format line lists.
	if (LL_OLDFORMAT(ll)) {
	    ll_len = OLL_LEN(ll)
	    ll_first = OLL_FIRST
	} else {
	    ll_len = LL_LEN(ll)
	    ll_first = LL_FIRST(ll)
	}

	# Output the line label and advance to the first column.  If the label
	# extends beyond the first column, start a new line.

	call putline (fd, label)
	col = strlen (label) + 1
	if (col > firstcol)
	    call pl_debugout (fd, "", col, firstcol, maxcols)

	skipword = false
	pv = 1
	x = 1

	# Decode the line list proper.
	do ip = ll_first, ll_len {
	    if (skipword) {
		skipword = false
		next
	    }

	    opcode = I_OPCODE(ll[ip])
	    data   = I_DATA(ll[ip])

	    switch (opcode) {
	    case I_ZN:
		x = x + data
		call sprintf (Memc[buf], SZ_FNAME, "Z%d")
		    call pargi (data)
	    case I_HN:
		x = x + data
		call sprintf (Memc[buf], SZ_FNAME, "H%d")
		    call pargi (data)
	    case I_PN:
		x = x + data
		call sprintf (Memc[buf], SZ_FNAME, "P%d")
		    call pargi (data)

	    case I_SH:
		pv = (int(ll[ip+1]) * I_SHIFT) + data
		skipword = true
		call sprintf (Memc[buf], SZ_FNAME, "SH(%d)")
		    call pargi (pv)
	    case I_IH:
		pv = pv + data
		call sprintf (Memc[buf], SZ_FNAME, "IH%d(%d)")
		    call pargi (data)
		    call pargi (pv)
	    case I_DH:
		pv = pv - data
		call sprintf (Memc[buf], SZ_FNAME, "DH%d(%d)")
		    call pargi (data)
		    call pargi (pv)

	    case I_IS, I_DS:
		x = x + 1
		if (opcode == I_IS) {
		    pv = pv + data
		    call sprintf (Memc[buf], SZ_FNAME, "IS%d(%d)")
			call pargi (data)
			call pargi (pv)
		} else {
		    pv = pv - data
		    call sprintf (Memc[buf], SZ_FNAME, "DS%d(%d)")
			call pargi (data)
			call pargi (pv)
		}
	    }

	    call pl_debugout (fd, Memc[buf], col, firstcol, maxcols)
	}

	call sprintf (Memc[buf], SZ_FNAME, "(%d,%d)")
	    call pargi (x - 1)
	    call pargi (pv)
	call pl_debugout (fd, Memc[buf], col, firstcol, maxcols)

	call pl_debugout (fd, "", col, firstcol, maxcols)
	call sfree (sp)
end
