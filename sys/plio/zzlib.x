# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plio.h>

# PL_CHKFREE -- Verify that the count of free space in the linelist is
# correct. (DEBUG TOOL).

procedure pl_chkfree (pl, msg)

pointer	pl			#I mask descriptor
char	msg[ARB]		#I message to be printed if error occurs

pointer	o_lp
short	nref[8192], nl
int	used, free, nused, nfree, b_len, i

begin
	used = 0;  nused = 0
	free = 0;  nfree = 0

	# Count the space in the active line lists.
	nl = 0
	for (i=0;  i < PL_LLOP(pl);  i=i+b_len) {
	    o_lp = Ref (pl, i)
	    b_len = LP_BLEN(o_lp)

	    nl = nl + 1
	    nref[nl] = LP_NREF(o_lp)

	    if (LP_NREF(o_lp) < 0) {
		call eprintf ("lineoff %d, nref = %d\n")
		    call pargi (i)
		    call pargi (LP_NREF(o_lp))
		free = free + b_len
		nfree = nfree + 1
	    } else if (i == PL_EMPTYLINE || LP_NREF(o_lp) > 0) {
		used = used + b_len
		nused = nused + 1
	    } else {
		free = free + b_len
		nfree = nfree + 1
	    }
	}

	if (free != PL_LLFREE(pl)) {
	    call eprintf ("CHKFREE (%s): used=%d,%d, free=%d,%d, ")
		call pargstr (msg)
		call pargi (used)
		call pargi (nused)
		call pargi (free)
		call pargi (nfree)
	    call eprintf ("PL_LLFREE=%d, OP-FREE=%d\n")
		call pargi (PL_LLFREE(pl))
		call pargi (PL_LLOP(pl) - PL_LLFREE(pl))

	    do i = 1, nl {
		call eprintf ("%4d")
		    call pargs (nref[i])
		if (mod (i,19) == 0)
		    call eprintf ("\n")
	    }
	    call eprintf ("\n")
	}
end
