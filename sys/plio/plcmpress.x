# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<plio.h>

# PL_COMPRESS -- Compress the line list buffer to eliminate any unusable
# space, as is generated when mask lines are modified and the old line lists
# are freed.  The compress operation may move the line lists about, possibly
# changing the line offsets.

procedure pl_compress (pl)

pointer	pl			#I mask descriptor

pointer	n_bp, o_lp, n_lp, op
int	nwords, r_len, b_len, i
errchk	malloc, mfree, syserr

begin
	# Redundant calls are ignored.
	if (PL_LLNUPDATES(pl) <= 0)
	    return

        # Return if there was a prior error with plalloc.
	if (PL_LLBP(pl) == NULL)
	    call syserr (SYS_PLBADMASK)

	# Count the total space in the active line lists.
	nwords = 0
	for (i=0;  i < PL_LLOP(pl);  i=i+b_len) {
	    o_lp = Ref (pl, i)
	    b_len = LP_BLEN(o_lp)
	    if (b_len <= 0 || b_len > PL_LLOP(pl))
		call syserr (SYS_PLBADMASK)
	    if (i == PL_EMPTYLINE || LP_NREF(o_lp) > 0)
		nwords = nwords + LP_LEN(o_lp)
	}

	# Verify that the free space accounting is correct.
	if (nwords != (PL_LLOP(pl) - PL_LLFREE(pl)))
	    call eprintf ("Warning: PL_LLFREE inconsistent (recoverable)\n")

	# Allocate a new buffer large enough to hold the compressed line list.
	call malloc (n_bp, nwords, TY_SHORT)

	# Copy the active line lists to the new buffer; as each line is
	# copied, overwrite a couple words of the old line list with the
	# new offset of the line.

	op = 0
	for (i=0;  i < PL_LLOP(pl);  i=i+b_len) {
	    o_lp = Ref (pl, i)
	    b_len = LP_BLEN(o_lp)
	    if (b_len <= 0 || b_len > PL_LLOP(pl))
		call syserr (SYS_PLBADMASK)

	    if (i == PL_EMPTYLINE || LP_NREF(o_lp) > 0) {
		n_lp = n_bp + op
		r_len = LP_LEN(o_lp)

		# The following should not be possible, barring a bug.
		if (op + r_len > nwords)
		    call fatal (pl, "pl_compress: llbuf overflow")

		call amovs (Mems[o_lp], Mems[n_lp], r_len)

		LP_NREFS(o_lp) = op / I_SHIFT
		LP_SETBLEN(o_lp, mod (op, I_SHIFT))
		LP_SETBLEN(n_lp, r_len)
		op = op + r_len
	    }
	}
		
	# Fix up the line index by accessing the tag word in the old line list
	# in the old buffer to get the line list offset in the new buffer.

	do i = 1, PL_NLP(pl) {
	    o_lp = Ref (pl, PL_LP(pl,i))
	    PL_LP(pl,i) = LP_NREF(o_lp) * I_SHIFT + LP_BLEN(o_lp)
	}
	    
	# Deallocate the old buffer and install the new one.
	call mfree (PL_LLBP(pl), TY_SHORT)

	PL_LLBP(pl) = n_bp
	PL_LLOP(pl) = op
	PL_LLLEN(pl) = nwords
	PL_LLFREE(pl) = 0
	PL_LLNUPDATES(pl) = 0
end
