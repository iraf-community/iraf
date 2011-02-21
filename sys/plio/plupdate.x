# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include <plset.h>
include	<plio.h>

# PL_UPDATE -- Update the indicated line of a mask, i.e., insert a possibly
# modified line (encoded line list) into an image mask.  This is where image
# compression in the Y direction occurs: if the line to be inserted is the
# "empty line", or a copy of the adjacent line earlier in the image, then we
# merely set the new line pointer to point to the line already stored, and
# increment the reference count for that line.  When a new mask is created,
# all lines point to the "empty line" line list (PL_EMPTYLINE), which has a
# reference count equal to the number of lines in the mask.
#
# A new line may replace an existing line in storage if space permits and the
# reference count of the existing line is one or less, i.e., the stored line
# list is used only by the image line being updated.  Otherwise the new line
# list will be appended to the line list buffer, which is resized to a larger
# size if it overflows.  Note that the resize may move the buffer, which is
# why all line "pointers" are actually offsets into the line list buffer.
#
# We do not perform garbage collection on freed lines, rather we keep a count
# of the total amount of freed space (which cannot be reused), allowing the
# entire line list to be rebuilt periodically to reclaim the space, when the
# percentage of wasted space rises to a certain level.  Since this can be a
# relatively expensive operation it is only performed by high level, more
# macroscopic operators, e.g., following a full mask rasterop, or before
# writing the mask to external storage.  Note also that rebulding the mask
# may shift the stored line lists about, invalidating any line list pointers
# that may be cached in the calling procedure.  This is another reason why
# we do not perform mask compression automatically if buffer overflow occurs
# during a line update.

procedure pl_update (pl, v, ll)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I coordinates of line in the mask
short	ll[ARB]			#I encoded line list

pointer	o_pp, n_pp
int	totlen, axlen, index, i
int	o_lp, n_lp, o_len, n_len, b_len

bool	pll_equal()
int	pl_alloc()
errchk	pl_alloc
define	update_ 91
define	oob_ 92

begin
	# Compute the index of the line in the line pointer array.
	if (PL_NAXES(pl) == 2) {
	    # Optimized for case naxes=2.
	    index = v[2]
	    if (index < 1 || index > PL_AXLEN(pl,2))
		goto oob_
	} else {
	    # Generalized code.
	    index = 1
	    totlen = 1
	    do i = 2, PL_NAXES(pl) {
		axlen = PL_AXLEN(pl,i)
		if (v[i] < 1 || v[i] > axlen)
		    goto oob_
		index = index + totlen * (v[i] - 1)
		totlen = totlen * axlen
	    }
	}

	# Now the pointer to the current stored line list.
	o_lp = PL_LP(pl,index)
	o_pp = Ref (pl, o_lp)
	if (o_pp == NULL)
	    return

	# Has the line been modified?
	n_len = LL_LEN(ll)
	if (n_len == LP_LEN(o_pp))
	    if (pll_equal (ll, LL(pl,o_lp)))
		return

	# Keep track of the number of edits.
	PL_LLNUPDATES(pl) = PL_LLNUPDATES(pl) + 1

	# Is the new line empty?
	n_lp = PL_EMPTYLINE
	n_pp = Ref (pl, n_lp)
	if (n_len == LP_LEN(n_pp))
	    if (pll_equal (ll, LL(pl,n_lp)))
		goto update_

	# Is the new line a copy of the adjacent line (Y=Y-1) in the image?
	# Due to the short integer encoding a maximum of MAX_SHORT references
	# are allowed per line.

	if (index > 1) {
	    n_lp = PL_LP(pl,index-1)
	    n_pp = Ref (pl, n_lp)
	    if (LP_NREF(n_pp) < MAX_SHORT)
		if (n_len == LP_LEN(n_pp))
		    if (pll_equal (ll, LL(pl,n_lp)))
			goto update_
	}

	# The new line isn't a copy of the empty line or of an adjacent line,
	# so copy it into the line list buffer.

	b_len = LP_BLEN(o_pp)
	if (LP_NREF(o_pp) <= 1 && n_len <= b_len && o_lp != PL_EMPTYLINE) {
	    # Overwrite existing line.

	    o_len = LP_LEN(o_pp)
	    call amovs (ll, LL(pl,o_lp), n_len)
	    LP_NREFS(o_pp) = 1
	    LP_SETBLEN(o_pp, b_len)
	    PL_LLFREE(pl) = PL_LLFREE(pl) + (o_len - LP_LEN(o_pp))
	    return

	} else {
	    # Add a new line.

	    n_lp = pl_alloc (pl, n_len)
	    o_pp = Ref (pl, o_lp)
	    n_pp = Ref (pl, n_lp)
	    call amovs (ll, LL(pl,n_lp), n_len)

	    LP_NREFS(n_pp) = 0
	    LP_SETBLEN(n_pp, n_len)
	    PL_LLFREE(pl) = PL_LLFREE(pl) + n_len
	}

	# We get here only if the new line has already been stored in the
	# linelist buffer, with a pointer to such in N_LP, and a pointer to
	# the old line in O_LP.  Dereference the old line, which the new line
	# is no longer associated with, and increase the reference count of
	# the new line.
update_

	# Dereference the old line and free the space if it is no longer used.
	# If the old line buffer is freed we reclaim only LP_LEN words, since
	# we already reclaimed LP_BLEN-LP_LEN in an earlier edit operation.

	LP_NREFS(o_pp) = LP_NREFS(o_pp) - 1
	if (LP_NREF(o_pp) == 0 && o_lp != PL_EMPTYLINE)
	    PL_LLFREE(pl) = PL_LLFREE(pl) + LP_LEN(o_pp)

	# Add another reference to the new line.
	LP_NREFS(n_pp) = LP_NREFS(n_pp) + 1
	if (LP_NREF(n_pp) == 1 && n_lp != PL_EMPTYLINE)
	    PL_LLFREE(pl) = PL_LLFREE(pl) - LP_BLEN(n_pp)

	PL_LP(pl,index) = n_lp
	return
oob_
	call syserr (SYS_PLREFOOB)
end
