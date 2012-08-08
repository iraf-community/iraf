# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<plset.h>
include	<plio.h>

# PL_DEBUG -- Print debug information for a mask.  The information to be
# printed is selected by summing the following bitflags:
#
#	PD_SUMMARY		mask level summary information
#	PD_INDEX		print the line list index
#	PD_LLOUT		print the line lists, line list format
#	PD_RLOUT		print the line lists, range list format
#
# The mask is not modified in any way.  All output is on the given stream,
# formatted to the page width given by the 'width' argument.

procedure pl_debug (pl, fd, width, what)

pointer	pl			#I mask descriptor
int	fd			#I output file
int	width			#I max width of formatted output, columns
int	what			#I flags defining what to print

pointer	sp, buf, rng, rl, pp
int	line_1, line_2, nne, nv, v, lp, i
int	naxes, axlen, nlp, firstcol, maxcol, col, rlen
errchk	pl_valid, fprintf, pl_debugout, pll_prints, plr_printi
bool	pl_empty()
int	pl_l2ri()

define	index_ 91
define	lines_ 92
define	done_  93
define	llout_ 94

begin
	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)
	call salloc (rng, SZ_FNAME, TY_CHAR)
	call salloc (rl, RL_MAXLEN(pl), TY_INT)

	call pl_valid (pl)
	naxes = PL_NAXES(pl)
	axlen = PL_AXLEN(pl,1)
	nlp   = PL_NLP(pl)
	maxcol = width - 1

	# Output the summary information.
	# ---------------------------------

	if (and (what, PD_SUMMARY) == 0)
	    goto index_

	# Line 1 of summary.
	call fprintf (fd, "Mask %x naxes=%d [")
	    call pargi (pl)
	    call pargi (naxes)
	do i = 1, naxes {
	    call fprintf (fd, "%d%c")
		call pargi (PL_AXLEN(pl,i))
		if (i == naxes)
		    call pargi (']')
		else
		    call pargi (',')
	}
	call fprintf (fd, " maxval=%o")
	    call pargi (PL_MAXVAL(pl))
	call fprintf (fd, " plane=[")
	do i = 1, naxes {
	    call fprintf (fd, "%d%c")
		call pargi (PL_AXLEN(pl,i))
		if (i == naxes)
		    call pargi (']')
		else
		    call pargi (',')
	}
	call fprintf (fd, "\n")

	# Line 2 of summary.
	call fprintf (fd,
	    "max buffered line size %d, max actual line size %d\n")
	    call pargi (PL_MAXLINE(pl))
	    v = 0
	    nne = 0
	    do i = 1, nlp {
		lp = PL_LP(pl,i)
		nv = LP_LEN (Ref (pl, lp))
		if (nv > v)
		    v = nv
		if (lp != PL_EMPTYLINE)
		    nne = nne + 1
	    }
	    call pargi (v)

	# Line 3 of summary.
	call fprintf (fd, "%d lines total, %d are nonempty, mask is %s\n")
	    call pargi (nlp)
	    call pargi (nne)
	if (pl_empty (pl))
	    call pargstr ("empty")
	else
	    call pargstr ("nonempty")

	# Line 4 of summary.
	call fprintf (fd, "llbp=%x, len=%d, op=%d, free=%d, nupdates=%d\n")
	    call pargi (PL_LLBP(pl))
	    call pargi (PL_LLLEN(pl))
	    call pargi (PL_LLOP(pl))
	    call pargi (PL_LLFREE(pl))
	    call pargi (PL_LLNUPDATES(pl))

index_
	# Print index.
	# ---------------------------------

	if (and (what, PD_INDEX) == 0)
	    goto lines_

	call fprintf (fd, "Index at %x containing %d lines:\n")
	    call pargi (PL_LPP(pl))
	    call pargi (nlp)
	col = 1
	firstcol = 1
	do i = 1, nlp {
	    lp = PL_LP(pl,i)
	    call sprintf (Memc[buf], SZ_LINE, "%6d")
		call pargi (lp)
	    call pl_debugout (fd, Memc[buf], col, firstcol, maxcol)
	}
	call pl_debugout (fd, "", col, firstcol, maxcol)

lines_
	# Print the line list.
	# ---------------------------------

	if (and (what, PD_LLOUT+PD_RLOUT+PD_LHDR) == 0)
	    goto done_

	call fprintf (fd, "Line list containing %d lines:\n")
	    call pargi (nlp)

	line_1 = 0
	do i = 1, nlp + 1 {
	    if (i > nlp && line_1 != 0)
		goto llout_
	    lp = PL_LP(pl,i)

	    if (lp == PL_EMPTYLINE && line_1 == 0) {
		# Skip over an empty line.
		next
	    } else if (line_1 == 0) {
		# Begin a new region.
		line_1 = i
		line_2 = i
		if (i == nlp)
		    goto llout_
	    } else if (lp == PL_LP(pl,line_1)) {
		# Add line to current region.
		line_2 = i
		if (i == nlp)
		    goto llout_

	    } else {
		# Output a region.
llout_
		lp = PL_LP(pl,line_1)
		pp = Ref (pl, lp)

		if (line_1 == line_2) {
		    call sprintf (Memc[rng], SZ_FNAME, "[%d]")
			call pargi (line_1)
		} else {
		    call sprintf (Memc[rng], SZ_FNAME, "[%d:%d]")
			call pargi (line_1)
			call pargi (line_2)
		}

		if (and (what, PD_LHDR) != 0) {
		    call sprintf (Memc[buf], SZ_LINE,
			"%s%12tlp=%5d, nref=%d, blen=%d, len=%d")
			call pargstr (Memc[rng])
			call pargi (lp)
			call pargi (LP_NREF(pp))
			call pargi (LP_BLEN(pp))
			call pargi (LP_LEN(pp))
		} else
		    call strcpy (Memc[rng], Memc[buf], SZ_LINE)

		# Output the line list as a line list.
		firstcol = 12
		if (and (what, PD_LLOUT) != 0)
		    call pll_prints (LL(pl,lp), fd, Memc[buf], firstcol, maxcol)

		# Output as a range list.
		if (and (what, PD_RLOUT) != 0) {
		    rlen = pl_l2ri (LL(pl,lp), 1, Memi[rl], axlen)
		    call plr_printi (Memi[rl], fd, Memc[buf], firstcol, maxcol)
		}

		if (and (what, PD_RLOUT+PD_LLOUT) == 0) {
		    col = 1;  firstcol = 1
		    call pl_debugout (fd, Memc[buf], col, firstcol, maxcol)
		    call pl_debugout (fd, "", col, firstcol, maxcol)
		}

		if (PL_LP(pl,i) == PL_EMPTYLINE || line_2 == i)
		    line_1 = 0
		else {
		    line_1 = i
		    line_2 = i
		}
	    }
	}

done_
	call flush (fd)
	call sfree (sp)
end
