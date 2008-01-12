include "tbtables.h"
include "tblerr.h"

# tbysft -- Y shift rows
# Shift one or more rows down (to leave a gap in the table) or up (to
# delete rows).  The range of rows that is shifted is from FIRST to
# the last row in the table.  Shift down if SHIFT > 0, or shift up if
# SHIFT < 0.  SHIFT is the number of rows by which to shift.
#
# If SHIFT > 0 rows that are exposed by the shift are NOT set to indef.
# If SHIFT < 0 rows at the end WILL be set to indef.
# In either case the number of rows TB_NROWS(tp) will be updated.
#
# Phil Hodge, 23-Mar-1988  Subroutine created.
# Phil Hodge,  1-Apr-1993  Include short datatype; errchk tbegp[] & tbepp[].
# Phil Hodge, 29-Jul-1994  Change calling sequence of tbeoff.
# Phil Hodge,  5-Mar-1998  Replace tbytsz by tbywer.

procedure tbysft (tp, first, shift)

pointer tp			# i: pointer to table descriptor
int	first			# i: first row to be affected by the shift
int	shift			# i: shift by this many rows
#--
pointer cptr			# pointer to a column descriptor
pointer v			# pointer to array of values
pointer vj			# pointer which is incremented in loop
int	abs_shift		# absolute value of shift
int	row1, row2		# range of rows to be copied
int	nrows			# number of rows written to table
int	nvals			# number of values in scratch array
int	dtype			# data type of a column
int	col_width		# space in char for an element in table
int	col			# loop index for column number
int	k			# loop index
long	i_offset		# offset in char to a table element
long	o_offset		# offset in char to a table element
long	tbeoff()
pointer tbcnum()
errchk	tbegpb, tbegpd, tbegpi, tbegps, tbegpr, tbegpt,
	tbeppb, tbeppd, tbeppi, tbepps, tbeppr, tbeppt,
	tbywer

begin
	nrows = TB_NROWS(tp)

	# Make sure there are enough rows allocated in the table.
	if (first > nrows) {
	    if (shift > 0)
		call tbywer (tp, shift+first-1)
	    return					# nothing else to do
	} else {
	    call tbywer (tp, shift+nrows)
	}


	# First consider the case of deleting all rows starting with FIRST.
	if (nrows + shift < first) {
	    call tbynll (tp, first, nrows)		# set to INDEF
	    TB_NROWS(tp) = max (0, first-1)
	    return
	}

	if (shift > 0)
	    TB_NROWS(tp) = TB_NROWS(tp) + shift

	abs_shift = abs (shift)

	# Rows row1:row2 will be copied to row1+shift:row2+shift.
	if (shift < 0) {
	    row1 = first + abs_shift
	    row2 = nrows
	} else {
	    row1 = first
	    row2 = nrows
	}
	nvals = row2 - row1 + 1

	do col = 1, TB_NCOLS(tp) {

	    cptr = tbcnum (tp, col)
	    dtype = COL_DTYPE(cptr)
	    col_width = COL_LEN(cptr)

	    switch (dtype) {
	    case TY_REAL:
		call malloc (v, nvals, TY_REAL)
		vj = v					# incremented in loop
		i_offset = tbeoff (tp, cptr, row1, 1)
		do k = row1, row2 {
		    call tbegpr (tp, cptr, i_offset, k, Memr[vj])	# get
		    vj = vj + 1
		    i_offset = i_offset + col_width
		}
		vj = v					# incremented in loop
		o_offset = tbeoff (tp, cptr, row1+shift, 1)
		do k = row1+shift, row2+shift {
		    call tbeppr (tp, cptr, o_offset, k, Memr[vj])	# put
		    vj = vj + 1
		    o_offset = o_offset + col_width
		}
		call mfree (v, TY_REAL)

	    case TY_DOUBLE:
		call malloc (v, nvals, TY_DOUBLE)
		vj = v
		i_offset = tbeoff (tp, cptr, row1, 1)
		do k = row1, row2 {
		    call tbegpd (tp, cptr, i_offset, k, Memd[vj])
		    vj = vj + 1
		    i_offset = i_offset + col_width
		}
		vj = v
		o_offset = tbeoff (tp, cptr, row1+shift, 1)
		do k = row1+shift, row2+shift {
		    call tbeppd (tp, cptr, o_offset, k, Memd[vj])
		    vj = vj + 1
		    o_offset = o_offset + col_width
		}
		call mfree (v, TY_DOUBLE)

	    case TY_INT:
		call malloc (v, nvals, TY_INT)
		vj = v
		i_offset = tbeoff (tp, cptr, row1, 1)
		do k = row1, row2 {
		    call tbegpi (tp, cptr, i_offset, k, Memi[vj])
		    vj = vj + 1
		    i_offset = i_offset + col_width
		}
		vj = v
		o_offset = tbeoff (tp, cptr, row1+shift, 1)
		do k = row1+shift, row2+shift {
		    call tbeppi (tp, cptr, o_offset, k, Memi[vj])
		    vj = vj + 1
		    o_offset = o_offset + col_width
		}
		call mfree (v, TY_INT)

	    case TY_SHORT:
		call malloc (v, nvals, TY_SHORT)
		vj = v
		i_offset = tbeoff (tp, cptr, row1, 1)
		do k = row1, row2 {
		    call tbegps (tp, cptr, i_offset, k, Mems[vj])
		    vj = vj + 1
		    i_offset = i_offset + col_width
		}
		vj = v
		o_offset = tbeoff (tp, cptr, row1+shift, 1)
		do k = row1+shift, row2+shift {
		    call tbepps (tp, cptr, o_offset, k, Mems[vj])
		    vj = vj + 1
		    o_offset = o_offset + col_width
		}
		call mfree (v, TY_SHORT)

	    case TY_BOOL:
		call malloc (v, nvals, TY_BOOL)
		vj = v
		i_offset = tbeoff (tp, cptr, row1, 1)
		do k = row1, row2 {
		    call tbegpb (tp, cptr, i_offset, k, Memb[vj])
		    vj = vj + 1
		    i_offset = i_offset + col_width
		}
		vj = v
		o_offset = tbeoff (tp, cptr, row1+shift, 1)
		do k = row1+shift, row2+shift {
		    call tbeppb (tp, cptr, o_offset, k, Memb[vj])
		    vj = vj + 1
		    o_offset = o_offset + col_width
		}
		call mfree (v, TY_BOOL)

	    default:
		if (dtype >= 0 && dtype != TY_CHAR)
		    call error (ER_TBCOLBADTYP,
				"tbysft:  table or memory corrupted?")
		call malloc (v, SZ_LINE, TY_CHAR)
		if (shift < 0) {
		    i_offset = tbeoff (tp, cptr, row1, 1)
		    o_offset = tbeoff (tp, cptr, row1-abs_shift, 1)
		    do k = row1, row2 {
			call tbegpt (tp, cptr, i_offset, k, Memc[v], SZ_LINE)
			call tbeppt (tp, cptr, o_offset, k, Memc[v])
			i_offset = i_offset + col_width
			o_offset = o_offset + col_width
		    }
		} else {
		    i_offset = tbeoff (tp, cptr, row2, 1)
		    o_offset = tbeoff (tp, cptr, row2+shift, 1)
		    # (actually, it's the offsets that count; k is ignored)
		    do k = row2+shift, row1+shift, -1 {
			call tbegpt (tp, cptr, i_offset, k, Memc[v], SZ_LINE)
			call tbeppt (tp, cptr, o_offset, k, Memc[v])
			i_offset = i_offset - col_width
			o_offset = o_offset - col_width
		    }
		}
		call mfree (v, TY_CHAR)
	    }
	}

	# If rows were deleted, set the extra rows at end to indef,
	# and change the value of TB_NROWS(tp).
	if (shift < 0) {
	    call tbynll (tp, nrows-abs_shift+1, nrows)
	    TB_NROWS(tp) = max (0, nrows - abs_shift)
	}
end
