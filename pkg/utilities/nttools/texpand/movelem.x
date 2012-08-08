include <tbset.h>

# MOV_ELEM -- Move an element in one row from one table to another
#
# B.Simon	15-Jan-99	Original
# B.Simon	27-Jan-99	Renamed

procedure mov_elem (rtp, rcp, rrow, wtp, wcp, wrow)

pointer	rtp		# i: Table descriptor of table read from
pointer	rcp		# i: Column descriptor of column read from
int	rrow		# i: Row number of table read from
pointer	wtp		# i: Table descriptor of table written to
pointer	wcp		# i: Column descriptor of column written to
int	wrow		# i: Row number of table written to
#--
int	type, nelem, sz_elem, nlen
pointer	sp, buf

int	tbcigi(), tbagtb(), tbagtt(), tbagti(), tbagtr(), tbagtd(), tbagts()

begin
	# First, get the type and number of elements in the column

	nelem = tbcigi (rcp, TBL_COL_LENDATA)
	type = tbcigi (rcp, TBL_COL_DATATYPE)

	if (type < 0) {
	    sz_elem = - type 
	    type = TY_CHAR
	} else {
	    sz_elem = 0
	}

	# Allocate buffer to hold values passed between tables

	call smark (sp)
	call salloc (buf, nelem*(sz_elem+1), type)

	# Copy the values according to their actual type

	if (nelem == 1) {
	    # Do not copy null scalar values

	    switch (type) {
	    case TY_BOOL:
		call tbegtb (rtp, rcp, rrow, Memb[buf])
		call tbeptb (wtp, wcp, wrow, Memb[buf])

	    case TY_CHAR:
		call tbegtt (rtp, rcp, rrow, Memc[buf], sz_elem)
		if (Memc[buf] != EOS)
		    call tbeptt (wtp, wcp, wrow, Memc[buf])

	    case TY_SHORT:
		call tbegts (rtp, rcp, rrow, Mems[buf])
		if (! IS_INDEFS (Mems[buf]))
		    call tbepts (wtp, wcp, wrow, Mems[buf])

	    case TY_INT, TY_LONG:
		call tbegti (rtp, rcp, rrow, Memi[buf])
		if (! IS_INDEFI (Memi[buf]))
		    call tbepti (wtp, wcp, wrow, Memi[buf])

	    case TY_REAL:
		call tbegtr (rtp, rcp, rrow, Memr[buf])
		if (! IS_INDEFR (Memr[buf]))
		    call tbeptr (wtp, wcp, wrow, Memr[buf])

	    case TY_DOUBLE:
		call tbegtd (rtp, rcp, rrow, Memd[buf])
		if (! IS_INDEFD (Memd[buf]))
		    call tbeptd (wtp, wcp, wrow, Memd[buf])
	    }

	} else {
	    # Don't copy zero length arrays

	    switch (type) {
	    case TY_BOOL:
		nlen = tbagtb (rtp, rcp, rrow, Memb[buf], 1, nelem)
		call tbaptb (wtp, wcp, wrow, Memb[buf], 1, nlen)

	    case TY_CHAR:
		nlen = tbagtt (rtp, rcp, rrow, Memc[buf], sz_elem, 1, nelem)
		if (Memc[buf] != EOS)
		    call tbaptt (wtp, wcp, wrow, Memc[buf], sz_elem, 1, nlen)

	    case TY_SHORT:
		nlen = tbagts (rtp, rcp, rrow, Mems[buf], 1, nelem)
		if (! IS_INDEFS (Mems[buf]))
		    call tbapts (wtp, wcp, wrow, Mems[buf], 1, nlen)

	    case TY_INT, TY_LONG:
		nlen = tbagti (rtp, rcp, rrow, Memi[buf], 1, nelem)
		if (! IS_INDEFI (Memi[buf]))
		    call tbapti (wtp, wcp, wrow, Memi[buf], 1, nlen)

	    case TY_REAL:
		nlen = tbagtr (rtp, rcp, rrow, Memr[buf], 1, nelem)
		if (! IS_INDEFR (Memr[buf]))
		    call tbaptr (wtp, wcp, wrow, Memr[buf], 1, nlen)

	    case TY_DOUBLE:
		nlen = tbagtd (rtp, rcp, rrow, Memd[buf], 1, nelem)
		if (! IS_INDEFD (Memd[buf]))
		    call tbaptd (wtp, wcp, wrow, Memd[buf], 1, nlen)
	    }
	}

	call sfree (sp)
end

