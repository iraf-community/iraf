# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include <plset.h>
include	<plio.h>

# PL_ACCESS -- Return a pointer (type short) to the encoded line list data
# for the indicated mask image line.  A valid pointer is always returned;
# if the mask line is empty, the pointer will point to "empty line" linelist.

pointer procedure pl_access (pl, v)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I coordinates of desired line

int	pl_reference()

begin
	return (Ref (pl, pl_reference(pl,v)))
end


# PL_REFERENCE -- Return a reference (llbuf offset) to the indicated mask
# image line.  A valid offset is always returned; if the mask line is empty,
# the offset will be that of the "empty line" linelist.

int procedure pl_reference (pl, v)

pointer	pl			#I mask descriptor
long	v[PL_MAXDIM]		#I coordinates of desired line

int	index, i
int	totlen, axlen
define	oob_ 91

begin
	# Compute the index of the line in the line pointer array.
	if (PL_NAXES(pl) == 2) {
	    # Optimized for case naxes=2.
	    index = v[2]
	    if (index < 1 || index > PL_AXLEN(pl,2))
		goto oob_
	} else {
	    # General case.
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

	return (PL_LP(pl,index))
oob_
	call syserr (SYS_PLREFOOB)
end
