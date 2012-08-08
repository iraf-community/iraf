include "tbtables.h"

# This file contains tbfiga and tbfisa for getting or setting the TDIM
# keyword in a FITS file.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  5-Aug-1999  Use COL_NELEM instead of tbalen to get array length.

# tbfiga -- get dimension of array and length of each axis

procedure tbfiga (tp, cp, ndim, axlen, maxdim)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	ndim		# o: dimension of array
int	axlen[maxdim]	# o: length of each axis
int	maxdim		# i: size of axlen array
#--
int	status		# zero is OK
errchk	tbferr

begin
	if (!TB_IS_OPEN(tp)) {
	    ndim = 1
	    axlen[1] = COL_NELEM(cp)
	    return
	}

	status = 0

	call fsgtdm (TB_FILE(tp), COL_NUMBER(cp), maxdim, ndim, axlen, status)
	if (status != 0)
	    call tbferr (status)
end

# tbfisa -- set dimension of array and length of each axis

procedure tbfisa (tp, cp, ndim, axlen)

pointer tp		# i: pointer to table descriptor
pointer cp		# i: pointer to column descriptor
int	ndim		# i: dimension of array
int	axlen[ARB]	# i: length of each axis
#--
int	status		# zero is OK
errchk	tbferr

begin
	if (!TB_IS_OPEN(tp))
	    return

	status = 0

	call fsptdm (TB_FILE(tp), COL_NUMBER(cp), ndim, axlen, status)
	if (status != 0)
	    call tbferr (status)
end
