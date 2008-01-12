# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SAXMAP -- Set the axis map.  This assigns a logical axis axno[i] to
# each physical axis I.  If axno[i]=0, the value of the physical axis
# coordinate is the constant axval[i], and the dimension of the logical
# system is reduced by one.  Setting the axis map automatically enables
# axis mapping if a nonstandard map is entered.

procedure mw_saxmap (mw, axno, axval, ndim)

pointer	mw			#I pointer to MWCS descriptor
int	axno[ndim]		#I physical -> logical axis assignments
int	axval[ndim]		#I value of physical axis if axno=0
int	ndim			#I physical dimension of axis map

int	i, j
errchk	syserrs, syserr

begin
	# Verify dimension.
	if (MI_NDIM(mw) != ndim)
	    call syserrs (SYS_MWNDIM, "mw_saxmap")

	# Store the arrays, and determine the dimension of the logical system.
	# Enable axis mapping if an interesting map has been entered.

	MI_NLOGDIM(mw) = 0
	MI_USEAXMAP(mw) = NO

	do i = 1, ndim {
	    MI_AXNO(mw,i) = axno[i]
	    MI_AXVAL(mw,i) = axval[i]
	    if (axno[i] > 0)
		MI_NLOGDIM(mw) = MI_NLOGDIM(mw) + 1
	    if (axno[i] != i)
		MI_USEAXMAP(mw) = YES
	}

	# Invert the axis map to facilitate logical->physical mappings.
	do j = 1, MI_NLOGDIM(mw) {
	    for (i=1;  i <= ndim;  i=i+1)
		if (axno[i] == j) {
		    MI_PHYSAX(mw,j) = i
		    break
		}
	    if (i > ndim)
		call syserr (SYS_MWINVAXMAP)
	}
end
