# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GAXMAP -- Get the axis map.  This assigns a logical axis axno[i] to
# each physical axis I.  If axno[i]=0, the value of the physical axis
# coordinate is the constant axval[i], and the dimension of the logical
# system is reduced by one.

procedure mw_gaxmap (mw, axno, axval, ndim)

pointer	mw			#I pointer to MWCS descriptor
int	axno[ndim]		#O physical -> logical axis assignments
int	axval[ndim]		#O value of physical axis if axno=0
int	ndim			#I physical dimension of axis map

int	i
errchk	syserrs

begin
	# Verify dimension.
	if (MI_NDIM(mw) != ndim)
	    call syserrs (SYS_MWNDIM, "mw_gaxmap")

	# Copy out the current axis map.
	do i = 1, ndim {
	    axno[i] = MI_AXNO(mw,i)
	    axval[i] = MI_AXVAL(mw,i)
	}
end
