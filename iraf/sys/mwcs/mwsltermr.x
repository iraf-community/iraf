# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SLTERMR -- Set the Lterm, single precision version.  Since all floating
# data is stored as double internally, a real->double conversion is involved,
# but no precision is lost provided single precision is adequate to describe
# the input data (an example of a case where precision is lost is a rotation,
# where there is a difference between the single and double precision version
# of, e.g., "sin(theta)").

procedure mw_sltermr (mw, ltm, ltv, ndim)

pointer	mw			#I pointer to MWCS descriptor
real	ltm[ndim,ndim]		#I linear transformation matrix
real	ltv[ndim]		#I translation vector
int	ndim			#I dimensionality of system

int	nelem
pointer	mw_allocd()
errchk	syserrs, mw_allocd

begin
	# The dimensionality of the data must match that of the current Lterm.
	if (ndim != MI_NDIM(mw))
	    call syserrs (SYS_MWNDIM, "mw_sltermr")

	# Copy in the data.  Cobber the old data if the Lterm has been set,
	# otherwise allocate space in the global data area.

	nelem = ndim * ndim
	if (MI_LTM(mw) == NULL)
	    MI_LTM(mw) = mw_allocd (mw, nelem)
	call achtrd (ltm, D(mw,MI_LTM(mw)), nelem)

	if (MI_LTV(mw) == NULL)
	    MI_LTV(mw) = mw_allocd (mw, ndim)
	call achtrd (ltv, D(mw,MI_LTV(mw)), ndim)
end
