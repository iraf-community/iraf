# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_SWSAMPR -- Set the sampled WCS curve for an axis.

procedure mw_swsampr (mw, axis, pv, wv, npts)

pointer	mw			#I pointer to MWCS descriptor
int	axis			#I axis which gets the wsamp vector
real	pv[ARB]			#I physical coordinates of points
real	wv[ARB]			#I world coordinates of points
int	npts			#I number of data point in curve

pointer	wp
int	mw_allocd()
errchk	syserrs, mw_allocd

begin
	# Get the current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, "mw_swsampr")

	# Overwrite the current curve, if any, else allocate new storage.
	if (WCS_PV(wp,axis) == NULL || WCS_NPTS(wp,axis) < npts)
	    WCS_PV(wp,axis) = mw_allocd (mw, npts)
	call achtrd (pv, D(mw,WCS_PV(wp,axis)), npts)

	if (WCS_WV(wp,axis) == NULL || WCS_NPTS(wp,axis) < npts)
	    WCS_WV(wp,axis) = mw_allocd (mw, npts)
	call achtrd (wv, D(mw,WCS_WV(wp,axis)), npts)

	WCS_NPTS(wp,axis) = npts
end
