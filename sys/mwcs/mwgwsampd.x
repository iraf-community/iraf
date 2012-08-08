# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"mwcs.h"

# MW_GWSAMPD -- Get the sampled WCS curve for an axis.

procedure mw_gwsampd (mw, axis, pv, wv, npts)

pointer	mw			#I pointer to MWCS descriptor
int	axis			#I axis which gets the wsamp vector
double	pv[ARB]			#O physical coordinates of points
double	wv[ARB]			#O world coordinates of points
int	npts			#I number of data point in curve

pointer	wp
errchk	syserrs
string	s_name "mw_gwsampd"

begin
	# Get the current WCS.
	wp = MI_WCS(mw)
	if (wp == NULL)
	    call syserrs (SYS_MWNOWCS, s_name)

	# Verify that there is a sampled curve for this WCS.
	if (WCS_NPTS(wp,axis) <= 0 || WCS_PV(wp,axis) == NULL
	    || WCS_WV(wp,axis) == NULL)
	    call syserrs (SYS_MWNOWSAMP, s_name)

	# Copy out the curves.
	call amovd (D(mw,WCS_PV(wp,axis)), pv, min(WCS_NPTS(wp,axis), npts))
	call amovd (D(mw,WCS_WV(wp,axis)), wv, min(WCS_NPTS(wp,axis), npts))
end
