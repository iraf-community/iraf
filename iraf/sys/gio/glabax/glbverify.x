# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

# GLB_VERIFY_LOG_SCALING -- Verify that log scaling makes sense, i.e., that
# the range covered by an axis compared to its distance from the origin is
# large enough to permit log scaling.  If log scaling is reasonable check if
# the window goes negative, and switch to ELOG scaling if such is the case.

procedure glb_verify_log_scaling (gp)

pointer	gp			# graphics descriptor
pointer	w

begin
	w  = GP_WCSPTR (gp, GP_WCS(gp))

	# Force ELOG scaling if any data <= 0.

	if (WCS_XTRAN(w) != LINEAR)
	    if (WCS_WX1(w) <= 0 || WCS_WX2(w) <= 0)
		WCS_XTRAN(w) = ELOG

	if (WCS_YTRAN(w) != LINEAR)
	    if (WCS_WY1(w) <= 0 || WCS_WY2(w) <= 0)
		WCS_YTRAN(w) = ELOG

	# Set the WCS state to modified even if it wasn't.  This is safe
	# and in any case the WCS is changed in the main glabax routine
	# shortly after we are called.

	GP_WCSSTATE(gp) = MODIFIED
end
