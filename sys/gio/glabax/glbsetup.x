# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>

# GLB_SETUP -- Set up the axis drawing and labelling parameters.  These are
# the coordinate transformations, i.e., log scaling, window and viewport
# coordinates, plus the parameters which pertain only to axis drawing and
# labelling.  The order in which the subprocedures are called is significant.

procedure glb_setup (gp, axes, ntitlelines, xlabel, ylabel)

pointer	gp			# graphics descriptor
pointer	axes[4]			# array of pointers to axis descriptors
int	ntitlelines		# number of lines in title block
char	xlabel[ARB]		# x axis label
char	ylabel[ARB]		# y axis label

pointer	w
bool	fp_nondegenr()

begin
	w = GP_WCSPTR (gp, GP_WCS(gp))

	# Verify that there is sufficient range in the wcs X and Y.
	if (fp_nondegenr (WCS_WX1(w), WCS_WX2(w)))
	    GP_WCSSTATE(gp) = MODIFIED
	if (fp_nondegenr (WCS_WY1(w), WCS_WY2(w)))
	    GP_WCSSTATE(gp) = MODIFIED

	# If log scaling is in effect on either axis, verify that log scaling
	# is sensible and if so select either LOG or ELOG scaling.

	call glb_verify_log_scaling (gp)

	# Set the viewport if not already set.
	call glb_set_viewport (gp, ntitlelines, xlabel, ylabel)

	# Find the best positions for the tick marks, and if rounding is
	# enabled, extend the WCS outward to the next tick mark on either
	# end.

	call glb_find_ticks (gp, GP_XAP(gp), axes[1], axes[4],  0)
	call glb_find_ticks (gp, GP_YAP(gp), axes[3], axes[2], 90)

	# Set the remaining parameters in the axis drawing descriptors.
	# Must not be called until the window and viewport coordinates are
	# fixed.

	call glb_set_axes (gp, GP_XAP(gp), axes[1], axes[4],  0)
	call glb_set_axes (gp, GP_YAP(gp), axes[3], axes[2], 90)
end
