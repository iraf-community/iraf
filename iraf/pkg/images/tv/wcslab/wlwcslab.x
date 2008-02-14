include <gio.h>
include <gset.h>
include "wcslab.h"
include "wcs_desc.h"

# Define the memory structure for saving the graphics wcs.
define SAVE_BLOCK_SIZE  16
define OLD_NDC_VIEW     Memr[P2R(wcs_save_block-1+$1)]
define OLD_NDC_WIND     Memr[P2R(wcs_save_block+3+$1)]
define OLD_PLT_VIEW     Memr[P2R(wcs_save_block+7+$1)]
define OLD_PLT_WIND     Memr[P2R(wcs_save_block+11+$1)]

# WL_WCSLAB -- Label using a defined wcs.
#
# Description
#   This routine uses the information in the WCSLAB descriptor to perform
#   labelling.
#
#   Before this routine can be called, several things must have already
#   occured.  They are as follows:
#      1 A call to wl_create must be made to create the WCSLAB descriptor.
#      2 The WCS_MW component must be set to the MWCS object of the
#        desired transformations.
#      3 A call to wl_get_system_type must be made.
#      4 The graphics device must have been opened and the window defined.
#        The WCS_GP component of the WCSLAB descriptor must be set to the
#        graphics window descriptor.
#
#   When done with this routine, the WL_GP and WL_MW components must be
#   deallocated seperately.  Then only wlab_destroy need be called to
#   remove the WCSLAB descriptor.
#
#---------------------------------------------------------------------------

procedure wl_wcslab (wd)

pointer wd 	 # I: the WCSLAB descriptor

int	old_clip, old_pltype, old_txquality, old_wcs
pointer	sp, wcs_save_block
real	old_plwidth, old_txsize, old_txup
int	gstati()
real	gstatr()

begin
	# Allocate working space.
	call smark(sp)
	call salloc(wcs_save_block, SAVE_BLOCK_SIZE, TY_STRUCT)

	# Store certain graphics parameters.
	old_plwidth = gstatr (WL_GP(wd), G_PLWIDTH)
	old_txsize = gstatr (WL_GP(wd), G_TXSIZE)
	old_txup = gstatr (WL_GP(wd), G_TXUP)
	old_clip = gstati (WL_GP(wd), G_CLIP)
	old_pltype = gstati (WL_GP(wd), G_PLTYPE)
	old_txquality= gstati (WL_GP(wd), G_TXQUALITY)
	old_wcs = gstati (WL_GP(wd), G_WCS)

	# Choose two other graphics wcs' for internal use.  Save the wcs for
	# later restoration.
	if( old_wcs < MAX_WCS - 2 ) {
	    WL_NDC_WCS(wd) = old_wcs + 1
	    WL_PLOT_WCS(wd) = WL_NDC_WCS(wd) + 1
	} else {
	    WL_NDC_WCS(wd) = old_wcs - 1
	    WL_PLOT_WCS(wd) = WL_NDC_WCS(wd) - 1
	}
	call gseti(WL_GP(wd), G_WCS, WL_NDC_WCS(wd))
	call ggview(WL_GP(wd), OLD_NDC_VIEW(LEFT), OLD_NDC_VIEW(RIGHT),
	    OLD_NDC_VIEW(BOTTOM), OLD_NDC_VIEW(TOP))
	call ggwind(WL_GP(wd), OLD_NDC_WIND(LEFT), OLD_NDC_WIND(RIGHT),
	    OLD_NDC_WIND(BOTTOM), OLD_NDC_WIND(TOP))
	call gseti(WL_GP(wd), G_WCS, WL_PLOT_WCS(wd))
	call ggview(WL_GP(wd), OLD_PLT_VIEW(LEFT), OLD_PLT_VIEW(RIGHT),
	    OLD_PLT_VIEW(BOTTOM), OLD_PLT_VIEW(TOP))
	call ggwind(WL_GP(wd), OLD_PLT_WIND(LEFT), OLD_PLT_WIND(RIGHT),
	    OLD_PLT_WIND(BOTTOM), OLD_PLT_WIND(TOP))

	# Set the graphics device the way wcslab requires it.
	call gseti (WL_GP(wd), G_WCS, old_wcs)
	call wl_graphics (wd)

	# Determine basic characteristics of the plot.
	call wl_setup (wd) 

	# Plot the grid lines.
	call wl_grid (wd)

	# Put the grid labels on the lines.
	if (WL_LABON(wd) == YES)
	    call wl_label (wd)

	# Restore the original graphics wcs.
        call gseti(WL_GP(wd), G_WCS, WL_NDC_WCS(wd))
        call gsview(WL_GP(wd), OLD_NDC_VIEW(LEFT), OLD_NDC_VIEW(RIGHT),
	    OLD_NDC_VIEW(BOTTOM), OLD_NDC_VIEW(TOP))
        call gswind(WL_GP(wd), OLD_NDC_WIND(LEFT), OLD_NDC_WIND(RIGHT),
            OLD_NDC_WIND(BOTTOM), OLD_NDC_WIND(TOP))
        call gseti(WL_GP(wd), G_WCS, WL_PLOT_WCS(wd))
        call gsview(WL_GP(wd), OLD_PLT_VIEW(LEFT), OLD_PLT_VIEW(RIGHT),
            OLD_PLT_VIEW(BOTTOM), OLD_PLT_VIEW(TOP))
        call gswind(WL_GP(wd), OLD_PLT_WIND(LEFT), OLD_PLT_WIND(RIGHT),
            OLD_PLT_WIND(BOTTOM), OLD_PLT_WIND(TOP))

	# Restore original graphics state.
	call gsetr (WL_GP(wd), G_PLWIDTH, old_plwidth)
	call gsetr (WL_GP(wd), G_TXSIZE, old_txsize)
	call gsetr (WL_GP(wd), G_TXUP, old_txup)
	call gseti (WL_GP(wd), G_CLIP, old_clip)
	call gseti (WL_GP(wd), G_PLTYPE, old_pltype)
	call gseti (WL_GP(wd), G_TXQUALITY, old_txquality)
	call gseti (WL_GP(wd), G_WCS, old_wcs)

	call sfree (sp)
end


# WL_GRAPHICS -- Setup the graphics device appropriate for the occasion.

procedure wl_graphics (wd)

pointer wd		  # I: the WCSLAB descriptor

real	relative_size, vl, vr, vb, vt
real	ggetr()

begin
	# Setup a graphics WCS that mimics the NDC coordinate WCS,
	# but with clipping.
	call ggview (WL_GP(wd), vl, vr, vb, vt)
	call gseti (WL_GP(wd), G_WCS, WL_NDC_WCS(wd))
	call gsview (WL_GP(wd), vl, vr, vb, vt)
	call gswind (WL_GP(wd), vl, vr, vb, vt)
	call gseti (WL_GP(wd), G_CLIP, YES)

	# Setup the initial viewport.
	WL_NEW_VIEW(wd,LEFT) = vl
	WL_NEW_VIEW(wd,RIGHT) = vr
	WL_NEW_VIEW(wd,BOTTOM) = vb
	WL_NEW_VIEW(wd,TOP) = vt

	# Setup some parameters.
	call gseti (WL_GP(wd), G_PLTYPE, GL_SOLID)
	call gsetr (WL_GP(wd), G_PLWIDTH, LINE_SIZE)

	# Draw the edges of the viewport.
	call gamove (WL_GP(wd), vl, vb)
	call gadraw (WL_GP(wd), vr, vb)
	call gadraw (WL_GP(wd), vr, vt)
	call gadraw (WL_GP(wd), vl, vt)
	call gadraw (WL_GP(wd), vl, vb)

	# Determine the tick mark size.
	relative_size = max (abs (vr - vl), abs (vt - vb ))
	WL_MAJ_TICK_SIZE(wd) = relative_size * WL_MAJ_TICK_SIZE(wd)
	WL_MIN_TICK_SIZE(wd) = relative_size * WL_MIN_TICK_SIZE(wd)

	# Determine various character sizes.
	WL_TITLE_SIZE(wd) = WL_TITLE_SIZE(wd) * relative_size
	WL_AXIS_TITLE_SIZE(wd) = WL_AXIS_TITLE_SIZE(wd) * relative_size
	WL_LABEL_SIZE(wd) = WL_LABEL_SIZE(wd) * relative_size

	# Now setup the general plotting WCS.
	call gseti (WL_GP(wd), G_WCS, WL_PLOT_WCS(WD))
	call gsview (WL_GP(wd), vl, vr, vb, vt)
	vl = real (WL_SCREEN_BOUNDARY(wd,LEFT))
	vr = real (WL_SCREEN_BOUNDARY(wd,RIGHT))
	vb = real (WL_SCREEN_BOUNDARY(wd,BOTTOM))
	vt = real (WL_SCREEN_BOUNDARY(wd,TOP))
	call gswind (WL_GP(wd), vl, vr, vb, vt)
	call gseti (WL_GP(wd), G_CLIP, YES)

	# Set some characteristics of the graphics device.
	call gseti (WL_GP(wd), G_TXQUALITY, GT_HIGH)
	call gseti (WL_GP(wd), G_CLIP, YES)
	call gsetr (WL_GP(wd), G_PLWIDTH, LINE_SIZE)

	# Determine the number of segments a "line" should consist of.
	WL_LINE_SEGMENTS(wd) = int (min (ggetr (WL_GP(wd), "xr"), 
	    ggetr (WL_GP(wd), "yr")) / 5)
end
