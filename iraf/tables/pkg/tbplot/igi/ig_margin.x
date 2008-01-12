include "igi.h"

define	DEF_MARGIN	0.025	# Default margin (2.5%)

procedure ig_margin (igs)

#  IG_MARGIN -- Reset the plot scale to be slightly larger in WCS such
#  that there is an uniform margin between the plotted points and the
#  vieport (axes).  There is a single optional parameter that specifies
#  the fraction of the viewport in the margin.

#  First code 29 November 1990, Z.G. Levay, STScI.
## 6/26/92  Add cmdcat() to add to command buffer.  ZGL

pointer igs                     # igi parameters structure

pointer igps                    # Plot parameters structure
real	margin			# Fraction of VPage in margin

real    get_real()

errchk  get_real

begin
        call lcmdcat (igs, YES)

        iferr (margin = get_real (igs))
            # Invalid parameter
            return

        if (IS_INDEF (margin))
            # No arguments;  use default
	    margin = DEF_MARGIN

        if (DEBUG_OUTPUT(igs) == YES) {
	    call eprintf ("Margin:  %.4f ")
		call pargr (margin)
	}

	if (margin <= 0.0 || margin >= 1.0) {
	    call eprintf ("Margin must be between 0 and 1 ")
	    return
	}

	call ii_margin (igs, margin)
	
        if (DEBUG_OUTPUT(igs) == YES) {
            igps = PLOT_PARMS(igs)
            call eprintf ("Data window:  %.3f %.3f %.3f %.3f (WC) ")
                call pargr (MG_WINDLEFT(igps))
                call pargr (MG_WINDRIGHT(igps))
                call pargr (MG_WINDBOTTOM(igps))
                call pargr (MG_WINDTOP(igps))
        }

        call cmdcat (igs, NO)
end


procedure ii_margin (igs, margin)

pointer igs                     # igi parameters structure
real	margin			# Fraction of VPage in margin

pointer igps                    # Plot parameters structure
real    wl, wr, wb, wt          # Window
real	dd

begin
        igps = PLOT_PARMS(igs)

	# Get the Window
	call ggwind (GIO_GP(igs), wl, wr, wb, wt)

	dd = margin * (wr - wl)
 	wl = wl - dd
	wr = wr + dd

	dd = margin * (wt - wb)
	wb = wb - dd
	wt = wt + dd

	# Reset the window
	call gswind (GIO_GP(igs), wl, wr, wb, wt)

	# Reset the igi window parameters
	MG_WINDLEFT(igps)   = wl;  MG_WINDRIGHT(igps) = wr
	MG_WINDBOTTOM(igps) = wb;  MG_WINDTOP(igps)   = wt
end
