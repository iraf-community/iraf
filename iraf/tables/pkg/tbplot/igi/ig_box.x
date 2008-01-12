include <gset.h>
include <mach.h>

include "igi.h"

#  1/27/93  Fixed INDEF tests.

procedure ig_box (igs)

pointer	igs		# igi parameters structure

int	xlabclk, ylabclk

int	get_int()

errchk	get_int

begin
	call lcmdcat (igs, YES)

	# Set the virtual page and viewport on the device
	call vpage (igs)

	iferr (xlabclk = get_int(igs))
	    return

	if (IS_INDEFI (xlabclk))
	    ylabclk = INDEFI
	else
	    iferr (ylabclk = get_int(igs))
		return

	ifnoerr (call ii_box (igs, xlabclk, ylabclk))
	    call cmdcat (igs, YES)

	call gflush (GIO_GP(igs))
end


procedure ii_box (igs, labelx, labely)

pointer	igs		# Parameters structure pointer
int	labelx, labely	# Draw labels parallel to axis?

pointer	igps
real	angle
real	a1, a2
real	wl, wr, wb, wt
real	vl, vr, vb, vt
real	size
int	label
int	clock
real	maxexp

begin
	call ggwind (GIO_GP(igs), wl, wr, wb, wt)
	call ggview (GIO_GP(igs), vl, vr, vb, vt)
	call gseti  (GIO_GP(igs), G_CLIP, NO)

	igps  = PLOT_PARMS(igs)

	# Save current angle
	angle = MG_ANGLE(igps)

	# X axes horizontal
	MG_ANGLE(igps)  = 0.0
	if (MG_MINORX(igps) < 0.0) {
	    # Log X axis

	    maxexp = real (MAX_EXPONENT)

	    if (wl > maxexp || wr > maxexp ||
		wl < -maxexp || wr < -maxexp) {
		call eprintf ("WCS limits out of range for log axis.\n")
		call eprintf ("Perhaps you need to specify them in the log.\n")
		call error (0, "")
	    }

	    a1 = 10.0 ** wl
	    a2 = 10.0 ** wr

	} else {
	    # Linear 
	    a1 = wl
	    a2 = wr
	}

	size = vr - vl

	# Bottom axis labeled
	if (IS_INDEFI (labelx))
	    label = 1
	else
	    label = labelx

	clock = 0
	call axis (igs, a1, a2, MG_MINORX(igps), MG_MAJORX(igps),
	    vl, vb, size, label, clock, MG_TICKFMT(igps))

	# Top axis not labeled
	label = 0
	clock = 1

	call axis (igs, a1, a2, MG_MINORX(igps), MG_MAJORX(igps),
	    vl, vt, size, label, clock, MG_TICKFMT(igps))

	# Save major tick spacing
	MG_GXSTEP(igps) = MG_GSTEP(igps)

	# Y axes vertical
	MG_ANGLE(igps) = 90.0

	if (MG_MINORY(igps) < 0.0) {
	    # Log Y axis
	    a1 = 10.0 ** wb
	    a2 = 10.0 ** wt
	} else {
	    a1 = wb
	    a2 = wt
	}
	size = vt - vb

	# Left axis labeled
	if (IS_INDEFI (labely))
	    label = 2
	else
	    label = labely
	clock = 1

	call axis (igs, a1, a2, MG_MINORY(igps), MG_MAJORY(igps),
	    vl, vb, size, label, clock, MG_TICKFMT(igps))

	# Right axis not labeled
	label = 0
	clock = 0

	call axis (igs, a1, a2, MG_MINORY(igps), MG_MAJORY(igps),
	    vr, vb, size, label, clock, MG_TICKFMT(igps))

	# Save major tick spacing
	MG_GYSTEP(igps) = MG_GSTEP(igps)

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	MG_ANGLE(igps) = angle 
end
