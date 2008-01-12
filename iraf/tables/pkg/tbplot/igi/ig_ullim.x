include <gset.h>
include "igi.h"

#  IG_ULLIM -- Draw upper or lower limit markers at the coordinates in
#  the input column(s).  The direction of the marker is specified by 
#  values in the limits column.

#  8/20/91 Removed ^Ls. ZGL
#  11/17/93  Inserted brackets in loop in igvlim() to fix bug causing
#            limits not to be drawn if X data are used.

procedure ig_ullim (igs)

pointer	igs		# Parameters structure

pointer	igps
int	axis

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	if (MG_LDATAP(igps) == NULL) {
	    call eprintf ("No Limits data ")
	    return
	}

	axis = 2

	call ii_ullim (igs, axis)
end


procedure ii_ullim (igs, axis)

pointer	igs		# Parameters structure
int	axis

pointer	igps
real	size
int	npts

begin
	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL || MG_LDATAP(igps) == NULL)
	    return

	call setltype (igs, MG_LTYPEN(igps))
	call gseti (GIO_GP(igs), G_CLIP, YES)

	size = MG_CHARSIZE(igps) * MG_EXPAND(igps)

	if (axis == 2) {
	    if (MG_XDATAP(igps) == NULL) {
		# No X data;  use pixel numbers
		npts = min (MG_LNPTS(igps), MG_YNPTS(igps))
		call igplim (GIO_GP(igs), Memr[MG_YDATAP(igps)], 
		    Memr[MG_LDATAP(igps)], npts, size)

	    } else {
		# Both X and Y data
		npts = min (MG_LNPTS(igps), MG_YNPTS(igps))
		npts = min (npts, MG_XNPTS(igps))
		call igvlim (GIO_GP(igs), 
		    Memr[MG_XDATAP(igps)], Memr[MG_YDATAP(igps)], 
		    Memr[MG_LDATAP(igps)], npts, size)
	    }

	    MG_NPTS(igps) = npts

	    call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	    call gflush (GIO_GP(igs))
	}
end


procedure igplim (gp, ydata, ldata, npts, size)

pointer	gp
real	ydata[ARB], ldata[ARB]
int	npts
real	size

real	x
int	i

begin
	do i = 1, npts {
	    x = real (i)
	    if (IS_INDEF(ldata[i]))
		next
	    if (ldata[i] < 0.0)
		# Lower limit
		call arrow (gp, x, ydata[i], size, UP)
	    else if (ldata[i] > 0.0)
		call arrow (gp, x, ydata[i], size, DOWN)
	}
end


procedure igvlim (gp, xdata, ydata, ldata, npts, size)

pointer	gp
real	xdata[ARB], ydata[ARB]
real	ldata[ARB]
int	npts
real	size

int	i

begin
	do i = 1, npts {
	    if (IS_INDEF(ldata[i]))
		next
	    if (ldata[i] < 0.0)
		# Lower limit
		call arrow (gp, xdata[i], ydata[i], size, UP)
	    else if (ldata[i] > 0.0)
		call arrow (gp, xdata[i], ydata[i], size, DOWN)
	}
end
