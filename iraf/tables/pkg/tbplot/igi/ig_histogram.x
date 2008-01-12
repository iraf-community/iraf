#  IG_HISTOGRAM -- Draw a histogram curve (bar graph)

include <gset.h>
include <mach.h>
include "igi.h"

#  8/20/91 Removed ^Ls. ZGL
## 7/16/92  Replaced gfill() with gpline() (tmeporarily) since it does
##          not recognize line style.  ZGL
## 7/17/92  Added fillpat argument and restored gfill, this time, with
##          the fill pattern used.  ZGL
## 7/20/92  Make sure max data value is at or below top edge to avoid
##          clipping one or more edges of bin.  ZGL
##          Add argument to set relative bar width.
## 7/21/92  Offset top and bottom of bars to avoid clipping (or turn
##          off clipping?).  ZGL
## 1/29/93  Fixed bug in drawing bar graph from X and Y data.


procedure ig_histogram (igs)

pointer	igs

real	frac			# Relative width of bars

real	get_real()

begin
	call lcmdcat (igs, YES)
	call cmdcat  (igs, YES)

	if (MG_YDATAP(PLOT_PARMS(igs)) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	iferr (frac = get_real (igs))
	    return

	call cmdcat (igs, NO)

	if (IS_INDEF(frac))
	    frac = 1.0

	call ii_histogram (igs, frac)
end


procedure ii_histogram (igs, frac)

pointer	igs
real	frac			# Relative width of bars

pointer	igps
int	npts

begin
	igps = PLOT_PARMS(igs)

	if (MG_YDATAP(igps) == NULL)
	    return

	call setltype (igs, MG_LTYPEN(igps))
	call gseti (GIO_GP(igs), G_CLIP, YES)

	if (MG_XDATAP(igps) == NULL)
	    # No X data;  use pixel numbers
	    call hist_vcrv (GIO_GP(igs), 
		Memr[MG_YDATAP(igps)], MG_YNPTS(igps),
		MG_FILLPAT(igps), frac)

	else {
	    # X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps)) 
	    call hist_crv (GIO_GP(igs), Memr[MG_XDATAP(igps)], 
		Memr[MG_YDATAP(igps)], npts, MG_FILLPAT(igps), frac)
	}

	MG_NPTS(igps) = npts

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end


#  HIST_CRV -- Draw a histogram style curve (bar graph) through the
#  points. 

procedure hist_crv (gp, xdata, ydata, npts, fillpat, frac)

pointer	gp			# Graphics descriptor
real	xdata[ARB]		# X coordinates of the line endpoints
real	ydata[ARB]		# Y coordinates of the line endpoints
int	npts			# Number of data values
int	fillpat			# Fill pattern index
real	frac			# Relative width of bar

int	bin, bin1, bin2
real	xbin[5], ybin[5]	# Fill polyline
real	x, x1, x2
real	dx
real	left, right, bottom, top	# Data window
int	wcs
real	dbx

int	ignbin(), gstati()

begin
	# Find the edges of the data window
	call ggwind (gp, left, right, bottom, top)
	wcs = gstati (gp, G_WCS)

	#call ig_gctran (gp, 0.0, 0.0, left, bottom, 0, wcs)

	bin1 = ignbin (1, xdata, ydata, npts)
	if (bin1 >= npts)
	    call eprintf ("No valid data\n")

	bin2 = ignbin (bin1+1, xdata, ydata, npts)
	if (bin2 >= npts)
	    call eprintf ("No valid data\n")

	x1 = xdata[bin1]
	x2 = xdata[bin2]

	dbx = frac / 2.0
	dx  = (x2 - x1) * dbx

	top = top - EPSILONR
	bottom = bottom + EPSILONR

	xbin[1] = max (x1 - dx, left)
	ybin[1] = bottom
	xbin[2] = xbin[1]
	ybin[2] = min (ydata[bin1], top)
	xbin[3] = x1 + dx
	ybin[3] = ybin[2]
	xbin[4] = xbin[3]
	ybin[4] = ybin[1]
	xbin[5] = xbin[1]
	ybin[5] = ybin[1]

	if (fillpat > GF_HOLLOW)
	    call gfill (gp, xbin, ybin, 5, fillpat)

	call gpline (gp, xbin, ybin, 5)

	bin = bin2

	while (bin < npts) {

	    bin2 = ignbin (bin+1, xdata, ydata, npts)

	    x  = xdata[bin]
	    x1 = xdata[bin1]
	    x2 = xdata[bin2]

	    xbin[1] = x - (x - x1) * dbx
	    ybin[1] = bottom
	    xbin[2] = xbin[1]
	    ybin[2] = min (ydata[bin], top)
	    xbin[3] = x + (x2 - x) * dbx
	    ybin[3] = ybin[2]
	    xbin[4] = xbin[3]
	    ybin[4] = ybin[1]
	    xbin[5] = xbin[1]
	    ybin[5] = ybin[1]

	    if (fillpat > GF_HOLLOW)
		call gfill (gp, xbin, ybin, 5, fillpat)

	    call gpline (gp, xbin, ybin, 5)

	    bin1 = bin
	    bin  = bin2
	}

	x1 = xdata[bin1]
	x  = xdata[npts]

	dx = (x - x1) * dbx

	xbin[1] = x - dx
	ybin[1] = bottom
	xbin[2] = xbin[1]
	ybin[2] = min (ydata[bin], top)
	xbin[3] = min (x + dx, right)
	ybin[3] = ybin[2]
	xbin[4] = xbin[3]
	ybin[4] = ybin[1]
	xbin[5] = xbin[1]
	ybin[5] = ybin[1]

	if (fillpat > GF_HOLLOW)
	    call gfill (gp, xbin, ybin, 5, fillpat)

	call gpline (gp, xbin, ybin, 5)
end


int procedure ignbin (start, x, y, npts)

int	start
real	x[ARB], y[ARB]
int	npts

int	bin

begin
	for (bin = start;  
	    bin <= npts && (IS_INDEF(x[bin]) || IS_INDEF(y[bin]));  
	    bin = bin + 1)
	    ;
	return (bin)
end


#  HIST_VCRV -- Draw a histogram style curve (bar graph) through the
#  points. 

procedure hist_vcrv (gp, ydata, npts, fillpat, frac)

pointer	gp			# Graphics descriptor
real	ydata[ARB]		# Y coordinates of the line endpoints
int	npts			# Number of data values
int	fillpat			# Fill pattern index
real	frac			# Relative width of bar

int	bin
real	xbin[5], ybin[5]	# Fill polyline
real	dx
real	left, right, bottom, top	# Data window
int	wcs

int	nxtvbin(), gstati()

begin
	# Find the edges of the data window
	call ggwind (gp, left, right, bottom, top)
	wcs = gstati (gp, G_WCS)
	call ig_gctran (gp, 0.0, 0.0, left, bottom, 0, wcs)

	bin = nxtvbin (1, ydata, npts)

	if (bin >= npts)
	    call eprintf ("No valid data\n")

	dx = frac / 2.0

	while (bin <= npts) {
	    xbin[1] = real (bin) - dx
	    ybin[1] = bottom + EPSILONR
	    xbin[2] = xbin[1]
	    ybin[2] = min (ydata[bin], top - EPSILONR)
	    xbin[3] = real (bin) + dx
	    ybin[3] = ybin[2]
	    xbin[4] = xbin[3]
	    ybin[4] = ybin[1]
	    xbin[5] = xbin[1]
	    ybin[5] = ybin[1]

	    if (fillpat > GF_HOLLOW)
		call gfill (gp, xbin, ybin, 5, fillpat)

	    call gpline (gp, xbin, ybin, 5)

	    bin = nxtvbin (bin+1, ydata, npts)
	}
end


int procedure nxtvbin (start, y, npts)

int	start
real	y[ARB]
int	npts

int	bin

begin
	for (bin = start;
	    bin <= npts && IS_INDEF(y[bin]);
	    bin = bin + 1)
	    ;

	return (bin)
end
