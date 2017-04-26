# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"imexam.h"

define	HGM_TYPES	"|line|box|"
define	HGM_LINE	1		# line vectors for histogram plot
define	HGM_BOX		2		# box vectors for histogram plot

# IE_HIMEXAM -- Compute and plot or list a histogram.
# If the GIO pointer is NULL list the histogram otherwise make a graph.

procedure ie_himexam (gp, mode, ie, x, y)

pointer	gp		# GIO pointer (NULL for histogram listing)
int	mode		# Mode
pointer	ie		# Structure pointer
real	x, y		# Center coordinate

real	z1, z2, dz, zmin, zmax
int	i, j, x1, x2, y1, y2, nx, ny, npts, nbins, nbins1, nlevels, nwide
pointer	pp, sp, hgm, title, im, data, xp, yp

int	clgpseti()
real	clgpsetr()
bool	clgpsetb(), fp_equalr()
pointer	clopset(), ie_gimage(), ie_gdata()

begin
	# Get the image and return on error.
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	# Use last graph coordinate if redrawing.  Close last graph pset
	# pointer if making new graph.

	if (gp != NULL) {
	    if (!IS_INDEF(x))
	        IE_X1(ie) = x
	    if (!IS_INDEF(y))
	        IE_Y1(ie) = y

	    z1 = IE_X1(ie)
	    z2 = IE_Y1(ie)

	    if (IE_PP(ie) != NULL)
		call clcpset (IE_PP(ie))
	} else {
	    z1 = x
	    z2 = y
	}

	# Get the data.
	pp = clopset ("himexam")
	nx = clgpseti (pp, "ncolumns")
	ny = clgpseti (pp, "nlines")
	x1 = z1 - (nx - 1) / 2 + 0.5
	x2 = z1 + nx / 2 + 0.5
	y1 = z2 - (ny - 1) / 2 + 0.5
	y2 = z2 + ny / 2 + 0.5
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	# Get default histogram resolution.
	nbins = clgpseti (pp, "nbins")

	# Get histogram range.
	z1 = clgpsetr (pp, "z1")
	z2 = clgpsetr (pp, "z2")

	# Use data limits for INDEF limits.
	if (IS_INDEFR(z1) || IS_INDEFR(z2)) {
	    call alimr (Memr[data], npts, zmin, zmax)
	    if (IS_INDEFR(z1))
		z1 = zmin
	    if (IS_INDEFR(z2))
		z2 = zmax
	}

	if (z1 > z2) {
	    dz = z1;  z1 = z2;  z2 = dz
	}

	# Adjust the resolution of the histogram and/or the data range
	# so that an integral number of data values map into each
	# histogram bin (to avoid aliasing effects).

	if (clgpsetb (pp, "autoscale")) {
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
		nlevels = nint (z2) - nint (z1)
		nwide = max (1, nint (real (nlevels) / real (nbins)))
		nbins = max (1, nint (real (nlevels) / real (nwide)))
		z2 = nint (z1) + nbins * nwide
	    }
	}

	# Test for constant valued image, which causes zero divide in ahgm.
	if (fp_equalr (z1, z2)) {
	    call eprintf ("Warning: Image `%s' has no data range.\n")
		call pargstr (IE_IMAGE(ie))
	    return
	}

	# The extra bin counts the pixels that equal z2 and shifts the
	# remaining bins to evenly cover the interval [z1,z2].
	# Note that real numbers could be handled better - perhaps
	# adjust z2 upward by ~ EPSILONR (in ahgm itself).

	nbins1 = nbins + 1

	# Initialize the histogram buffer and image line vector.
	call smark (sp)
	call salloc (hgm,  nbins1, TY_INT)
	call aclri  (Memi[hgm], nbins1)

	call ahgmr (Memr[data], npts, Memi[hgm], nbins1, z1, z2)

	# "Correct" the topmost bin for pixels that equal z2.  Each
	# histogram bin really wants to be half open.

	if (clgpsetb (pp, "top_closed"))
	    Memi[hgm+nbins-1] = Memi[hgm+nbins-1] + Memi[hgm+nbins1-1]

	# List or plot the histogram.  In list format, the bin value is the
	# z value of the left side (start) of the bin.

	dz = (z2 - z1) / real (nbins)

	if (gp != NULL) {
	    # Draw the plot.
	    if (clgpsetb (pp, "pointmode")) {
		nbins1 = nbins
		call salloc (xp, nbins1, TY_REAL)
	        call salloc (yp, nbins1, TY_REAL)
	        call achtir (Memi[hgm], Memr[yp], nbins1)
		Memr[xp] = z1 + dz / 2.
		do i = 1, nbins1 - 1
		    Memr[xp+i] = Memr[xp+i-1] + dz
	    } else {
		nbins1 = 2 * nbins
		call salloc (xp, nbins1, TY_REAL)
	        call salloc (yp, nbins1, TY_REAL)
		Memr[xp] = z1
		Memr[yp] = Memi[hgm]
		j = 0
		do i = 1, nbins - 1 {
		    Memr[xp+j+1] = Memr[xp+j] + dz
		    Memr[yp+j+1] = Memr[yp+j]
		    j = j + 1
		    Memr[xp+j+1] = Memr[xp+j]
		    Memr[yp+j+1] = Memi[hgm+i]
		    j = j + 1
		}
		Memr[xp+j+1] = Memr[xp+j] + dz
		Memr[yp+j+1] = Memr[yp+j]
	    }

	    call salloc (title, IE_SZTITLE, TY_CHAR)
	    call sprintf (Memc[title], IE_SZTITLE,
		"%s[%d:%d,%d:%d]: Histogram from z1=%g to z2=%g, nbins=%d\n%s")
	        call pargstr (IE_IMNAME(ie))
		call pargi (x1)
		call pargi (x2)
	        call pargi (y1)
	        call pargi (y2)
		call pargr (z1)
		call pargr (z2)
		call pargi (nbins)
	        call pargstr (IM_TITLE(im))
	    call ie_graph (gp, mode, pp, Memc[title], Memr[xp],
		Memr[yp], nbins1, "", "")

	    IE_PP(ie) = pp
	} else {
	    do i = 1, nbins {
		call printf ("%g %d\n")
		    call pargr (z1 + (i-1) * dz)
		    call pargi (Memi[hgm+i-1])
	    }
	   call clcpset (pp)
	}

	call sfree (sp)
end
