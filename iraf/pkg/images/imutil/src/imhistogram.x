# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<gset.h>

define	SZ_CHOICE	18

define	HIST_TYPES	"|normal|cumulative|difference|second_difference|"
define	NORMAL		1
define	CUMULATIVE	2
define	DIFFERENCE	3
define	SECOND_DIFF	4

define	PLOT_TYPES	"|line|box|"
define	LINE		1
define	BOX		2

define	SZ_TITLE	512		# plot title buffer

# IMHISTOGRAM -- Compute and plot the histogram of an image.

procedure t_imhistogram()

long	v[IM_MAXDIM]
real	z1, z2, dz, z1temp, z2temp, zstart
int	npix, nbins, nbins1, nlevels, nwide, z1i, z2i, i, maxch, histtype
pointer gp, im, sp, hgm, hgmr, buf, image, device, str, title, op

real	clgetr()
pointer	immap(), gopen()
int	clgeti(), clgwrd()
int	imgnlr(), imgnli()
bool	clgetb(), fp_equalr()

begin
	call smark (sp)
	call salloc (image, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_CHOICE, TY_CHAR)

	# Get the image name.
	call clgstr ("image", Memc[image], SZ_LINE)
	im = immap (Memc[image], READ_ONLY, 0)
	npix = IM_LEN(im,1)

	# Get histogram range.
	z1 = clgetr ("z1")
	z2 = clgetr ("z2")

	if (IS_INDEFR(z1) || IS_INDEFR(z2)) {

	    if (IM_LIMTIME(im) >= IM_MTIME(im)) {
		z1temp = IM_MIN(im)
		z2temp = IM_MAX(im)
	    } else
		call im_minmax (im, z1temp, z2temp)

	    if (IS_INDEFR(z1))
		z1 = z1temp

	    if (IS_INDEFR(z2))
		z2 = z2temp
	}

	if (z1 > z2) {
	    dz = z1;  z1 = z2;  z2 = dz
	}

	# Get default histogram resolution.
	dz = clgetr ("binwidth")
	if (IS_INDEFR(dz))
	    nbins = clgeti ("nbins")
	else {
	    nbins = nint ((z2 - z1) / dz)
	    z2 = z1 + nbins * dz
	}

	# Set the limits for integer images.
	switch (IM_PIXTYPE(im)) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    z1i = nint (z1)
	    z2i = nint (z2)
	    z1 = real (z1i)
	    z2 = real (z2i)
	}

	# Adjust the resolution of the histogram and/or the data range
	# so that an integral number of data values map into each
	# histogram bin (to avoid aliasing effects).

	if (clgetb ("autoscale"))
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
		nlevels = z2i - z1i
		nwide = max (1, nint (real (nlevels) / real (nbins)))
		nbins = max (1, nint (real (nlevels) / real (nwide)))
		z2i = z1i + nbins * nwide
		z2 = real (z2i)
	    }

	# The extra bin counts the pixels that equal z2 and shifts the
	# remaining bins to evenly cover the interval [z1,z2].
	# Real numbers could be handled better - perhaps adjust z2
	# upward by ~ EPSILONR (in ahgm itself).

	nbins1 = nbins + 1

	# Initialize the histogram buffer and image line vector.
	call salloc (hgm,  nbins1, TY_INT)
	call aclri  (Memi[hgm], nbins1)
	call amovkl (long(1), v, IM_MAXDIM)

	# Read successive lines of the image and accumulate the histogram.

	switch (IM_PIXTYPE(im)) {
	case TY_SHORT, TY_USHORT, TY_INT, TY_LONG:
	    # Test for constant valued image, which causes zero divide in ahgm.
	    if (z1i == z2i) {
	        call eprintf ("Warning: Image `%s' has no data range.\n")
		    call pargstr (Memc[image])
	        call imunmap (im)
	        call sfree (sp)
	        return
	    }

	    while (imgnli (im, buf, v) != EOF) 
		call ahgmi (Memi[buf], npix, Memi[hgm], nbins1, z1i, z2i)

	default:
	    # Test for constant valued image, which causes zero divide in ahgm.
	    if (fp_equalr (z1, z2)) {
	        call eprintf ("Warning: Image `%s' has no data range.\n")
		    call pargstr (Memc[image])
	        call imunmap (im)
	        call sfree (sp)
	        return
	    }

	    while (imgnlr (im, buf, v) != EOF)
		call ahgmr (Memr[buf], npix, Memi[hgm], nbins1, z1, z2)
	}

	# "Correct" the topmost bin for pixels that equal z2.  Each
	# histogram bin really wants to be half open.

	if (clgetb ("top_closed"))
	    Memi[hgm+nbins-1] = Memi[hgm+nbins-1] + Memi[hgm+nbins1-1]

	dz = (z2 - z1) / real (nbins)

	histtype = clgwrd ("hist_type", Memc[str], SZ_CHOICE, HIST_TYPES)

	switch (histtype) {
	case NORMAL:
	    # do nothing
	case CUMULATIVE:
	    call ih_acumi (Memi[hgm], Memi[hgm], nbins)
	case DIFFERENCE:
	    call ih_amrgi (Memi[hgm], Memi[hgm], nbins)
	    z1 = z1 + dz / 2.
	    z2 = z2 - dz / 2.
	    nbins = nbins - 1
	case SECOND_DIFF:
	    call ih_amrgi (Memi[hgm], Memi[hgm], nbins)
	    call ih_amrgi (Memi[hgm], Memi[hgm], nbins-1)
	    z1 = z1 + dz
	    z2 = z2 - dz
	    nbins = nbins - 2
	default:
	    call error (1, "bad switch 1")
	}

	# List or plot the histogram.  In list format, the bin value is the
	# z value of the left side (start) of the bin.

	if (clgetb ("listout")) {
	    zstart = z1 + dz / 2.0
	    do i = 1, nbins {
		call printf ("%g %d\n")
		    call pargr (zstart)
		    call pargi (Memi[hgm+i-1])
		zstart = zstart + dz
	    }
	} else {
	    call salloc (device, SZ_FNAME, TY_CHAR)
	    call salloc (title, SZ_TITLE, TY_CHAR)
	    call salloc (hgmr, nbins, TY_REAL)
	    call achtir (Memi[hgm], Memr[hgmr], nbins)

	    call clgstr ("device", Memc[device], SZ_FNAME)
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	    if (clgetb ("logy"))
		call gseti (gp, G_YTRAN, GW_LOG)
	    call gswind (gp, z1, z2, INDEF, INDEF)
	    call gascale (gp, Memr[hgmr], nbins, 2)

	    # Format the plot title, starting with the system banner.
	    call sysid (Memc[title], SZ_TITLE)
	    for (op=title;  Memc[op] != '\n' && Memc[op] != EOS;  op=op+1)
		;
	    Memc[op] = '\n';  op = op + 1
	    maxch = SZ_TITLE - (op - title)

	    # Format the remainder of the plot title.
	    call sprintf (Memc[op], maxch,
		"%s of %s = %s\nFrom z1=%g to z2=%g, nbins=%d, width=%g")
		switch (histtype) {
		case NORMAL:
		    call pargstr ("Histogram")
		case CUMULATIVE:
		    call pargstr ("Cumulative histogram")
		case DIFFERENCE:
		    call pargstr ("Difference histogram")
		case SECOND_DIFF:
		    call pargstr ("Second difference histogram")
		default:
		    call error (1, "bad switch 3")
		}

		call pargstr (Memc[image])
		call pargstr (IM_TITLE(im))
		call pargr (z1)
		call pargr (z2)
		call pargi (nbins)
		call pargr (dz)

	    # Draw the plot.  Center the bins for plot_type=line.
	    call glabax (gp, Memc[title], "", "")

	    switch (clgwrd ("plot_type", Memc[str], SZ_LINE, PLOT_TYPES)) {
	    case LINE:
		call gvline (gp, Memr[hgmr], nbins, z1 + dz/2., z2 - dz/2.)
	    case BOX:
		call hgline (gp, Memr[hgmr], nbins, z1, z2)
	    default:
		call error (1, "bad switch 2")
	    }

	    call gclose (gp)
	}

	call imunmap (im)
	call sfree (sp)
end


# HGLINE -- Draw a stepped curve of the histogram data.

procedure hgline (gp, ydata, npts, x1, x2)

pointer	gp		# Graphics descriptor
real	ydata[ARB]	# Y coordinates of the line endpoints
int	npts		# Number of line endpoints
real	x1, x2

int	pixel
real	x, y, dx

begin
	dx = (x2 - x1) / npts

	# Do the first horizontal line
	x = x1
	y = ydata[1]
	call gamove (gp, x, y)
	x = x + dx
	call gadraw (gp, x, y)

	do pixel = 2, npts {
	    x = x1 + dx * (pixel - 1)
	    y = ydata[pixel]
	    # vertical connection
	    call gadraw (gp, x, y)
	    # horizontal line
	    call gadraw (gp, x + dx, y)
	}
end


# These two routines are intended to be generic vops routines.  Only
# the integer versions are included since that's all that's used here.

# <NOT IMPLEMENTED!>  The operation is carried out in such a way that
# the result is the same whether or not the output vector overlaps
# (partially) the input vector.  The routines WILL work in place!

# ACUM -- Compute a cumulative vector (generic).  Should b[1] be zero?

procedure ih_acumi (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i

# int	npix, i, a_first, b_first

begin
#	call zlocva (a, a_first)
#	call zlocva (b, b_first)
#
#	if (b_first <= a_first) {
	    # Shouldn't use output arguments internally,
	    # but no reason to use this routine unsafely.
	    b[1] = a[1]
	    do i = 2, npix
		b[i] = b[i-1] + a[i]
#	} else {
	    # overlapping solution not implemented yet!
#	}
end


# AMRG -- Compute a marginal (forward difference) vector (generic).

procedure ih_amrgi (a, b, npix)

int	a[ARB], b[ARB]
int	npix, i

# int	npix, i, a_first, b_first

begin
#	call zlocva (a, a_first)
#	call zlocva (b, b_first)
#
#	if (b_first <= a_first) {
	    do i = 1, npix-1
		b[i] = a[i+1] - a[i]
	    b[npix] = 0
#	} else {
	    # overlapping solution not implemented yet!
#	}
end
