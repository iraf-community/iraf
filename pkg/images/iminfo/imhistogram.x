# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<gset.h>

define	HGM_TYPES	"|line|box|"
define	HGM_LINE	1		# line vectors for histogram plot
define	HGM_BOX		2		# box vectors for histogram plot
define	SZ_TITLE	512		# plot title buffer

# IMHISTOGRAM -- Compute and plot the histogram of an image.

procedure t_imhistogram()

long	v[IM_MAXDIM]
real	z1, z2, dz, z1temp, z2temp
int	npix, nbins, nbins1, nlevels, nwide, z1i, z2i, i, maxch
pointer gp, im, sp, hgm, hgmr, buf, image, device, str, title, op

real	clgetr()
pointer	immap(), gopen()
int	clgeti(), clgwrd()
int	imgnlr(), imgnli()
bool	clgetb(), fp_equalr()

begin
	call smark (sp)
	call salloc (image, SZ_LINE, TY_CHAR)

	# Get the image name.
	call clgstr ("image", Memc[image], SZ_LINE)
	im = immap (Memc[image], READ_ONLY, 0)
	npix = IM_LEN(im,1)

	# Get default histogram resolution.
	nbins = clgeti ("nbins")

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

	z1i = nint (z1)
	z2i = nint (z2)

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
	# Note that real numbers could be handled better - perhaps
	# adjust z2 upward by ~ EPSILONR (in ahgm itself).

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

	# List or plot the histogram.  In list format, the bin value is the
	# z value of the left side (start) of the bin.

	dz = (z2 - z1) / real (nbins)

	if (clgetb ("listout"))
	    do i = 1, nbins {
		call printf ("%g %d\n")
		    call pargr (z1 + (i-1) * dz)
		    call pargi (Memi[hgm+i-1])
	    }
	else {
	    call salloc (device, SZ_FNAME, TY_CHAR)
	    call salloc (title, SZ_TITLE, TY_CHAR)
	    call salloc (str, SZ_LINE, TY_CHAR)
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
		"%s\nHistogram from z1=%g to z2=%g, nbins=%d")
		call pargstr (IM_TITLE(im))
		call pargr (z1)
		call pargr (z2)
		call pargi (nbins)

	    # Draw the plot.  Center the bins for plot_type=line.
	    call glabax (gp, Memc[title], "", "")
	    if (clgwrd ("plot_type", Memc[str], SZ_LINE, HGM_TYPES) == HGM_BOX)
		call hgline (gp, Memr[hgmr], nbins, z1, z2)
	    else
		call gvline (gp, Memr[hgmr], nbins, z1 + dz/2., z2 - dz/2.)

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
