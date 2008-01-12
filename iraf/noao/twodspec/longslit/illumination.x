include	<imhdr.h>
include	<error.h>
include	<math/iminterp.h>
include	<pkg/gtools.h>
include	<pkg/rg.h>
include	<pkg/xtanswer.h>

# T_ILLUMINATION -- Determine the illumination function for longslit spectra.
#
# The calibration image is binned in wavelength.  Each wavelength  bin is
# then smoothed by curve fitting and normalized to the middle point.
# Finally the binned image is interpolated back to the original image
# dimension.  The binning and curve fitting may be performed interactively.
# A illumination function is determined for each input images.  Image
# sections in the input image allow only parts of the illumination function
# to be created.  Thus, multiple slits in the same image may have
# independent illumination functions on the same illumination image.

# CL callable procedure.
#
# The input and output images are given by image templates.  The
# number of output images must match the number of input images.
# Input image sections are allowed.

procedure t_illumination ()

pointer	image1
pointer	image2
int	list1				# Calibration image list
int	list2				# Illumination image list
int	interactive			# Interactive?
int	naverage			# Sample averaging size
int	order				# Order of curve fitting function
real	low_reject, high_reject		# Rejection thresholds
int	niterate			# Number of rejection iterations
real	grow				# Rejection growing radius

int	answer
char	history[SZ_LINE]
pointer	in, out, ic, gt, sp, str

int	clgeti(), imtopen(), imtgetim(), imtlen(), gt_init()
bool	clgetb()
real	clgetr()
errchk	il_make

begin
	call smark (sp)
	call salloc (image1, SZ_LINE, TY_CHAR)
	call salloc (image2, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
 
	# Get calibration and illumination image template lists.

	call clgstr ("images", Memc[image1], SZ_LINE)
	call clgstr ("illuminations", Memc[image2], SZ_LINE)

	# Check that the number of illumination calibration images are the same.

	list1 = imtopen (Memc[image1])
	list2 = imtopen (Memc[image2])
	if (imtlen (list1) != imtlen (list2)) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call error (0,
		"The number of input and output images are not the same.")
	}

	# Get other parameters and initialize the curve fitting package.

	if (clgetb ("interactive"))
	    interactive = YES
	else
	    interactive = ALWAYSNO

	call clgstr ("sample", Memc[image1], SZ_LINE)
	naverage = clgeti ("naverage")
	call clgstr ("function", Memc[str], SZ_LINE)
	order = clgeti ("order")
	low_reject = clgetr ("low_reject")
	high_reject = clgetr ("high_reject")
	niterate = clgeti ("niterate")
	grow = clgetr ("grow")

	# Set the ICFIT pointer structure.
	call ic_open (ic)
	call ic_pstr (ic, "sample", Memc[image1])
	call ic_puti (ic, "naverage", naverage)
	call ic_pstr (ic, "function", Memc[str])
	call ic_puti (ic, "order", order)
	call ic_putr (ic, "low", low_reject)
	call ic_putr (ic, "high", high_reject)
	call ic_puti (ic, "niterate", niterate)
	call ic_putr (ic, "grow", grow)
	call ic_pstr (ic, "ylabel", "")

	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")

	# Create an illumination image for each calibration image
	while ((imtgetim (list1, Memc[image1], SZ_LINE) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_LINE) != EOF)) {

	    call ls_immap (Memc[image1], Memc[image2], in, out)

	    call sprintf (Memc[str], SZ_LINE,
		"Determine illumination interactively for %s")
	        call pargstr (Memc[image1])
	    call xt_answer (Memc[str], interactive)
	    answer = interactive

	    iferr {
		call il_make (in, out, ic, gt, Memc[str], answer)
 
	        call imaddr (out, "ccdmean", 1.)
	        call sprintf (history, SZ_LINE,
		    "Illumination correction determined from %s.")
		    call pargstr (Memc[image1])
	        call imastr (out, "mkillum", history)
	        call imunmap (in)
	        call imunmap (out)
	    } then {
		call erract (EA_WARN)
	        call imunmap (in)
	        call imunmap (out)
		call imdelete (Memc[image2])
	    }
	}

	call ic_closer (ic)
	call gt_free (gt)
	call imtclose (list1)
	call imtclose (list2)
	call sfree (sp)
end


# IL_MAKE -- Given the calibration and illumination image descriptors
# make the illumination function.

procedure il_make (in, out, ic, gt, title, interactive)

pointer	in			# Calibration IMIO pointer
pointer	out			# Illumination IMIO pointer
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
char	title[ARB]		# Title
int	interactive		# Interactive?

char	graphics[SZ_FNAME]	# Graphics output device
int	i, laxis, paxis, axis, npts, nbins, len_title
pointer	bins, cv, gp, sp, x, y, z, z1, wts

pointer	gopen()
int	strlen()
errchk	get_daxis

begin
	# Determine the slit axis and set the axis labels.
	call get_daxis (in, laxis, paxis)
	if (laxis == 1)
	    axis = 2
	else
	    axis = 1

	switch (axis) {
	case 1:
	    call ic_pstr (ic, "xlabel", "Column")
	case 2:
	    call ic_pstr (ic, "xlabel", "Line")
	}

	# Set the bins and bin the calibration image.

	switch (axis) {
	case 1:
	    call il_setbins (in, 2, interactive, bins)
	case 2:
	    call il_setbins (in, 1, interactive, bins)
	}

	call il_binimage (in, axis, bins, x, y, z, npts, nbins)
	call rg_free (bins)

	# Allocate memory for the fit.

	call smark (sp)
	call salloc (wts, npts, TY_REAL)
	call amovkr (1., Memr[wts], npts)

	# Smooth each bin.
		
	call ic_putr (ic, "xmin", Memr[x])
	call ic_putr (ic, "xmax", Memr[x+npts-1])

	len_title = strlen (title)
	z1 = z

	do i = 1, nbins {
	    title[len_title + 1] = EOS
	    call sprintf (title, SZ_LINE, "%s at bin %d")
		call pargstr (title)
		call pargi (i)
	    call xt_answer (title, interactive)

	    if ((interactive == YES) || (interactive == ALWAYSYES)) {
	        call sprintf (title, SZ_LINE, "%s\n%s")
	            call pargstr (title)
	            call pargstr (IM_TITLE(in))
	        call gt_sets (gt, GTTITLE, title)

		call clgstr ("graphics", graphics, SZ_FNAME)
	        gp = gopen (graphics, NEW_FILE, STDGRAPH)
	        call icg_fit (ic, gp, "cursor", gt, cv, Memr[x], Memr[z1],
		    Memr[wts], npts)
		call amovkr (1., Memr[wts], npts)
	        call gclose (gp)
	    } else {
	        call ic_fit (ic, cv, Memr[x], Memr[z1], Memr[wts], npts,
		    YES, YES, YES, YES)
	    }

	    call cvvector (cv, Memr[x], Memr[z1], npts)
	    z1 = z1 + npts
	}
	call cvfree (cv)

	# Compute the illumination image by linear interpolation.

	call il_expand (out, axis, Memr[x], Memr[y], Memr[z], npts, nbins)

	# Free allocated memory.

	call mfree (x, TY_REAL)
	call mfree (y, TY_REAL)
	call mfree (z, TY_REAL)
	call sfree (sp)
end


# IL_BINIMAGE -- Read the calibration image and bin it.

procedure il_binimage (im, axis, bins, x, y, z, npts, nbins)

pointer im			# Calibration IMIO pointer
int	axis			# Slit axis
pointer	bins			# Bins
pointer	x			# Slit positions
pointer	y			# Dispersion positions of bins
pointer	z			# Binned image
int	npts			# Number of points per bin
int	nbins			# Number of bins

int	i, y1, y2
pointer	z1

begin
	# Allocate memory.

	npts = IM_LEN (im, axis)
	nbins = RG_NRGS (bins)
	call malloc (y, nbins, TY_REAL)
	call malloc (z, npts * nbins, TY_REAL)

	# Bin the image data.

	x = NULL
	do i = 1, nbins {
	    y1 = RG_X1 (bins, i)
	    y2 = RG_X2 (bins, i)
	    Memr[y+i-1] = (y1 + y2) / 2

	    call mfree (x, TY_REAL)
	    switch (axis) {
	    case 1:
	        call ls_aimavg (im, axis, 1, IM_LEN(im, 1), y1, y2, x, z1, npts)
	    case 2:
	        call ls_aimavg (im, axis, y1, y2, 1, IM_LEN(im, 2), x, z1, npts)
	    }
	    call amovr (Memr[z1], Memr[z+(i-1)*npts], npts)
	    call mfree (z1, TY_REAL)
	}
end


# IL_EXPAND -- Expand the reduced illumination back to the original size.
# This procedure request the interpolation type.

procedure il_expand (im, axis, x, y, z, nx, ny)

pointer	im			# Illumination image pointer
int	axis			# Slit axis
real	x[nx]			# Slit coordinates
real	y[ny]			# Dispersion coordinates
real	z[nx, ny]		# Slit profile
int	nx			# Number of points per slit profile
int	ny			# Number of slit profiles

char	dummy[7]
int	nyout, ncols, nlines
int	i, j, y1, y2
real	dy
pointer	msi, sp, out, yout

int	clgwrd()
pointer	impl2r()

int	msitypes[5]
data	msitypes/II_BINEAREST,II_BILINEAR,II_BIPOLY3,II_BIPOLY5,II_BISPLINE3/
string	msinames "|nearest|linear|poly3|poly5|spline3|"

begin
	ncols = IM_LEN (im, 1)
	nlines = IM_LEN (im, 2)

	# Normalize illumination to the center of each slit.

	i = nx / 2 - 1
	do j = 1, ny {
	    dy = z[i, j]
	    call adivkr (z[1, j], dy, z[1, j], nx)
	}

	# If there is only one slit profile then copy the profile to each
	# image line or column.

	if (ny == 1) {
	    switch (axis) {
	    case 1:
	        do i = 1, nlines
	            call amovr (z, Memr[impl2r (im, i)], ncols)
	    case 2:
	        do i = 1, nlines
		    call amovkr (z[i, 1], Memr[impl2r (im, i)], ncols)
	    }

	    return
	}

	# If there is more than one slit profile fit a 2D interpolator.

	i = clgwrd ("interpolator", dummy, 7, msinames)
	if (i == 0)
	    i = II_BILINEAR
	else
	    i = msitypes[i]

	switch (i) {
	case II_POLY3, II_SPLINE3:
	    if (ny < 4)
		i = II_BILINEAR
	case II_POLY5:
	    if (ny < 6) {
		if (ny < 4)
		   i = II_BILINEAR
		else
		   i = II_POLY3
	    }
	}

	call msiinit (msi, i)
	call msifit (msi, z, nx, ny, nx)

	# Set the output grid in terms of the interpolation surface.

	switch (axis) {
	case 1:
	    nyout = IM_LEN (im, 2)
	case 2:
	    nyout = IM_LEN (im, 1)
	}

	call smark (sp)
	call salloc (yout, nyout, TY_REAL)

	y1 = 1
	y2 = y[1]
	do i = y1, y2
	    Memr[yout+i-1] = 1
	do j = 2, ny {
	    y1 = y2 + 1
	    y2 = y[j]
	    dy = 1. / (y2 - y1)
	    do i = y1, y2
		Memr[yout+i-1] = j - 1 + (i - y1) * dy
	    }
	y1 = y2 + 1
	y2 = nyout
	do i = y1, y2
	    Memr[yout+i-1] = ny

	# Evaluate the interpolation surface on the output grid.

	ncols = IM_LEN (im, 1)
	nlines = IM_LEN (im, 2)
	call salloc (out, ncols, TY_REAL)

	switch (axis) {
	case 1:
	    do i = 1, nlines {
	        call amovkr (Memr[yout+i-1], Memr[out], ncols)
	        call msivector (msi, x, Memr[out], Memr[impl2r (im, i)],
		    ncols)
	    }
	case 2:
	    do i = 1, nlines {
	        call amovkr (x[i], Memr[out], ncols)
	        call msivector (msi, Memr[out], Memr[yout], Memr[impl2r(im, i)],
		    ncols)
	    }
	}

	call sfree (sp)
end
