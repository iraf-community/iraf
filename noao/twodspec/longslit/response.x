include	<imhdr.h>
include	<pkg/gtools.h>
include <pkg/xtanswer.h>

# T_RESPONSE -- Determine the response function for 2D spectra.
#
# A calibration image is divided by a normalization spectrum to form
# a response image.  The normalization spectrum is derived by averaging
# the normalization image across dispersion.  The normalization spectrum
# is then smoothed by curve fitting.  The smoothed normalization
# spectrum is divided into the calibration image to form the response
# function image.  The curve fitting may be performed interactively
# using the icfit package.  A response function is determined for each
# input image.  Image sections in the calibration image may be used to determine
# the response for only part of an image such as with multiple slits.

# CL callable task.
#
# The images are given by image templates.  The number of images must
# in each list must match.  Image sections are allowed in the calibration
# image.

procedure t_response ()

int	list1				# List of calibration images
int	list2				# List of normalization images
int	list3				# List of response images
real	threshold			# Response threshold
int	naverage			# Sample averaging size
int	order				# Order of curve fitting function
real	low_reject, high_reject		# Rejection thresholds
int	niterate			# Number of rejection iterations
real	grow				# Rejection growing radius
int	interactive			# Interactive?

pointer	cal, norm, resp, ic, gt
pointer	sp, image1, image2, image3, history

int	clgeti(), imtopen(), imtgetim(), imtlen(), gt_init(), ic_geti()
bool	clgetb()
real	clgetr(), ic_getr()
pointer	immap()

errchk	immap, ls_immap

begin
	call smark (sp)
	call salloc (image1, SZ_LINE, TY_CHAR)
	call salloc (image2, SZ_LINE, TY_CHAR)
	call salloc (image3, SZ_LINE, TY_CHAR)
	call salloc (history, SZ_LINE, TY_CHAR)

	# Get the calibration, normalization, and response image lists and
	# check that the they match.

	call clgstr ("calibration", Memc[image1], SZ_LINE)
	call clgstr ("normalization", Memc[image2], SZ_LINE)
	call clgstr ("response", Memc[image3], SZ_LINE)

	list1 = imtopen (Memc[image1])
	list2 = imtopen (Memc[image2])
	list3 = imtopen (Memc[image3])
	if ((imtlen(list1)!=imtlen(list3)) || (imtlen(list2)!=imtlen(list3))) {
	    call imtclose (list1)
	    call imtclose (list2)
	    call imtclose (list3)
	    call error (0,  "Image lists do not match")
	}

	# Get remaining parameters and initialize the curve fitting package.

	threshold = clgetr ("threshold")
	call clgstr ("sample", Memc[image1], SZ_LINE)
	naverage = clgeti ("naverage")
	call clgstr ("function", Memc[image2], SZ_LINE)
	order = clgeti ("order")
	low_reject = clgetr ("low_reject")
	high_reject = clgetr ("high_reject")
	niterate = clgeti ("niterate")
	grow = clgetr ("grow")
	if (clgetb ("interactive"))
	    interactive = YES
	else
	    interactive = ALWAYSNO

	# Set the ICFIT pointer structure.
	call ic_open (ic)
	call ic_pstr (ic, "sample", Memc[image1])
	call ic_puti (ic, "naverage", naverage)
	call ic_pstr (ic, "function", Memc[image2])
	call ic_puti (ic, "order", order)
	call ic_putr (ic, "low", low_reject)
	call ic_putr (ic, "high", high_reject)
	call ic_puti (ic, "niterate", niterate)
	call ic_putr (ic, "grow", grow)
	call ic_pstr (ic, "ylabel", "")

	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")

	# Create the response image for each calibration image.

	while ((imtgetim (list1, Memc[image1], SZ_LINE) != EOF) &&
	    (imtgetim (list2, Memc[image2], SZ_LINE) != EOF) &&
	    (imtgetim (list3, Memc[image3], SZ_LINE) != EOF)) {

	    # Map the images.  If the response image does not exist it
	    # is created and initialized to unit response everywhere.
	    # If the calibration image is an image section then the response
	    # image is opened as a section also.

	    call ls_immap (Memc[image1], Memc[image3], cal, resp)
	    norm = immap (Memc[image2], READ_ONLY, 0)

	    # Determine whether the normalization spectrum is to be fit
	    # interactively and if so set the graphics title.

	    call sprintf (Memc[image2], SZ_LINE,
		"Fit the normalization spectrum for %s interactively")
	        call pargstr (Memc[image1])
	    call xt_answer (Memc[image2], interactive)

	    if ((interactive == YES) || (interactive == ALWAYSYES)) {
	        call sprintf (Memc[image2], SZ_LINE,
		    "Fit the normalization spectrum for %s\n%s")
	            call pargstr (Memc[image1])
	            call pargstr (IM_TITLE(cal))
	        call gt_sets (gt, GTTITLE, Memc[image2])
	    }

	    # Make the response.
	    call re_make (cal, norm, resp, ic, gt, threshold, interactive)

	    # Document the fit.
	    call ic_gstr (ic, "sample", Memc[history], SZ_LINE)
	    call clpstr ("sample", Memc[history])
	    naverage = ic_geti (ic, "naverage")
	    call clputi ("naverage", naverage)
	    call ic_gstr (ic, "function", Memc[history], SZ_LINE)
	    call clpstr ("function", Memc[history])
	    order = ic_geti (ic, "order")
	    call clputi ("order", order)
	    low_reject = ic_getr (ic, "low")
	    call clputr ("low_reject", low_reject)
	    high_reject = ic_getr (ic, "high")
	    call clputr ("high_reject", high_reject)
	    niterate = ic_geti (ic, "niterate")
	    call clputi ("niterate", niterate)
	    grow = ic_getr (ic, "grow")
	    call clputr ("grow", grow)

	    call imaddr (resp, "ccdmean", 1.)
	    call sprintf (Memc[history], SZ_LINE,
		"Response determined from %s.")
		call pargstr (Memc[image2])
	    call xt_phistory (resp, Memc[history])
	    call imunmap (cal)
	    call imunmap (norm)
	    call imunmap (resp)
	}

	# Finish up.

	call ic_closer (ic)
	call imtclose (list1)
	call imtclose (list2)
	call imtclose (list3)
	call gt_free (gt)
	call sfree (sp)
end


# RE_MAKE -- Given the calibration image determine the response.

procedure re_make (cal, norm, resp, ic, gt, threshold, interactive)

pointer	cal			# Calibration IMIO pointer
pointer	norm			# Normalization IMIO pointer
pointer	resp			# Response IMIO pointer
pointer	ic			# ICFIT pointer
pointer	gt			# GTOOLS pointer
real	threshold		# Response threshold
int	interactive		# Interactive?

char	graphics[SZ_FNAME]	# Graphics output device
int	laxis, paxis, npts
pointer	cv, gp, sp, wavelengths, spectrum, wts

pointer	gopen()
errchk	get_daxis

begin
	# Determine the dispersion axis and set the axis labels.
	call get_daxis (cal, laxis, paxis)

	switch (laxis) {
	case 1:
	    call ic_pstr (ic, "xlabel", "Column")
	case 2:
	    call ic_pstr (ic, "xlabel", "Line")
	}

	# Get the normalization spectrum.

	call ls_aimavg (norm, laxis, 1, IM_LEN(norm, 1), 1, IM_LEN(norm, 2),
	    wavelengths, spectrum, npts)

	# Allocate memory for the fit.

	call smark (sp)
	call salloc (wts, npts, TY_REAL)
	call amovkr (1., Memr[wts], npts)

	# Smooth the normalization spectrum.

	call ic_putr (ic, "xmin", Memr[wavelengths])
	call ic_putr (ic, "xmax", Memr[wavelengths+npts-1])

	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    call clgstr ("graphics", graphics, SZ_FNAME)
	    gp = gopen (graphics, NEW_FILE, STDGRAPH)
	    call icg_fit (ic, gp, "cursor", gt, cv, Memr[wavelengths],
		Memr[spectrum], Memr[wts], npts)
	    call gclose (gp)
	} else {
	    call ic_fit (ic, cv, Memr[wavelengths], Memr[spectrum], Memr[wts],
		npts, YES, YES, YES, YES)
	}

	call cvvector (cv, Memr[wavelengths], Memr[spectrum], npts)
	call cvfree (cv)

	# Compute the response image by normalizing the calibration
	# image by the normalization spectrum.

	call re_normalize (cal, resp, laxis, threshold, Memr[spectrum], npts)

	# Free allocated memory.

	call sfree (sp)
	call mfree (wavelengths, TY_REAL)
	call mfree (spectrum, TY_REAL)
end
	

# RE_NORMALIZE -- Divide each calibration image pixel by the normalization
# spectrum at that pixel.

procedure re_normalize (cal, resp, axis, threshold, spectrum, npts)

pointer	cal			# Calibration IMIO pointer
pointer	resp			# Response IMIO pointer
int	axis			# Dispersion axis
real	threshold		# Normalization treshold
real	spectrum[npts]		# Pointer to normalization spectrum
int	npts			# Number of points in spectrum

int	i, j, ncols, nlines
real	norm
pointer	datain, dataout

pointer	imgl2r(), impl2r()

begin
	ncols = IM_LEN (cal, 1)
	nlines = IM_LEN (cal, 2)

	# Compute the response image.
	if (IS_INDEF (threshold)) {
	    do i = 1, nlines {
		datain = imgl2r (cal, i)
		dataout = impl2r (resp, i)

		switch (axis) {
		case 1:
		    call adivr (Memr[datain], spectrum, Memr[dataout], ncols)
		case 2:
		    call adivkr (Memr[datain], spectrum[i], Memr[dataout],
			ncols)
		}
	    }
	} else {
	    do i = 1, nlines {
		datain = imgl2r (cal, i)
		dataout = impl2r (resp, i)

		switch (axis) {
		case 1:
		    do j = 1, ncols {
			norm = spectrum[j]
			if (norm < threshold || Memr[datain] < threshold)
			    Memr[dataout] = 1.
			else
			    Memr[dataout] = Memr[datain] / norm
			datain = datain + 1
			dataout = dataout + 1
		    }
		case 2:
		    norm = spectrum[i]
		    if (norm < threshold)
			call amovkr (1., Memr[dataout], ncols)
		    else {
			do j = 1, ncols {
			    if (Memr[datain] < threshold)
				Memr[dataout] = 1.
			    else
				Memr[dataout] = Memr[datain] / norm
			    datain = datain + 1
			    dataout = dataout + 1
			}
		    }
		}
	    }
	}
end
