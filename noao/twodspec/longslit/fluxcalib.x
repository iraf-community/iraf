include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>

# T_FLUXCALIB -- CL task for applying flux calibration to longslit images.
#
# The image headers must contain the parameters DISPAXIS, W0, and WPC
# to define the wavelength coordinates in Angstroms and an exposure time
# in seconds.  
#
# The flux file is an image containing sensitivity corrections in magnitudes:
#
#	2.5 log10 ((counts/sec/Ang) / (ergs/cm2/sec/Ang))
#
# The flux file wavelengths need not be the same as the image but must
# span the entire range of the input image.  If interpolation is required
# the interpolator is a cubic spline.

procedure t_fluxcalib()

int	list1			# List of images to be calibrated
int	list2			# List of calibrated images
char	fluxfile[SZ_FNAME]	# Name of flux file
bool	fnu			# Convert to fnu?

char	image1[SZ_FNAME], image2[SZ_FNAME], history[SZ_LINE]
bool	fluxcor
pointer	im1, im2, ff, fluxdata

int	imtopen(), imtgetim()
bool	clgetb(), imgetb(), streq()
pointer	immap()
errchk	get_fluxdata(), do_fluxcalib()

data	fluxdata/NULL/

begin
	# Get task parameters.

	call clgstr ("input", history, SZ_LINE)
	list1 = imtopen (history)
	call clgstr ("output", history, SZ_LINE)
	list2 = imtopen (history)
	call clgstr ("fluxfile", fluxfile, SZ_FNAME)
	fnu = clgetb ("fnu")
	ff = immap (fluxfile, READ_ONLY, 0)

	# Loop through each pair of input and output images.  Check if the
	# input image has been corrected previously.  If TRUE then print
	# message and go on to the next input image.  If FALSE print message
	# and apply flux corrections.  Missing information in the image header
	# will return an error which will warn the user and go on to the next
	# image.

	while ((imtgetim (list1, image1, SZ_FNAME) != EOF) &&
	    (imtgetim (list2, image2, SZ_FNAME) != EOF)) {

	    # Open image to be calibrated.
	    iferr (im1 = immap (image1, READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }

	    # Check if the image has already been flux calibrated.
	    iferr (fluxcor = imgetb (im1, "fluxcor"))
		fluxcor = false
	    if (fluxcor) {
		call printf ("Image %s is flux calibrated.\n")
		    call pargstr (image1)
		call imunmap (im1)
		next
	    }

	    # Open output image
	    if (streq (image1, image2))
	        im2 = immap ("fluxcalibtemp", NEW_COPY, im1)
	    else
	        im2 = immap (image2, NEW_COPY, im1)
	    IM_PIXTYPE(im2) = TY_REAL

	    # Apply flux calibration.  If error delete output image.
	    iferr {
	        call printf ("Flux calibration: %s --> %s.\n")
		    call pargstr (image1)
		    call pargstr (image2)
	        call flush (STDOUT)
		call get_fluxdata (im1, ff, fnu, fluxdata)
		call do_fluxcalib (im1, im2, Memr[fluxdata])
		call sprintf (history, SZ_LINE,
		    "Flux calibration %s applied with fnu=%b.")
		    call pargstr (fluxfile)
		    call pargb (fnu)
		call xt_phistory (im2, history)
		call imunmap (im2)
	        call imunmap (im1)
		if (streq (image1, image2)) {
		    call imdelete (image1)
		    call imrename ("fluxcalibtemp", image1)
		}
	    } then {
		call imunmap (im2)
	        call imunmap (im1)
		call imdelete (image2)
		call printf ("!!No flux calibration for %s!!\n")
		    call pargstr (image1)
		call flush (STDOUT)
		call erract (EA_WARN)
	    }
	}

	call mfree (fluxdata, TY_REAL)
	call imunmap (ff)
	call imtclose (list1)
	call imtclose (list2)
end


# GET_FLUXDATA -- Get the flux calibration data for the mapped image.
# For efficiency read the data from the flux file only once and interpolate
# to the wavelengths of the image only if they differ from those of the
# flux file.  Correct for the dispersion and exposure time of the image
# and convert to fnu if needed.

procedure get_fluxdata (im, ff, fnu, fluxdata)

pointer	im		# IMIO pointer for image to be calibrated
pointer	ff		# IMIO pointer for the flux file
bool	fnu		# Convert to fnu?
pointer	fluxdata	# Pointer to flux data

int	i, laxis, paxis, nw, ff_nw, ff_dcflag, dcflag
char	exposure[SZ_LINE]
real	w, dw, w0, wpc, crpix, exptime, ff_w0, ff_wpc
pointer	ff_data, wavelens, asi

int	imgeti()
real	imgetr()
pointer	imgl1r()
errchk	imgeti, imgetr

define	VLIGHT	2.997925e18		# Speed of light in Angstroms/sec

begin
	# If the fluxdata pointer is NULL then initialize.

	if (fluxdata == NULL) {
	    # Determine the dispersion.

	    ff_dcflag = imgeti (ff, "dc-flag")
	    ff_w0 = imgetr (ff, "crval1")
	    iferr (ff_wpc = imgetr (ff, "cdelt1"))
	        ff_wpc = imgetr (ff, "cd1_1")
	    crpix = imgetr (ff, "crpix1")
	    ff_w0 = ff_w0 + (1 - crpix) * ff_wpc
	    ff_nw = IM_LEN (ff, 1)

	    # Read the flux file and convert to multiplicative correction.

	    ff_data = imgl1r (ff)
	    do i = ff_data, ff_data + ff_nw - 1
	        Memr[i] = 10.0 ** (-0.4 * Memr[i])
	}

	# Determine dispersion and exposure time for the image.
	call get_daxis (im, laxis, paxis)
	dcflag = imgeti (im, "dc-flag")
	if (laxis == 1) {
	    w0 = imgetr (im, "crval1")
	    iferr (wpc = imgetr (im, "cdelt1"))
	        wpc = imgetr (im, "cd1_1")
	    crpix = imgetr (im, "crpix1")
	} else {
	    w0 = imgetr (im, "crval2")
	    iferr (wpc = imgetr (im, "cdelt2"))
	        wpc = imgetr (im, "cd2_2")
	    crpix = imgetr (im, "crpix2")
	}
	w0 = w0 + (1 - crpix) * wpc
	nw = IM_LEN (im, laxis)
	call clgstr ("exposure", exposure, SZ_LINE)
	exptime = imgetr (im, exposure)
	if (exptime <= 0.)
	    call error (0, "Bad integration time in image header")

	# Allocate memory for the flux calibration data.

	call mfree (fluxdata, TY_REAL)
	call malloc (fluxdata, nw, TY_REAL)

	# Check if the data from the flux file needs to be interpolated.

	if ((w0 != ff_w0) || (wpc != ff_wpc) || (nw != ff_nw)) {
	    # Compute the interpolation wavelengths.

	    call malloc (wavelens, nw, TY_REAL)
	    if ((ff_dcflag == 1) && (dcflag == 0))
	        do i = 1, nw
		    Memr[wavelens+i-1] = (log10 (w0+(i-1)*wpc) - ff_w0) /
			ff_wpc + 1
	    else if ((ff_dcflag == 0) && (dcflag == 1))
	        do i = 1, nw
		    Memr[wavelens+i-1] = (10. ** (w0+(i-1)*wpc) - ff_w0) /
			ff_wpc + 1
	    else
	        do i = 1, nw
		    Memr[wavelens+i-1] = ((w0+(i-1)*wpc) - ff_w0) / ff_wpc + 1

	    if ((Memr[wavelens] < 1.) || (Memr[wavelens+nw-1] > ff_nw)) {
		if ((Memr[wavelens]<0.5) || (Memr[wavelens+nw-1]>ff_nw+0.5))
		    call eprintf (
		    "Warning: Wavelengths extend beyond flux calibration\n.")
		call arltr (Memr[wavelens], nw, 1., 1.)
		call argtr (Memr[wavelens], nw, real(ff_nw), real(ff_nw))
	    }

	    # Fit an interpolation cubic spline and evaluate.

	    call asiinit (asi, II_SPLINE3)
	    call asifit (asi, Memr[ff_data], ff_nw)
	    call asivector (asi, Memr[wavelens], Memr[fluxdata], nw)
	    call asifree (asi)
	    call mfree (wavelens, TY_REAL)
	} else
	    call amovr (Memr[ff_data], Memr[fluxdata], nw)

	# Convert to flux

	if (fnu) {
	    if (dcflag == 0) {
	        do i = 1, nw {
		    w = w0 + (i - 1) * wpc
		    dw = wpc
		    Memr[fluxdata+i-1] = Memr[fluxdata+i-1] / exptime / dw *
			w**2 / VLIGHT
	        }
	    } else {
	        do i = 1, nw {
		    w = 10. ** (w0 + (i - 1) * wpc)
		    dw = 2.30259 * wpc * w
		    Memr[fluxdata+i-1] = Memr[fluxdata+i-1] / exptime / dw *
		        w**2 / VLIGHT
	        }
	    }
	} else {
	    if (dcflag == 0) {
		dw = wpc
	        call amulkr (Memr[fluxdata], 1./dw/exptime, Memr[fluxdata], nw)
	    } else {
	        do i = 1, nw {
		    dw = 2.30259 * wpc * (10. ** (w0 + (i - 1) * wpc))
		    Memr[fluxdata+i-1] = Memr[fluxdata+i-1] / exptime / dw
	        }
	    }
	}
end


# DO_FLUXCALIB -- Apply the flux calibration to a mapped image.
# This procedure works for images of any dimension.

procedure do_fluxcalib (im1, im2, fluxdata)

pointer	im1			# IMIO pointer for image to be calibrated
pointer	im2			# IMIO pointer for calibrated image
real	fluxdata[ARB]		# Flux calibration data

int	laxis, paxis, nw, npts
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
pointer	in, out

int	imgnlr(), impnlr()
errchk	get_daxis

begin
	# Determine the dispersion axis of the image.

	call get_daxis (im1, laxis, paxis)
	nw = IM_LEN (im1, laxis)

	# Calibrate the image.

	npts = IM_LEN (im1, 1)
	call amovkl (long (1), v1, IM_MAXDIM)
	call amovkl (long (1), v2, IM_MAXDIM)

	if (laxis == 1) {
	    while ((imgnlr(im1, in, v1) != EOF) &&
		(impnlr(im2, out, v2) != EOF))
		call amulr (Memr[in], fluxdata, Memr[out], npts)

	} else {
	    while ((imgnlr(im1, in, v1) != EOF) &&
		(impnlr(im2, out, v2) != EOF))
		call amulkr (Memr[in], fluxdata[v1[laxis]-1], Memr[out],
		    npts)
	}

	# Add the flux correction flag and return.

	call imaddb (im2, "fluxcor", true)
	call imaddi (im2, "ca-flag", 0)
end
