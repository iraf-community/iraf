include <error.h>
include <mach.h>
include <imhdr.h>
include <imset.h>
include <fset.h>
include <math.h>
include "starfind.h"


# SF_FIND -- Find stars in an image using a pattern matching technique and
# a circularly symmetric Gaussian pattern.

procedure sf_find (im, out, sf, nxblock, nyblock, wcs, wxformat, wyformat,
	boundary, constant, verbose)

pointer	im		#I pointer to the input image
int	out		#I the output file descriptor
pointer	sf		#I pointer to the apphot structure
int	nxblock		#I the x dimension blocking factor
int	nyblock		#I the y dimension blocking factor
char	wcs[ARB]	#I the world coordinate system
char	wxformat[ARB]	#I the x axis world coordinate format
char	wyformat[ARB]	#I the y axis world coordinate format
int	boundary	#I type of boundary extension
real	constant	#I constant for constant boundary extension
int	verbose		#I verbose switch

int	i, j, fwidth, swidth, norm
int	l1, l2, c1, c2, ncols, nlines, nxb, nyb, nstars, stid
pointer	sp, gker2d, ngker2d, skip, fmtstr, twxformat, twyformat
pointer	imbuf, denbuf, str, mw, ct
real	sigma, nsigma, a, b, c, f, gsums[LEN_GAUSS], relerr, dmin, dmax
real	maglo, maghi

bool	streq()
int	sf_stfind()
pointer	mw_openim(), mw_sctran()
real	sf_egkernel()
errchk	mw_openim(), mw_sctran(), mw_gattrs()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (twxformat, SZ_FNAME, TY_CHAR)
	call salloc (twyformat, SZ_FNAME, TY_CHAR)
	call salloc (fmtstr, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Compute the parameters of the Gaussian kernel.
	sigma = HWHM_TO_SIGMA * SF_HWHMPSF(sf)
	nsigma =  SF_FRADIUS(sf) / HWHM_TO_SIGMA
	call sf_egparams (sigma, 1.0, 0.0, nsigma, a, b, c, f, fwidth, fwidth)

	# Compute the separation parameter
	swidth = max (2, int (SF_SEPMIN(sf) * SF_HWHMPSF(sf) + 0.5))

	# Compute the minimum and maximum pixel values.
	if (IS_INDEFR(SF_DATAMIN(sf)) && IS_INDEFR(SF_DATAMAX(sf))) {
	    norm = YES
	    dmin = -MAX_REAL
	    dmax = MAX_REAL
	} else {
	    norm = NO
	    if (IS_INDEFR(SF_DATAMIN(sf)))
		dmin = -MAX_REAL
	    else
		dmin = SF_DATAMIN(sf)
	    if (IS_INDEFR(SF_DATAMAX(sf)))
		dmax = MAX_REAL
	    else
		dmax = SF_DATAMAX(sf)
	}

	# Compute the magnitude limits
	if (IS_INDEFR(SF_MAGLO(sf)))
	    maglo = -MAX_REAL
	else
	    maglo = SF_MAGLO(sf)
	if (IS_INDEFR(SF_MAGHI(sf)))
	    maghi = MAX_REAL
	else
	    maghi = SF_MAGHI(sf)

	# Open the image WCS. 
	if (wcs[1] == EOS) {
	    mw = NULL
	    ct = NULL
	} else {
	    iferr {
	        mw = mw_openim (im)
	    } then {
		call erract (EA_WARN)
		mw = NULL
	        ct = NULL
	    } else {
	        iferr {
		    ct = mw_sctran (mw, "logical", wcs, 03B)
		} then {
		    call erract (EA_WARN)
		    ct = NULL
		    call mw_close (mw)
		    mw = NULL
		}
	    }
	}

	# Set the WCS formats.
	if (ct == NULL)
	    call strcpy (wxformat, Memc[twxformat], SZ_FNAME)
	else if (wxformat[1] == EOS) {
	    if (mw != NULL) {
                iferr (call mw_gwattrs (mw, 1, "format", Memc[twxformat],
		    SZ_FNAME)) {
		    if (streq (wcs, "world")) 
	        	call strcpy ("%11.8g", Memc[twxformat], SZ_FNAME)
		    else
	        	call strcpy ("%9.3f", Memc[twxformat], SZ_FNAME)
		}
	    } else
	        call strcpy ("%9.3f", Memc[twxformat], SZ_FNAME)
	} else
	    call strcpy (wxformat, Memc[twxformat], SZ_FNAME)
	if (ct == NULL)
	    call strcpy (wyformat, Memc[twyformat], SZ_FNAME)
	else if (wyformat[1] == EOS) {
	    if (mw != NULL) {
                iferr (call mw_gwattrs (mw, 2, "format", Memc[twyformat],
		    SZ_FNAME)) {
		    if (streq (wcs, "world")) 
	        	call strcpy ("%11.8g", Memc[twyformat], SZ_FNAME)
		    else
	        	call strcpy ("%9.3f", Memc[twyformat], SZ_FNAME)
		}
	    } else
	        call strcpy ("%9.3f", Memc[twyformat], SZ_FNAME)
	} else
	    call strcpy (wyformat, Memc[twyformat], SZ_FNAME)

	# Create the output format string.
	call sprintf (Memc[fmtstr],
	    SZ_LINE, "   %s %s %s %s %s %s %s %s %s %s %s\n")
	    call pargstr ("%9.3f")
	    call pargstr ("%9.3f")
	    call pargstr (Memc[twxformat])
	    call pargstr (Memc[twyformat])
	    call pargstr ("%7.2f")
	    call pargstr ("%6d")
	    call pargstr ("%6.2f")
	    call pargstr ("%6.3f")
	    call pargstr ("%6.1f")
	    call pargstr ("%7.3f")
	    call pargstr ("%6d")

	# Set up the image boundary extension characteristics.
        call imseti (im, IM_TYBNDRY, boundary)
        call imseti (im, IM_NBNDRYPIX, 1 + fwidth / 2 + swidth)
        if (boundary == BT_CONSTANT)
            call imsetr (im, IM_BNDRYPIXVAL, constant)

	# Set up the blocking factor.
	# Compute the magnitude limits
	if (IS_INDEFI(nxblock))
	    nxb = IM_LEN(im,1)
	else
	    nxb = nxblock
	if (IS_INDEFI(nyblock))
	    nyb = IM_LEN(im,2)
	else
	    nyb = nyblock

	# Print the detection criteria on the standard output.
	if (verbose == YES) {
	    call fstats (out, F_FILENAME, Memc[str], SZ_LINE)
	    call printf ("\nImage: %s  Output: %s\n")
		call pargstr (IM_HDRFILE(im))
		call pargstr (Memc[str])
	    call printf ("Detection Parameters\n")
	    call printf (
	    "    Hwhmpsf: %0.3f (pixels)  Threshold: %g (ADU) Npixmin: %d\n")
		call pargr (SF_HWHMPSF(sf))
		call pargr (SF_THRESHOLD(sf))
		call pargi (SF_NPIXMIN(sf))
	    call printf ("    Datamin: %g (ADU)  Datamax: %g (ADU)\n")
		call pargr (SF_DATAMIN(sf))
		call pargr (SF_DATAMAX(sf))
	    call printf ("    Fradius: %0.3f (HWHM)  Sepmin: %0.3f (HWHM)\n\n")
		call pargr (SF_FRADIUS(sf))
		call pargr (SF_SEPMIN(sf))
	}

	if (out != NULL) {
	    call fstats (out, F_FILENAME, Memc[str], SZ_LINE)
	    call fprintf (out, "\n# Image: %s  Output: %s\n")
		call pargstr (IM_HDRFILE(im))
		call pargstr (Memc[str])
	    call fprintf (out, "# Detection Parameters\n")
	    call fprintf (out,
	    "#     Hwhmpsf: %0.3f (pixels)  Threshold: %g (ADU)  Npixmin: %d\n")
		call pargr (SF_HWHMPSF(sf))
		call pargr (SF_THRESHOLD(sf))
		call pargi (SF_NPIXMIN(sf))
	    call fprintf (out, "#     Datamin: %g (ADU)  Datamax: %g (ADU)\n")
		call pargr (SF_DATAMIN(sf))
		call pargr (SF_DATAMAX(sf))
	    call fprintf (out, "#     Fradius: %g (HWHM)  Sepmin: %g (HWHM)\n")
		call pargr (SF_FRADIUS(sf))
		call pargr (SF_SEPMIN(sf))
	    call fprintf (out, "# Selection Parameters\n")
		call pargi (SF_NPIXMIN(sf))
	    call fprintf (out, "#     Maglo: %0.3f  Maghi: %0.3f\n")
		call pargr (SF_MAGLO(sf))
		call pargr (SF_MAGHI(sf))
	    call fprintf (out, "#     Roundlo: %0.3f Roundhi: %0.3f\n")
		call pargr (SF_ROUNDLO(sf))
		call pargr (SF_ROUNDHI(sf))
	    call fprintf (out, "#     Sharplo: %0.3f  Sharphi: %0.3f\n")
		call pargr (SF_SHARPLO(sf))
		call pargr (SF_SHARPHI(sf))
	    call fprintf (out, "# Columns\n")
	        call fprintf (out, "#     1: X      2: Y \n")
	    if (ct == NULL) {
	        call fprintf (out, "#     3: Mag    4: Area\n")
	        call fprintf (out, "#     5: Hwhm   6: Roundness\n")
	        call fprintf (out, "#     7: Pa     8: Sharpness\n\n")
	    } else {
	        call fprintf (out, "#     3: Wx     4: Wy \n")
	        call fprintf (out, "#     5: Mag    6: Area\n")
	        call fprintf (out, "#     7: Hwhm   8: Roundness\n")
	        call fprintf (out, "#     9: Pa    10: Sharpness\n\n")
	    }
	}

	# Process the image block by block.
	stid = 1
	nstars = 0
	do j = 1, IM_LEN(im,2), nyb {

	    l1 = j
	    l2 = min (IM_LEN(im,2), j + nyb - 1)
	    nlines = l2 - l1 + 1 + 2 * (fwidth / 2 + swidth)

	    do i = 1, IM_LEN(im,1), nxb {

		# Allocate space for the convolution kernel.
		call malloc (gker2d, fwidth * fwidth, TY_REAL)
		call malloc (ngker2d, fwidth * fwidth, TY_REAL)
		call malloc (skip, fwidth * fwidth, TY_INT)

		# Allocate space for the data and the convolution.
	        c1 = i
	        c2 = min (IM_LEN(im,1), i + nxb - 1)
		ncols = c2 - c1 + 1 + 2 * (fwidth / 2 + swidth)
		call malloc (imbuf, ncols * nlines, TY_REAL)
		call malloc (denbuf, ncols * nlines, TY_REAL)

		# Compute the convolution kernels.
		relerr = sf_egkernel (Memr[gker2d], Memr[ngker2d], Memi[skip],
	    	    fwidth, fwidth, gsums, a, b, c, f)

		# Do the convolution.
		if (norm == YES)
	            call sf_fconvolve (im, c1, c2, l1, l2, swidth, Memr[imbuf],
		        Memr[denbuf], ncols, nlines, Memr[ngker2d], Memi[skip],
			fwidth, fwidth)
		else
	            call sf_gconvolve (im, c1, c2, l1, l2, swidth, Memr[imbuf],
		        Memr[denbuf], ncols, nlines, Memr[gker2d], Memi[skip],
			fwidth, fwidth, gsums, dmin, dmax)

	        # Find the stars.
		nstars = sf_stfind (out, Memr[imbuf], Memr[denbuf], ncols,
		    nlines, c1, c2, l1, l2, swidth, Memi[skip], fwidth,
		    fwidth, SF_HWHMPSF(sf), SF_THRESHOLD(sf), dmin, dmax,
		    ct, SF_NPIXMIN(sf), maglo, maghi, SF_ROUNDLO(sf),
		    SF_ROUNDHI(sf), SF_SHARPLO(sf), SF_SHARPHI(sf),
		    Memc[fmtstr], stid, verbose)

		# Increment the sequence number.
		stid = stid + nstars

		# Free the memory.
		call mfree (imbuf, TY_REAL)
		call mfree (denbuf, TY_REAL)
		call mfree (gker2d, TY_REAL)
		call mfree (ngker2d, TY_REAL)
		call mfree (skip, TY_INT)
	    }
	}

	# Print out the selection parameters.
	if (verbose == YES) {
	    call printf ("\nSelection Parameters\n")
	    call printf ( "    Maglo: %0.3f  Maghi: %0.3f\n")
		call pargr (SF_MAGLO(sf))
		call pargr (SF_MAGHI(sf))
	    call printf ( "    Roundlo: %0.3f  Roundhi: %0.3f\n")
		call pargr (SF_ROUNDLO(sf))
		call pargr (SF_ROUNDHI(sf))
	    call printf ( "    Sharplo: %0.3f  Sharphi: %0.3f\n")
		call pargr (SF_SHARPLO(sf))
		call pargr (SF_SHARPHI(sf))
	}

	if (mw != NULL) {
	    call mw_ctfree (ct)
	    call mw_close (mw)
	}
	call sfree (sp)
end


# SF_STFIND -- Detect images in the convolved image and then compute image
# characteristics using the original image.

int procedure sf_stfind (out, imbuf, denbuf, ncols, nlines, c1, c2, l1, l2,
	sepmin, skip, nxk, nyk, hwhmpsf, threshold, datamin, datamax,
	ct, nmin, maglo, maghi, roundlo, roundhi, sharplo, sharphi,
	fmtstr, stid, verbose)

int	out			#I the output file descriptor
real	imbuf[ncols,nlines]	#I the input data buffer
real	denbuf[ncols,nlines]	#I the input density enhancements buffer
int	ncols, nlines		#I the dimensions of the input buffers
int	c1, c2			#I the image columns limits
int	l1, l2			#I the image lines limits
int	sepmin			#I the minimum object separation
int	skip[nxk,ARB]		#I the pixel fitting array
int	nxk, nyk		#I the dimensions of the fitting array
real	hwhmpsf			#I the HWHM of the PSF in pixels
real	threshold		#I the threshold for object detection
real	datamin, datamax	#I the minimum and maximum good data values
pointer	ct			#I the coordinate transformation pointer
int	nmin			#I the minimum number of good object pixels
real	maglo,maghi		#I the magnitude estimate limits
real	roundlo,roundhi		#I the ellipticity estimate limits
real	sharplo, sharphi	#I the sharpness estimate limits
char	fmtstr[ARB]		#I the format string
int	stid			#U the object sequence number
int	verbose			#I verbose mode

int	line1, line2, inline, xmiddle, ymiddle, ntotal, nobjs, nstars
pointer	sp, cols, sharp, x, y, ellip, theta, npix, mag, size
int	sf_detect(), sf_test()

begin
	# Set up useful line and column limits.
	line1 = 1 + sepmin + nyk / 2
	line2 = nlines - sepmin - nyk / 2 
	xmiddle = 1 + nxk / 2
	ymiddle = 1 + nyk / 2

	# Set up a cylindrical buffers and some working space for
	# the detected images.
	call smark (sp)
	call salloc (cols, ncols, TY_INT)
	call salloc (x, ncols, TY_REAL)
	call salloc (y, ncols, TY_REAL)
	call salloc (mag, ncols, TY_REAL)
	call salloc (npix, ncols, TY_INT)
	call salloc (size, ncols, TY_REAL)
	call salloc (ellip, ncols, TY_REAL)
	call salloc (theta, ncols, TY_REAL)
	call salloc (sharp, ncols, TY_REAL)

	# Generate the starlist line by line.
	ntotal = 0
	do inline = line1, line2 {

	    # Detect local maximum in the density enhancement buffer.
	    nobjs = sf_detect (denbuf[1,inline-nyk/2-sepmin], ncols, sepmin,
	        nxk, nyk, threshold, Memi[cols])
	    if (nobjs <= 0)
		next

	    # Do not skip the middle pixel in the moments computation.
	    call sf_moments (imbuf[1,inline-nyk/2], denbuf[1,inline-nyk/2],
	        ncols, skip, nxk, nyk, Memi[cols], Memr[x], Memr[y],
		Memi[npix], Memr[mag], Memr[size], Memr[ellip], Memr[theta],
		Memr[sharp], nobjs, datamin, datamax, threshold, hwhmpsf,
		real (-sepmin - nxk / 2 + c1 - 1), real (inline - sepmin -
		nyk + l1 - 1))

	    # Test the image characeteristics of detected objects.
	    nstars = sf_test (Memi[cols], Memr[x], Memr[y], Memi[npix],
	        Memr[mag], Memr[size], Memr[ellip], Memr[theta], Memr[sharp],
		nobjs, real (c1 - 0.5), real (c2 + 0.5), real (l1 - 0.5),
		real (l2 + 0.5),  nmin, maglo, maghi, roundlo, roundhi,
		sharplo, sharphi)

	    # Print results on the standard output.
	    if (verbose == YES)
	        call sf_write (STDOUT, Memi[cols], Memr[x], Memr[y],
		    Memr[mag], Memi[npix], Memr[size], Memr[ellip],
		    Memr[theta], Memr[sharp], nstars, ct, fmtstr,
		    ntotal + stid)

	    # Save the results in the file.
	    call sf_write (out, Memi[cols], Memr[x], Memr[y], Memr[mag],
	        Memi[npix], Memr[size], Memr[ellip], Memr[theta],
		Memr[sharp], nstars, ct, fmtstr, ntotal + stid)

	    ntotal = ntotal + nstars

	}

	# Free space
	call sfree (sp)

	return (ntotal)
end


# SF_DETECT -- Detect stellar objects in an image line. In order to be
# detected as a star the candidate object must be above threshold and have
# a maximum pixel value greater than any pixels within sepmin pixels.

int procedure sf_detect (density, ncols, sepmin, nxk, nyk, threshold, cols)

real	density[ncols, ARB]	#I the input density enhancements array
int	ncols			#I the x dimension of the input array
int	sepmin			#I the minimum separation in pixels
int	nxk, nyk		#I size of the fitting area
real	threshold		#I density threshold
int	cols[ARB]		#O column numbers of detected stars

int	i, j, k, ymiddle, nxhalf, nyhalf, ny, b2, nobjs, rj2, r2
define	nextpix_	11

begin
	ymiddle = 1 + nyk / 2 + sepmin
	nxhalf = nxk / 2
	nyhalf = nyk / 2
	ny = 2 * sepmin + 1
	b2 = sepmin ** 2

	# Loop over all the columns in an image line.
	nobjs = 0
	for (i = 1 + nxhalf + sepmin; i <= ncols - nxhalf - sepmin; ) {

	    # Test whether the density enhancement is above threshold.
	    if (density[i,ymiddle] < threshold)
		goto nextpix_

	    # Test whether a given density enhancement satisfies the
	    # separation criterion.
	    do j = 1, ny {
		rj2 = (j - sepmin - 1) ** 2
		do k = i - sepmin, i + sepmin {
		    r2 = (i - k) ** 2 + rj2
		    if (r2 <= b2) {
		        if (density[i,ymiddle] < density[k,j+nyhalf])
		           goto nextpix_
		    }
		}
	    }

	    # Add the detected object to the list.
	    nobjs = nobjs + 1
	    cols[nobjs] = i

	    # If a local maximum is detected there can be no need to
	    # check pixels in this row between i and i + sepmin.
	    i = i + sepmin
nextpix_
	    # Work on the next pixel.
	    i = i + 1
	}

	return (nobjs)
end


# SF_MOMENTS -- Perform a moments analysis on the dectected objects.

procedure sf_moments (data, den, ncols, skip, nxk, nyk, cols, x, y,
	npix, mag, size, ellip, theta, sharp, nobjs, datamin, datamax,
	threshold, hwhmpsf, xoff, yoff)

real	data[ncols,ARB]		#I the input data array
real	den[ncols,ARB]		#I the input density enhancements array
int	ncols			#I the x dimension of the input buffer
int	skip[nxk,ARB]		#I the input fitting array
int	nxk, nyk		#I the dimensions of the fitting array
int	cols[ARB]		#I the input initial positions	
real	x[ARB]			#O the output x coordinates
real	y[ARB]			#O the output y coordinates
int	npix[ARB]		#O the output area in number of pixels
real	mag[ARB]		#O the output magnitude estimates
real	size[ARB]		#O the output size estimates
real	ellip[ARB]		#O the output ellipticity estimates
real	theta[ARB]		#O the output position angle estimates
real	sharp[ARB]		#O the output sharpness estimates
int	nobjs			#I the number of objects
real	datamin, datamax	#I the minium and maximum good data values
real	threshold		#I threshold for moments computation
real	hwhmpsf			#I the HWHM of the PSF
real	xoff, yoff		#I the x and y coordinate offsets

int	i, j, k, xmiddle, ymiddle, sumn
double	pixval, sumix, sumiy, sumi, sumixx, sumixy, sumiyy, r2, dx, dy, diff
double	mean

begin
	# Initialize
	xmiddle = 1 + nxk / 2
	ymiddle = 1 + nyk / 2 

	# Compute the pixel sum, number of pixels, and the x and y centers.
	do i = 1, nobjs {

	    # Estimate the background using the input data and the
	    # best fitting Gaussian amplitude
	    sumn = 0
	    sumi = 0.0
	    do j = 1, nyk {
		do k = 1, nxk {
		    if (skip[k,j] == NO)
			next
		    pixval = data[cols[i]-xmiddle+k,j]
		    if (pixval < datamin || pixval > datamax)
			next
		    sumi = sumi + pixval
		    sumn = sumn + 1
		}
	    }
	    if (sumn <= 0)
	        mean = data[cols[i],ymiddle] - den[cols[i],ymiddle]
	    else
		mean = sumi / sumn

	    # Compute the first order moments.
	    sumi = 0.0
	    sumn = 0
	    sumix = 0.0d0
	    sumiy = 0.0d0
	    do j = 1, nyk {
		do k = 1, nxk {
		    if (skip[k,j] == YES)
			next
		    pixval = data[cols[i]-xmiddle+k,j]
		    if (pixval < datamin || pixval > datamax)
			next
		    pixval = pixval - mean
		    if (pixval <= 0.0)
			next
		    sumi = sumi + pixval
		    sumix = sumix + (cols[i] - xmiddle + k) * pixval
		    sumiy = sumiy + j * pixval
		    sumn = sumn + 1
		}

	    }

	    # Use the first order moments to estimate the positions
	    # magnitude, area, and amplitude of the object.
	    if (sumi <= 0.0) {
		x[i] = cols[i] 
		y[i] = (1.0 + nyk) / 2.0 
		mag[i] = INDEFR
		npix[i] = 0
	    } else {
		x[i] = sumix / sumi 
		y[i] = sumiy / sumi 
		mag[i] = -2.5 * log10 (sumi)
		npix[i] = sumn
	    }

	    # Compute the second order central moments using the results of
	    # the first order moment analysis.
	    sumixx = 0.0d0
	    sumiyy = 0.0d0
	    sumixy = 0.0d0
	    do j = 1, nyk {
		dy = j - y[i]
		do k = 1, nxk {
		    if (skip[k,j] == YES)
			next
		    pixval = data[cols[i]-xmiddle+k,j]
		    if (pixval < datamin || pixval > datamax)
			next
		    pixval = pixval - mean
		    if (pixval <= 0.0)
			next
		    dx = cols[i] - xmiddle + k - x[i]
		    sumixx = sumixx + pixval * dx ** 2
		    sumixy = sumixy + pixval * dx * dy
		    sumiyy = sumiyy + pixval * dy ** 2
		}
	    }

	    # Use the second order central moments to estimate the size,
	    # ellipticity, position angle, and sharpness of the objects.
	    if (sumi <= 0.0) {
		size[i] = 0.0
		ellip[i] = 0.0
		theta[i] = 0.0
		sharp[i] = INDEFR
	    } else {
		sumixx = sumixx / sumi
		sumixy = sumixy / sumi
		sumiyy = sumiyy / sumi
		r2 = sumixx + sumiyy
		if (r2 <= 0.0) {
		    size[i] = 0.0
		    ellip[i] = 0.0
		    theta[i] = 0.0
		    sharp[i] = INDEFR
		} else {
		    size[i] = sqrt (LN_2 * r2)
		    sharp[i] = size[i] / hwhmpsf
		    diff = sumixx - sumiyy
		    ellip[i] = sqrt (diff ** 2 + 4.0d0 * sumixy ** 2) / r2
		    if (diff == 0.0d0 && sumixy == 0.0d0)
			theta[i] = 0.0
		    else
		        theta[i] = RADTODEG (0.5d0 * atan2 (2.0d0 * sumixy,
			    diff))
		    if (theta[i] < 0.0)
			theta[i] = theta[i] + 180.0
		}
	    }

	    # Convert the computed coordinates to the image system.
	    x[i] = x[i] + xoff
	    y[i] = y[i] + yoff
	}
end


# SF_TEST -- Check that the detected objects are in the image, contain
# enough pixels above background to be measurable objects, and are within
# the specified magnitude, roundness and sharpness range.

int procedure sf_test (cols, x, y, npix, mag, size, ellip, theta, sharps,
	nobjs, c1, c2, l1, l2, nmin, maglo, maghi, roundlo, roundhi,
	sharplo, sharphi)

int	cols[ARB]			#U the column ids of detected object
real	x[ARB]				#U the x position estimates
real	y[ARB]				#U the y positions estimates
int	npix[ARB]			#U the area estimates
real	mag[ARB]			#U the magnitude estimates
real	size[ARB]			#U the size estimates
real	ellip[ARB]			#U the ellipticity estimates
real	theta[ARB]			#U the position angle estimates
real	sharps[ARB]			#U sharpness estimates
int	nobjs				#I the number of detected objects
real	c1, c2				#I the image column limits
real	l1, l2				#I the image line limits
int	nmin				#I the minimum area
real	maglo, maghi			#I the magnitude limits
real	roundlo, roundhi		#I the roundness limits
real	sharplo, sharphi		#I the sharpness limits

int	i, nstars

begin
	# Loop over the detected objects.
	nstars = 0
	do i = 1, nobjs {

	    if (x[i] < c1 || x[i] > c2)
		next
	    if (y[i] < l1 || y[i] > l2)
		next
	    if (npix[i] < nmin)
		next
	    if (mag[i] < maglo || mag[i] > maghi)
		next
	    if (ellip[i] < roundlo || ellip[i] > roundhi)
		next
	    if (! IS_INDEFR(sharps[i]) && (sharps[i] < sharplo ||
	        sharps[i] > sharphi))
		next

	    # Add object to the list.
	    nstars = nstars + 1
	    cols[nstars] = cols[i]
	    x[nstars] = x[i]
	    y[nstars] = y[i]
	    mag[nstars] = mag[i]
	    npix[nstars] = npix[i]
	    size[nstars] = size[i]
	    ellip[nstars] = ellip[i]
	    theta[nstars] = theta[i]
	    sharps[nstars] = sharps[i]
	}

	return (nstars)
end


# SF_WRITE -- Write the results to the output file.

procedure sf_write (fd, cols, x, y, mag, npix, size, ellip, theta, sharp,
	nstars, ct, fmtstr, stid)

int     fd                              #I the output file descriptor
int     cols[ARB]                       #I column numbers
real    x[ARB]                          #I xcoords
real    y[ARB]                          #I y coords
real    mag[ARB]                        #I magnitudes
int     npix[ARB]                       #I number of pixels
real    size[ARB]                       #I object sizes
real    ellip[ARB]                      #I ellipticities
real    theta[ARB]                      #I position angles
real    sharp[ARB]                      #I sharpnesses
int     nstars                          #I number of detected stars in the line
pointer	ct				#I coordinate transformation
char	fmtstr[ARB]			#I the output format string
int     stid                            #I output file sequence number

double	lx, ly, wx, wy
int     i

begin
        if (fd == NULL)
            return

        do i = 1, nstars {
            call fprintf (fd, fmtstr)
                call pargr (x[i])
                call pargr (y[i])
		if (ct != NULL) {
		    lx = x[i]
		    ly = y[i]
		    call mw_c2trand (ct, lx, ly, wx, wy)
		    call pargd (wx)
		    call pargd (wy)
		}
                call pargr (mag[i])
		call pargi (npix[i])
                call pargr (size[i])
                call pargr (ellip[i])
                call pargr (theta[i])
		call pargr (sharp[i])
                call pargi (stid + i - 1)
        }
end
