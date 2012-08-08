include <mach.h>
include <imhdr.h>
include "daoedit.h"


# DP_ERPROFILE -- Compute and optionally plot the radial profile of the
# selected star.

procedure dp_erprofile (gd, id, banner, xwcs, ywcs, im, xinit, yinit)

pointer	gd		# pointer to the graphics descriptor
pointer	id		# pointer to the display descriptor
int	banner		# print banner for photometry results
int	xwcs		# the x wcs type
int	ywcs		# the y wcs type
pointer	im		# pointer to the input image
real	xinit		# initial x position
real	yinit		# initial y position

int	cboxsize, rboxsize, npts, naperts, nbins, nr
pointer	sp, radius, intensity, apstr, aperts, rcentroid, pmean, integral, title
real	scale, pradius, iannulus, oannulus, mean, median, sigma, pnorm, inorm
real	fwhmpsf, xcenter, ycenter, apradius, counts
real	clgetr(), dp_aprcounts()
int	dp_rprofile(), strlen(), dp_gaperts(), btoi()
bool	clgetb()

begin
	# Return if the initial x and y centers are undefined.
	if (IS_INDEFR(xinit) || IS_INDEFR(yinit))
	    return

	# Get the scale of the image from the datapars pset.
	scale = clgetr ("datapars.scale")
	if (scale <= 0.0)
	    scale = 1.0
	else
	    scale = 1.0 / scale

	# Estimate the center of the star using the datapars parameter
	# scale and the centerpars parameter cbox and an initial center.
	# Force the centering box to be an odd number of pixels.
	cboxsize = scale * clgetr ("centerpars.cbox") + 1.0
	if (mod (cboxsize, 2) == 0)
	    cboxsize = cboxsize + 1
	call dp_rcenter (im, xinit, yinit, cboxsize, xcenter, ycenter)

	# Allocate working memory for the radial profile data.
	pradius = scale * (clgetr ("fitskypars.annulus") +
	    clgetr ("fitskypars.dannulus") + 1.0)
	rboxsize = 2 * nint (pradius + 1.0) + 1
	call smark (sp)
	call salloc (radius, rboxsize * rboxsize, TY_REAL)
	call salloc (intensity, rboxsize * rboxsize, TY_REAL)

	# Compute the radial profile.
	npts = dp_rprofile (im, xcenter, ycenter, rboxsize, pradius,
	    Memr[radius], Memr[intensity])
	if (npts <= 0) {
	    call printf ("No data for computing radial profile\n")
	    call sfree (sp)
	    return
	}

	# Estimate the sky level and subtract it from the profile.
	iannulus = scale * clgetr ("fitskypars.annulus")
	oannulus = iannulus + scale * clgetr ("fitskypars.dannulus")
	call dp_rskyval (Memr[radius], Memr[intensity], npts,
	    iannulus, oannulus, mean, median, sigma)
	call asubkr (Memr[intensity], median, Memr[intensity], npts)

	# Get the last photometry aperture and sum up the pixels inside it.
	call salloc (apstr, SZ_LINE, TY_CHAR)
	call salloc (aperts, MAX_NAPERTS, TY_REAL)
	call clgstr ("photpars.apertures", Memc[apstr], SZ_LINE)
	naperts = dp_gaperts (Memc[apstr], Memr[aperts], MAX_NAPERTS)
	apradius = scale * Memr[aperts+naperts-1]
	counts = dp_aprcounts (Memr[radius], Memr[intensity], npts, apradius)

	# Compute the normalized mean intensity as a function of radius
	# and the normalized integral of the intensity as a function of
	# radius.
	nbins = int (pradius / 0.5 + 0.5)
	call salloc (nr, nbins, TY_INT)
	call salloc (rcentroid, nbins, TY_REAL)
	call salloc (pmean, nbins, TY_REAL)
	call salloc (integral, nbins, TY_REAL)
	call dp_crprofile (Memr[radius], Memr[intensity], npts, Memi[nr],
	    Memr[rcentroid], Memr[pmean], Memr[integral], nbins, 0.0,
	    pradius, real ((cboxsize-1)/ 2), iannulus, fwhmpsf, pnorm, inorm)

	# Mark the objects on the display.
	if (id != NULL) {
	    call dp_eomark (id, xcenter, ycenter, iannulus, oannulus,
	        apradius, btoi (clgetb ("centerpars.mkcenter")),
		btoi (clgetb ("fitskypars.mksky")),
		btoi (clgetb ("photpars.mkapert")))
	    if (gd == id)
		call gflush (gd)
	    else
		call gframe (id)
	}
	# Convert the center coordinates if appropriate.
	call dp_ltov (im, xcenter, ycenter, xcenter, ycenter, 1)

	# Draw the plot.
	if (gd != NULL) {
	    call salloc (title, 3 * SZ_LINE, TY_CHAR)
	    call sprintf (Memc[title], 3 * SZ_LINE,
	        "%s: Radial profile at %0.2f %0.2f\n")
	        call pargstr (IM_HDRFILE(im))
	        call pargr (xcenter)
	        call pargr (ycenter)
	    call sprintf (Memc[title+strlen(Memc[title])], 3 * SZ_LINE, 
		"Sky: mean %g median %g sigma %g\n")
		call pargr (mean)
	        call pargr (median)
		call pargr (sigma)
	    call sprintf (Memc[title+strlen(Memc[title])], 3 * SZ_LINE, 
	       "Fwhmpsf: %0.2f Counts: %g Mag: %0.3f\n\n")
	        call pargr (fwhmpsf)
		call pargr (counts)
		if (counts <= 0.0)
		    call pargr (INDEFR)
		else
		    call pargr (-2.5 * log10 (counts))
	    call dp_erplot (gd, Memc[title], xwcs, ywcs, Memr[radius],
	        Memr[intensity], npts, Memr[rcentroid], Memr[pmean],
		Memr[integral], nbins, 0.0, pradius, iannulus, oannulus,
		apradius, median, sigma, scale, pnorm)
	}

	# Print the results.
	if (gd == NULL) {
	    if (banner == YES)
	        call dp_bprint (STDOUT)
	    call dp_aprint (STDOUT, xcenter, ycenter, median, sigma, fwhmpsf,
	        counts)
	}

	# Free memory.
	call sfree (sp)
end


# DP_RCENTER -- Compute the star center using a simple 1D centroiding
# algorithm on the x and y marginals, after thresholding at the mean. 

procedure dp_rcenter (im, xstart, ystart, boxsize, xcntr, ycntr)

pointer	im			# pointer to the input image
real	xstart, ystart		# starting coordinates
int	boxsize			# width of the centering box
real	xcntr, ycntr		# centered coordinates

int	half_box, x1, x2, y1, y2, ncols, nrows, nx, ny, try
pointer	bufptr, sp, x_vect, y_vect
real	xinit, yinit
pointer	imgs2r()

begin
	# Initialize.
	half_box = (boxsize - 1) / 2
	xinit = xstart
	yinit = ystart
	ncols = IM_LEN (im,1)
	nrows = IM_LEN (im,2)

	try = 0
	repeat {

	    # Compute the parameters of the extraction region.
	    x1 = max (xinit - half_box, 1.0) + 0.5
	    x2 = min (xinit + half_box, real (ncols)) + 0.5
	    y1 = max (yinit - half_box, 1.0) + 0.5
	    y2 = min (yinit + half_box, real (nrows)) + 0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # Get the data.
	    bufptr = imgs2r (im, x1, x2, y1, y2)

	    # Allocate space for the marginals.
	    call smark (sp)
	    call salloc (x_vect, nx, TY_REAL)
	    call salloc (y_vect, ny, TY_REAL)

	    # Compute the marginals.
	    call aclrr (Memr[x_vect], nx)
	    call aclrr (Memr[y_vect], ny)
	    call dp_rowsum (Memr[bufptr], Memr[x_vect], nx, ny)
	    call dp_colsum (Memr[bufptr], Memr[y_vect], nx, ny)

	    # Compute the centers.
	    call dp_vcentroid (Memr[x_vect], nx, xcntr)
	    call dp_vcentroid (Memr[y_vect], ny, ycntr)

	    # Add in offsets to image coordinate system.
	    xcntr = xcntr + x1
	    ycntr = ycntr + y1

	    call sfree (sp)

	    # If the shifts are greater than 1 pixel in either direction
	    # do 1 more iteration.
	    try = try + 1
	    if (try == 1) {
		if ((abs (xcntr - xinit) > 1.0) || (abs (ycntr - yinit) >
		    1.0)) {
		    xinit = xcntr
		    yinit = ycntr
		}
	    } else
		break
	}
end


# DP_RPROFILE -- Get the data and compute the radius and intensity vectors.

int procedure dp_rprofile (im, xcntr, ycntr, rboxsize, pradius, radius,
	intensity)

pointer	im			# pointer to the input image
real	xcntr, ycntr		# the center of the extraction box
int	rboxsize		# the width of the extraction box
real	pradius			# the plotting radius
real	radius[ARB]		# the output radius vector
real	intensity[ARB]		# the output intensity vector

int	half_box, ncols, nrows, x1, x2, y1, y2, nx, ny, npts
pointer	bufptr	
real	xinit, yinit
int	dp_rivectors()
pointer	imgs2r()

begin
	# Initialize.
	half_box = (rboxsize - 1) / 2
	xinit = xcntr
	yinit = ycntr
	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)

	# Get the data.
	x1 = max (xinit - half_box, 1.0) + 0.5
	x2 = min (xinit + half_box, real (ncols)) + 0.5
	y1 = max (yinit - half_box, 1.0) + 0.5
	y2 = min (yinit + half_box, real (nrows)) + 0.5
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	bufptr = imgs2r (im, x1, x2, y1, y2)

	# Compute the radius and intensity vectors.
	npts = dp_rivectors (Memr[bufptr], nx, ny, x1, y1, xcntr, ycntr, 
	    pradius, radius, intensity) 

	return (npts)
end


# DP_RSKYVAL -- Compute the mean, median and sigma of the pixels in the
# sky annulus.

procedure dp_rskyval (radius, intensity, npts, iannulus, oannulus, mean,
	median, sigma)

real	radius[ARB]		# the output radius vector
real	intensity[ARB]		# the output intensity vector
int	npts			# number of points in the profile
real	iannulus		# inner radius of sky annulus
real	oannulus		# outer radius of sky annulus
real	mean			# mean sky value
real	median			# median sky value
real	sigma			# standard deviation of sky values

int	i, nsky, il, ih, ngood
pointer	skypix
real	slocut, shicut

begin
	call malloc (skypix, npts, TY_REAL)

	nsky = 0
	do i = 1, npts {
	    if (radius[i] < iannulus || radius[i] > oannulus)
		next
	    nsky = nsky + 1
	    Memr[skypix+nsky-1] = intensity[i]
	}
	call asrtr (Memr[skypix], Memr[skypix], nsky)

	if (nsky == 0) {
	    mean = 0.0
	    median = 0.0
	    sigma = 0.0
	} else {
	    call aavgr (Memr[skypix], nsky, mean, sigma)
	    if (mod (nsky, 2) == 0)
	        median = 0.5 * (Memr[skypix+(nsky+1)/2-1] +
		    Memr[skypix+(nsky+1)/2])
	    else
	        median = Memr[skypix+(nsky+1)/2-1]
	}

	# Detect pixels to be rejected.
	slocut = median - min (median - Memr[skypix], Memr[skypix+nsky-1] -
	    median, 3.0 * sigma)
	shicut = median + min (median - Memr[skypix], Memr[skypix+nsky-1] -
	    median, 3.0 * sigma)
	for (il = 1; il <= nsky; il = il + 1) {
	    if (Memr[skypix+il-1] >= slocut)
		break
	}
	for (ih = nsky; ih >= 1; ih = ih - 1) {
	    if (Memr[skypix+ih-1] <= shicut)
		break
	}
	ngood = ih - il + 1
	if (ngood < nsky) {
	    if (ngood == 0) {
	        mean = 0.0
	        median = 0.0
	        sigma = 0.0
	    } else {
	        call aavgr (Memr[skypix+il-1], ngood, mean, sigma)
	        if (mod (ngood, 2) == 0)
	            median = 0.5 * (Memr[skypix+il-1+(ngood+1)/2-1] +
		        Memr[skypix+il-1+(ngood+1)/2])
	        else
	            median = Memr[skypix+il-1+(ngood+1)/2-1]
	    }
	}


	call mfree (skypix, TY_REAL)
end


# DP_APRCOUNTS -- Sumup the counts inside the aperture.

real procedure dp_aprcounts (radius, intensity, npts, apradius)

real	radius[ARB]		# the output radius vector
real	intensity[ARB]		# the output intensity vector
int	npts			# number of points in the profile
real	apradius		# the aperture radius

int	i
real	counts

begin
	counts = 0.0
	do i = 1, npts {
	    if (radius[i] > apradius)
		next
	    counts = counts + intensity[i]
	}
	return (counts)
end


# DP_CRPROFILE -- Compute the smoothed radial profile and its integral at half
# pixel intervals.

procedure dp_crprofile (radius, intensity, npts, nr, rcentroid, pmean,
	integral, nbins, rmin, rmax, prad, irad, fwhmpsf, pnorm, inorm)

real	radius[ARB]		# the output radius vector
real	intensity[ARB]		# the output intensity vector
int	npts			# number of points in the profile
int	nr[ARB]			# the number of points in each interval
real	rcentroid[ARB]		# the centroid of the radius values
real	pmean[ARB]		# the mean of the intensity values
real	integral[ARB]		# the integral of the curve
int	nbins			# the number of radius bins
real	rmin, rmax		# the radius min and max values
real	prad			# the normalization radius for the profile
real	irad			# the normalization radius for the integral
real	fwhmpsf			# computed full width halfmax psf
real	pnorm			# the maximum profile value
real	inorm			# the maximum count value

int	i, bin
real	dr, r

begin
	# Initialize the arrays.
	call aclri (nr, nbins)
	call aclrr (rcentroid, nbins)
	call aclrr (pmean, nbins)
	call aclrr (integral, nbins)

	# Accumulate the data.
	dr = real (nbins - 1) / real (rmax - rmin)
	do i = 1, npts {
	    r = radius[i]
	    if (r < rmin || r > rmax)
		next
	    bin = int ((r - rmin) * dr) + 1
	    nr[bin] = nr[bin] + 1
	    pmean[bin] = pmean[bin] + intensity[i]
	    rcentroid[bin] = rcentroid[bin] + r
	}

	# Compute the integral of the radial profile and normalize it
	# to 1.0 at the radius irad.
	do i = 2, nbins 
	    integral[i] = integral[i-1] + pmean[i-1]
	bin = int ((irad - rmin) * dr) + 1
	inorm = integral[min (bin, nbins)]
	call adivkr (integral, inorm, integral, nbins)

	# Compute the smoothed radial profile and normalize to the
	# maximum data point inside the radius prad.
	pnorm = -MAX_REAL
	do i = 1, npts {
	    if (radius[i] > prad)
		next
	    if (intensity[i] > pnorm)
		pnorm = intensity[i]
	}
	do i = 1, nbins {
	    if (nr[i] <= 0) {
		rcentroid[i] = (i - 1.0 + 0.5) / dr
		if (rcentroid[i] < rmin || rcentroid[i] > rmax)
		    pmean[i] = 0.0
		else
		    pmean[i] = pnorm
	    } else {
		rcentroid[i] = rcentroid[i] / nr[i]
	        pmean[i] = pmean[i] / nr[i]
	    }
	}
	call adivkr (pmean, pnorm, pmean, nbins)

	# Estimate the full width half max of the psf in pixels.
	do i = 1, nbins {
	    if (pmean[i] == 0.0)
		next
	    if (pmean[i] < 0.5)
		break
	}
	if (i == 1)
	    fwhmpsf = 2.0 * rcentroid[1]
	else if (i == nbins && pmean[nbins] >= 0.5)
	    fwhmpsf = 2.0 * rcentroid[nbins]
	else if (pmean[i-1] == pmean[i])
	    fwhmpsf = rcentroid[i-1] + rcentroid[i]
	else
	    fwhmpsf = 2.0 * ((rcentroid[i] * (0.5 - pmean[i-1]) +
	        rcentroid[i-1] * (pmean[i] - 0.5)) / (pmean[i] - pmean[i-1]))
end


# DP_ROWSUM -- Sum all the rows in a raster.

procedure dp_rowsum (raster, row, nx, ny)

real	raster[nx,ny]		# the input subraster
real	row[ARB]		# the output summed row
int	nx, ny			# the dimensions of the input subraster

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		row[j] = row[j] + raster[j,i]
end


# DP_COLSUM -- Sum all the columns in a raster.

procedure dp_colsum (raster, col, nx, ny)

real	raster[nx,ny]		# the input subraster
real	col[ARB]		# the output summed column
int	nx, ny			# the dimensions of the input subraster

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		col[j] = col[j] + raster[i,j]
end


# DP_VCENTROID -- Compute the centroid of a vector.

procedure dp_vcentroid (vector, nv, vc)

real	vector[ARB]		# the input vector
int	nv			# length of the input array
real	vc			# the output centroid

int	i
real	sum1, sum2, sigma, cont

begin
	sum1 = 0.0
	sum2 = 0.0

	call aavgr (vector, nv, cont, sigma)
	do i = 1, nv
	    if (vector[i] > cont) {
	        sum1 = sum1 + (i - 1) * (vector[i] - cont)
	        sum2 = sum2 + (vector[i] - cont)
	    }

	vc = sum1 / sum2
end


# DP_RIVECTORS -- Compute the radius and intensity vectors.

int procedure dp_rivectors (a, nx, ny, x1, y1, xcntr, ycntr, pradius,
	radius, intensity)
								
real	a[nx,ny]		# the input data array
int	nx, ny			# dimensions of the input array
int	x1, y1			# lower left corner of input array
real	xcntr, ycntr		# coordinates of center pixel
real	pradius			# the plotting radius
real	radius[ARB]		# the output radius vector
real	intensity[ARB]		# the output intensity vector

int	i, j, npts
real	pr2, r2, dy2

begin
	pr2 = pradius * pradius

	npts = 0
	do i = 1, ny {
	    dy2 = (ycntr - y1 + 1 - i) ** 2 
	    do j = 1, nx {
		r2 = (xcntr - x1 + 1 - j) ** 2 + dy2 
		if (r2 > pr2)
		    next
		npts = npts + 1
		radius[npts] = sqrt (r2)
		intensity[npts] = a[j,i]
	    }
        }

	return (npts)
end


# DP_BPRINT -- Print the photometry banner.

procedure dp_bprint (fd)

int	fd		# output file descriptor

string	banner "# XCENTER YCENTER       SKY SKYSIGMA    FWHM   COUNTS     MAG"

begin
	call fprintf (fd, "\n%s\n")
	    call pargstr (banner)
end


# DP_APRINT -- Print the photometry results.

procedure dp_aprint (fd, xcenter, ycenter, skyval, sigma, fwhmpsf, inorm)

int	fd		# output file descriptor
real	xcenter		# x coordinate of profile
real	ycenter		# y coordinate of profile
real	skyval		# the sky value
real	sigma		# the standard deviation of the sky pixels
real	fwhmpsf		# the estimated fwhmpsf
real	inorm		# the total counts

begin
	call fprintf (fd, "  %7.2f %7.2f  %8.1f %8.2f  %6.2f %8.1f %7.3f\n")
	    call pargr (xcenter)
	    call pargr (ycenter)
	    call pargr (skyval)
	    call pargr (sigma)
	    call pargr (fwhmpsf)
	    call pargr (inorm)
	    if (inorm <= 0.0)
		call pargr (INDEFR)
	    else
	        call pargr (-2.5 * log10 (inorm))
end
