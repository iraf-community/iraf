include <gset.h>
include "daoedit.h"

define	FRACTION 0.10

# DP_ERPLOT -- Plot the radial profile.

procedure dp_erplot (gd, title, xwcs, ywcs, radius, intensity, npts,
	rcentroid, pmean, integral, nbins, rmin, rmax, iannulus, oannulus,
	apradius, skyval, skysigma, rscale, pnorm)

pointer	gd			# pointer to the graphics stream
char	title[ARB]		# the plot title
int	xwcs			# the x wcs type of the final plot
int	ywcs			# the y wcs type of the final plot
real	radius[ARB]		# the radius vector
real	intensity[ARB]		# the intensity vector
int	npts			# number of points in the profile
real	rcentroid[ARB]		# the radius centroid vector
real	pmean[ARB]		# the mean intensity vector
real	integral[ARB]		# the integral of the profile
int	nbins			# the number of bins
real	rmin, rmax		# min and max radius
real	iannulus		# the inner radius of the sky annulus
real	oannulus		# the outer radius of the sky annulus
real	apradius		# the aperture radius
real	skyval			# the sky value
real	skysigma		# the sigma of the sky value
real	rscale			# the image scale
real	pnorm			# the profile normalization factor

int	i, j
pointer	sp, pxlabel, pylabel, sxlabel, sylabel
real	r1, r2, rp1, rp2, rs1, rs2, i1, i2, ip1, ip2, is1, is2, dr, fraction

begin
	# Get space for the x and y labels.
	call smark (sp)
	call salloc (pxlabel, SZ_FNAME, TY_CHAR)
	call salloc (pylabel, SZ_FNAME, TY_CHAR)
	call salloc (sxlabel, SZ_FNAME, TY_CHAR)
	call salloc (sylabel, SZ_FNAME, TY_CHAR)

	# Clear the plot.
	call gclear (gd)

	# Determine the data range of the x and y axes.

	r1 = rmin - FRACTION * (rmax - rmin)
	r2 = rmax + FRACTION * (rmax - rmin)
	switch (xwcs) {
	case WCS_XPIX:
	    rp1 = r1
	    rp2 = r2
	    rs1 = r1 / rscale
	    rs2 = r2 / rscale
	case WCS_XSCALE:
	    rp1 = r1 / rscale
	    rp2 = r2 / rscale
	    rs1 = r1
	    rs2 = r2
	default:
	    rp1 = r1
	    rp2 = r2
	    rs1 = r1 / rscale
	    rs2 = r2 / rscale
	}
	fraction = max (FRACTION, 5.0 * skysigma / pnorm)
	i1 = -pnorm * fraction
	i2 = pnorm  * (1.0 + fraction)
	switch (ywcs) {
	case WCS_YNORM:
	    ip1 = i1 / pnorm
	    ip2 = i2 / pnorm
	    is1 = i1 + skyval
	    is2 = i2 + skyval
	case WCS_YCOUNT:
	    ip1 = i1 + skyval
	    ip2 = i2 + skyval
	    is1 = i1 / pnorm
	    is2 = i2 / pnorm
	default:
	    ip1 = i1 / pnorm
	    ip2 = i2 / pnorm
	    is1 = i1 + skyval
	    is2 = i2 + skyval
	}

	# Draw the axes and axis labels.
	call gsetr (gd, G_ASPECT, 0.0)
	switch (xwcs) {
	case WCS_XPIX:
	    call strcpy ("Radius (b=pixels, t=scale units)", Memc[pxlabel],
	        SZ_FNAME)
	    call strcpy ("", Memc[sxlabel], SZ_FNAME)
	case WCS_XSCALE:
	    call strcpy ("Radius (b=scale units, t=pixels)", Memc[pxlabel],
		SZ_FNAME)
	    call strcpy ("", Memc[sxlabel], SZ_FNAME)
	default:
	    call strcpy ("Radius (b=pixels, t=scale units)", Memc[pxlabel],
	        SZ_FNAME)
	    call strcpy ("", Memc[sxlabel], SZ_FNAME)
	}
	switch (ywcs) {
	case WCS_YCOUNT:
	    call strcpy ("Counts", Memc[pylabel], SZ_FNAME)
	    call strcpy ("Norm Counts", Memc[sylabel], SZ_FNAME)
	case WCS_YNORM:
	    call strcpy ("Norm Counts", Memc[pylabel], SZ_FNAME)
	    call strcpy ("Counts", Memc[sylabel], SZ_FNAME)
	default:
	    call strcpy ("Counts", Memc[pylabel], SZ_FNAME)
	    call strcpy ("Norm Counts", Memc[sylabel], SZ_FNAME)
	}

	call gseti (gd, G_XDRAWAXES, 1)
	call gseti (gd, G_YDRAWAXES, 1)
	call gswind (gd, rp1, rp2, ip1, ip2)
	call glabax (gd, title, Memc[pxlabel], Memc[pylabel])
	call gseti (gd, G_XDRAWAXES, 2)
	call gseti (gd, G_YDRAWAXES, 2)
	call gswind (gd, rs1, rs2, is1, is2)
	call glabax (gd, title, Memc[sxlabel], Memc[sylabel])

	# Plot the data points.
	call gswind (gd, r1, r2, i1, i2)
	call gpmark (gd, radius, intensity, npts, GM_PLUS, 1.0, 1.0)

	# Plot the smoothed radial profile skipping any points with no data.
	call gswind (gd, r1, r2, -fraction, 1.0 + fraction)
	for (i = 1; i <= nbins && rcentroid[i] <= 0.0; i = i + 1)
	    ;
	call gamove (gd, rcentroid[i], pmean[i])
	do j = i, nbins {
	    if (pmean[j] == 0.0)
		next
	    call gadraw (gd, rcentroid[j], pmean[j])
	}

	# Plot the integral.
	call gswind (gd, r1, r2, -fraction, 1.0 + fraction)
	call gamove (gd, 0.0, 0.0)
	dr = (rmax - rmin) / real (nbins - 1)
	do j = 2, nbins {
	    call gadraw (gd, real (j - 1) * dr, integral[j])
	}

	# Plot the sky annuli and the aperture radius.
	call gamove (gd, iannulus, -fraction)
	call gadraw (gd, iannulus, 1.0 + fraction)
	call gamove (gd, oannulus, -fraction)
	call gadraw (gd, oannulus, 1.0 + fraction)
	call gamove (gd, apradius, -fraction)
	call gadraw (gd, apradius, 1.0 + fraction)

	# Plot the zero level.
	call gswind (gd, rp1, rp2, ip1, ip2)
	switch (ywcs) {
	case WCS_YCOUNT:
	    call gamove (gd, rp1, skyval)
	    call gadraw (gd, rp2, skyval)
	case WCS_YNORM:
	    call gamove (gd, rp1, 0.0)
	    call gadraw (gd, rp2, 0.0)
	default:
	    call gamove (gd, rp1, 0.0)
	    call gadraw (gd, rp2, 0.0)
	}

	call sfree (sp)
end
