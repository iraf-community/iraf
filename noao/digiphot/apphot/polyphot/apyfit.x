include <imhdr.h>
include <mach.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/center.h"
include "../lib/polyphot.h"

# AP_YFIT -- Procedure to compute the magnitude of an object inside a polygonal
# aperture.

int procedure ap_yfit (py, im, xver, yver, nver, skyval, skysig, nsky)

pointer	py		# pointer to polyphot strucuture
pointer	im		# pointer to IRAF image
real	xver[ARB]	# x vertices coords
real	yver[ARB]	# y vertices coords
int	nver		# number of vertices
real	skyval		# sky value
real	skysig		# sigma of sky pixels
int	nsky		# number of sky pixels

double	flux
int	noise, badpix, ier
real	datamin, datamax, mag, area, magerr, zmag, padu, itime, readnoise
int	apstati(), ap_yyfit(), ap_byyfit()
real	apstatr()

begin
	# Initialize.
	call apsetr (py, PYFLUX, 0.0)
	call apsetr (py, PYNPIX, 0.0)
	call apsetr (py, PYMAG, INDEFR)
	call apsetr (py, PYMAGERR, INDEFR)

	# Compute the flux inside the polygon.
	if (IS_INDEFR(apstatr (py, DATAMIN)) && IS_INDEFR(apstatr(py,
	    DATAMAX))) {
	    ier = ap_yyfit (im, xver, yver, nver, flux, area)
	    badpix = NO
	} else {
	    if (IS_INDEFR(apstatr (py, DATAMIN)))
		datamin = -MAX_REAL
	    else
		datamin = apstatr (py, DATAMIN)
	    if (IS_INDEFR(apstatr (py, DATAMAX)))
		datamax = MAX_REAL
	    else
		datamax = apstatr (py, DATAMAX)
	    ier = ap_byyfit (im, xver, yver, nver, datamin, datamax, flux,
	        area, badpix)
	}
	if (ier == PY_NOPOLYGON)
	    return (PY_NOPOLYGON)

	# Store the results.
	call apsetr (py, PYFLUX, real (flux))
	call apsetr (py, PYNPIX, area)
	call apseti (py, PYBADPIX, badpix)
	if (IS_INDEFR(skyval))
	    return (PY_NOSKYMODE)

	# Get photometry parameters.
	zmag = apstatr (py, PYZMAG)
	itime = apstatr (py, ITIME)
	padu = apstatr (py, EPADU)
	noise = apstati (py, NOISEFUNCTION)
	readnoise = apstatr (py, READNOISE)

	# Compute the magnitude and error.
	if (apstati (py, POSITIVE) == YES)
	    call apcopmags (real (flux), area, mag, magerr, 1, skyval, skysig,
	        nsky, zmag, noise, padu)
	else
	    call apconmags (real (flux), area, mag, magerr, 1, skyval, skysig,
	        nsky, zmag, noise, padu, readnoise)

	# Store the magnitudes
	mag  = mag + 2.5 * log10 (itime)
	call apsetr (py, PYMAG, mag)
	call apsetr (py, PYMAGERR, magerr)

	return (ier)
end


# AP_YYFIT -- Procedure to measure the total flux inside a polygon.

int procedure ap_yyfit (im, xver, yver, nver, flux, area)

pointer	im		# pointer to IRAF image
real	xver[ARB]	# x coordinates of the vertices
real	yver[ARB]	# y coordinates of the vertices:
int	nver		# number of vertices
double	flux		# flux interior to the polygon
real	area		# approximate area of polygon

int	i, j, linemin, linemax, colmin, colmax, ncols, nintr, ier
pointer	sp, xintr, yintr, buf
real	xmin, xmax, ymin, ymax, x1, x2, lx, ld
real	xlow, xhigh, fctnx1, fctnx2, fctny
int	ap_yclip()
pointer	imgl2r()
real	asumr()

begin
	# Check that polygon has at least 3 vertices.
	if (nver < 3) {
	    flux = INDEFD
	    area = 0.0
	    return (PY_NOPOLYGON)
	}

	# Allocate the working space.
	call smark (sp)
	call salloc (xintr, nver, TY_REAL)
	call salloc (yintr, nver, TY_REAL)

	# Find minimum and maximum y values of the polygon vertices.
	call alimr (xver, nver, xmin, xmax)
	call alimr (yver, nver, ymin, ymax)
	if (xmin < 0.5 || xmax > (IM_LEN(im,1) + 0.5) || ymin < 0.5 || ymax >
	    (IM_LEN(im,2) + 0.5))
	    ier = PY_OUTOFBOUNDS
	else
	    ier = PY_OK

	# Find the min and max image line numbers.
	ymin = max (0.5, min (real (IM_LEN (im,2) + 0.5), ymin))
	ymax = min (real (IM_LEN(im,2) + 0.5), max (0.5, ymax))
	linemin = ymin
	if ((ymin - linemin) > 0.0)
	    linemin = max (1, min (IM_LEN(im,2), linemin + 1))
	linemax =  ymax

	# Set up line segment parameters and initialize fluxes.
	x1 = 0.5
	x2 = IM_LEN(im,1) + 0.5
	lx = x2 - x1
	flux = 0.0d0
	area = 0.0

	# Loop over the range of lines of interest.
	do i = linemin, linemax {

	    # Get equation of line segment.
	    ld = i * lx

	    # Find all the intersection points of image line and polygon.
	    nintr = ap_yclip (xver, yver, Memr[xintr], Memr[yintr], nver, lx,
	        ld)
	    if (nintr <= 0)
		next

	    # Read in image line.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		call error (0, "Error reading image")
	    if (i == linemin) {
		if ((ymin - linemin) <= 0.5)
		    fctny = linemin + 0.5 - ymin
		else
		    fctny = 1.0
	    } else if (i == linemax) {
		if ((ymax - linemax) <= 0.5)
		    fctny = ymax + 0.5 - linemax
		else
		    fctny = 1.0
	    } else
		fctny = 1.0

	    # Sort the x intersection points
	    call asrtr (Memr[xintr], Memr[xintr], nintr)

	    # Integrate the flux in the line segment
	    do j = 1, nintr, 2 {

		# Compute the high end fractional pixel contribution.
		xlow = min (real (IM_LEN(im,1) + 0.5), max (0.5,
		    Memr[xintr+j-1]))
		colmin = xlow
		if ((xlow - colmin) > 0.0)
		    colmin = colmin + 1 
		if ((colmin - xlow) <= 0.5) {
		    fctnx1 = colmin + 0.5 - xlow
		    flux = flux + fctny * fctnx1 * Memr[buf+colmin-1] 
		} else {
		    fctnx1 = colmin - xlow - 0.5
		    flux = flux + fctny * (Memr[buf+colmin-1] + fctnx1 *
		        Memr[buf+colmin-2]) 
		    fctnx1 = fctnx1 + 1.0
		}

		# Compute high end fractional pixel contribution.
		xhigh = max (0.5, min (Memr[xintr+j], real (IM_LEN(im,1) +
		    0.5)))
		colmax = xhigh
		if ((xhigh - colmax) <= 0.5) {
		    fctnx2 = xhigh + 0.5  - colmax
		    flux = flux + fctny * fctnx2 * Memr[buf+colmax-1]
		} else {
		    fctnx2 = xhigh - colmax - 0.5
		    flux = flux + fctny * (Memr[buf+colmax-1] + fctnx2 *
		        Memr[buf+colmax])
		    fctnx2 = 1.0 + fctnx2
		}

		# Compute the contribution from the middle of the segment.
		ncols = colmax - colmin - 1
		flux = flux + fctny * asumr (Memr[buf+colmin], ncols)
		area = area + fctny * (ncols + fctnx1 + fctnx2)
	    }
	}
	call sfree (sp)

	if (area <= 0.0)
	    return (PY_NOPIX)
	else if (ier != PY_OK)
	    return (ier)
	else
	    return (PY_OK)
end


# AP_BYYFIT -- Procedure to measure the total flux inside a polygon.

int procedure ap_byyfit (im, xver, yver, nver, datamin, datamax, flux, area,
	badpix)

pointer	im		# pointer to IRAF image
real	xver[ARB]	# x coordinates of the vertices
real	yver[ARB]	# y coordinates of the vertices:
int	nver		# number of vertices
real	datamin		# minimum good data value
real	datamax		# maximum good data value
double	flux		# flux interior to the polygon
real	area		# approximate area of polygon
int	badpix		# are there bad pixels

int	i, j, linemin, linemax, colmin, colmax, ncols, nintr, ier
pointer	sp, xintr, yintr, buf
real	xmin, xmax, ymin, ymax, x1, x2, lx, ld
real	xlow, xhigh, fctnx1, fctnx2, pixval1, pixval2, fctny, dmin, dmax
int	ap_yclip()
pointer	imgl2r()
real	asumr()

begin
	# Check that polygon has at least 3 vertices.
	if (nver < 3) {
	    flux = INDEFD
	    area = 0.0
	    badpix = NO
	    return (PY_NOPOLYGON)
	}

	# Allocate the working space.
	call smark (sp)
	call salloc (xintr, nver, TY_REAL)
	call salloc (yintr, nver, TY_REAL)

	# Find minimum and maximum y values of the polygon vertices.
	call alimr (xver, nver, xmin, xmax)
	call alimr (yver, nver, ymin, ymax)
	if (xmin < 0.5 || xmax > (IM_LEN(im,1) + 0.5) || ymin < 0.5 || ymax >
	    (IM_LEN(im,2) + 0.5))
	    ier = PY_OUTOFBOUNDS
	else
	    ier = PY_OK

	# Find the min and max image line numbers.
	ymin = max (0.5, min (real (IM_LEN (im,2) + 0.5), ymin))
	ymax = min (real (IM_LEN(im,2) + 0.5), max (0.5, ymax))
	linemin = ymin
	if ((ymin - linemin) > 0.0)
	    linemin = max (1, min (IM_LEN(im,2), linemin + 1))
	linemax =  ymax

	# Set up line segment parameters and initialize fluxes.
	x1 = 0.5
	x2 = IM_LEN(im,1) + 0.5
	lx = x2 - x1
	flux = 0.0d0
	area = 0.0

	# Loop over the range of lines of interest.
	badpix = NO
	do i = linemin, linemax {

	    # Get equation of line segment.
	    ld = i * lx

	    # Find all the intersection points of image line and polygon.
	    nintr = ap_yclip (xver, yver, Memr[xintr], Memr[yintr], nver, lx,
	        ld)
	    if (nintr <= 0)
		next

	    # Read in image line.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		call error (0, "Error reading image")
	    if (i == linemin) {
		if ((ymin - linemin) <= 0.5)
		    fctny = linemin + 0.5 - ymin
		else
		    fctny = 1.0
	    } else if (i == linemax) {
		if ((ymax - linemax) <= 0.5)
		    fctny = ymax + 0.5 - linemax
		else
		    fctny = 1.0
	    } else
		fctny = 1.0

	    # Sort the x intersection points
	    call asrtr (Memr[xintr], Memr[xintr], nintr)

	    # Integrate the flux in the line segment
	    do j = 1, nintr, 2 {

		# Compute the high end fractional pixel contribution.
		xlow = min (real (IM_LEN(im,1) + 0.5), max (0.5,
		    Memr[xintr+j-1]))
		colmin = xlow
		if ((xlow - colmin) > 0.0)
		    colmin = colmin + 1 
		pixval1 = Memr[buf+colmin-1]
		if ((colmin - xlow) <= 0.5) {
		    fctnx1 = colmin + 0.5 - xlow
		    flux = flux + fctny * fctnx1 * pixval1
		    if (pixval1 < datamin || pixval1 > datamax)
			badpix = YES
		} else {
		    pixval2 = Memr[buf+colmin-2]
		    fctnx1 = colmin - xlow - 0.5
		    flux = flux + fctny * (pixval1 + fctnx1 * pixval2) 
		    fctnx1 = fctnx1 + 1.0
		    if (pixval2 < datamin || pixval2 > datamax)
			badpix = YES
		}

		# Compute high end fractional pixel contribution.
		xhigh = max (0.5, min (Memr[xintr+j], real (IM_LEN(im,1) +
		    0.5)))
		colmax = xhigh
		pixval1 = Memr[buf+colmax-1]
		if ((xhigh - colmax) <= 0.5) {
		    fctnx2 = xhigh + 0.5  - colmax
		    flux = flux + fctny * fctnx2 * pixval1
		    if (pixval1 < datamin || pixval1 > datamax)
			badpix = YES
		} else {
		    pixval2 = Memr[buf+colmax]
		    fctnx2 = xhigh - colmax - 0.5
		    flux = flux + fctny * (pixval1 + fctnx2 * pixval2)
		    fctnx2 = 1.0 + fctnx2
		    if (pixval2 < datamin || pixval2 > datamax)
			badpix = YES
		}

		# Compute the contribution from the middle of the segment.
		ncols = colmax - colmin - 1
		call alimr (Memr[buf+colmin], ncols, dmin, dmax)
		if (dmin < datamin || dmax > datamax)
		    badpix = YES
		flux = flux + fctny * asumr (Memr[buf+colmin], ncols)
		area = area + fctny * (ncols + fctnx1 + fctnx2)
	    }
	}
	call sfree (sp)

	if (area <= 0.0)
	    return (PY_NOPIX)
	if (badpix == YES)
	    return (PY_BADDATA)
	else if (ier != PY_OK)
	    return (ier)
	else
	    return (PY_OK)
end


# AP_YCLIP -- Procedure to determine the intersection points of a
# horizontal image line with an arbitrary polygon.

int procedure ap_yclip (xver, yver, xintr, yintr, nver, lx, ld)

real	xver[ARB]		# x vertex coords
real	yver[ARB]		# y vertex coords
real	xintr[ARB]		# x intersection coords
real	yintr[ARB]		# y intersection coords
int	nver			# number of vertices
real	lx, ld 			# equation of image line

int	i, nintr
real	u1, u2, u1u2, dx, dy, dd, xa, ya, wa

begin
	nintr = 0
	u1 = - lx * yver[1] + ld
	do i = 2, nver {

	    u2 = - lx * yver[i] + ld
	    u1u2 = u1 * u2

	    # Test whether polygon line segment intersects image line or not.
	    if (u1u2 <= 0.0) {


		# Compute the intersection coords.
		if (u1 != 0.0 && u2 != 0.0) {

		    dy = yver[i-1] - yver[i]
		    dx = xver[i-1] - xver[i]
		    dd = xver[i-1] * yver[i] - yver[i-1] * xver[i]
		    xa = (dx * ld - lx * dd)
		    ya = dy * ld 
		    wa = dy * lx
		    nintr = nintr + 1
		    xintr[nintr] = xa / wa
		    yintr[nintr] = ya / wa

		# Test for collinearity.
		} else if (u1 == 0.0 && u2 == 0.0) {

		    nintr = nintr + 1
		    xintr[nintr] = xver[i-1]
		    yintr[nintr] = yver[i-1]
		    nintr = nintr + 1
		    xintr[nintr] = xver[i]
		    yintr[nintr] = yver[i]

		} else if (u1 != 0.0) {

		    if (i == 1) {
			dy = (yver[2] - yver[1])
			dd = (yver[nver-1] - yver[1])
		    } else if (i == nver) {
			dy = (yver[2] - yver[nver])
			dd = dy * (yver[nver-1] - yver[nver])
		    } else {
			dy = (yver[i+1] - yver[i])
			dd = dy * (yver[i-1] - yver[i])
		    }

		    if (dy != 0.0) {
			nintr = nintr + 1
			xintr[nintr] = xver[i]
			yintr[nintr] = yver[i]
		    }

		    if (dd > 0.0) {
			nintr = nintr + 1
			xintr[nintr] = xver[i]
			yintr[nintr] = yver[i]
		    }

		}
	    }

	    u1 = u2
	}

	return (nintr)
end
