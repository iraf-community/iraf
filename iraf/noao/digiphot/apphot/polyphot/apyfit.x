include <imhdr.h>
include <mach.h>
include "../lib/apphot.h"
include "../lib/noise.h"
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

double	flux, area
int	noise, badpix, ier
real	datamin, datamax, mag, magerr, zmag, padu, itime, readnoise
int	apstati(), ap_yyfit(), ap_byyfit()
real	apstatr()

begin
	# Initialize.
	call apsetd (py, PYFLUX, 0.0d0)
	call apsetd (py, PYNPIX, 0.0d0)
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
	else if (ier == PY_NOPIX)
	    return (PY_NOPIX)

	# Store the results.
	call apsetd (py, PYFLUX, flux)
	call apsetd (py, PYNPIX, area)
	call apseti (py, PYBADPIX, badpix)

	if (IS_INDEFR(skyval))
	    return (PY_NOSKYMODE)

	# Get the photometry parameters.
	zmag = apstatr (py, PYZMAG)
	itime = apstatr (py, ITIME)
	noise = apstati (py, NOISEFUNCTION)
	padu = apstatr (py, EPADU)
	readnoise = apstatr (py, READNOISE)

	# Compute the magnitude and error.
	if (badpix == NO) {
	    if (apstati (py, POSITIVE) == YES)
	        call apcopmags (flux, area, mag, magerr, 1, skyval,
		    skysig, nsky, zmag, noise, padu)
	    else
	        call apconmags (flux, area, mag, magerr, 1, skyval,
		    skysig, nsky, zmag, noise, padu, readnoise)
	    mag  = mag + 2.5 * log10 (itime)

	    call apsetr (py, PYMAG, mag)
	    call apsetr (py, PYMAGERR, magerr)
	}

	return (ier)
end


# AP_YYFIT -- Measure the total flux inside a polygon.

int procedure ap_yyfit (im, xver, yver, nver, flux, area)

pointer	im		# pointer to IRAF image
real	xver[ARB]	# x coordinates of the vertices
real	yver[ARB]	# y coordinates of the vertices:
int	nver		# number of vertices
double	flux		# flux interior to the polygon
double	area		# approximate area of polygon

double	fluxx, areax, fctnx, fctny
real	xmin, xmax, ymin, ymax, x1, x2, lx, ld
pointer	sp, work1, work2, xintr, buf
int	i, j, k, linemin, linemax, colmin, colmax, nintr, ier
int	ap_yclip()
pointer	imgl2r()

begin
	# Check that polygon has at least 3 vertices plus the closing vertex.
	if (nver < 4) {
	    flux = INDEFD
	    area = 0.0d0
	    return (PY_NOPOLYGON)
	}

	# Allocate working space.
	call smark (sp)
	call salloc (work1, nver, TY_REAL)
	call salloc (work2, nver, TY_REAL)
	call salloc (xintr, nver, TY_REAL)

	# Find the minimum and maximum x and y values of the polygon
	# and detemine whether the polygon is partially off the image.

	call alimr (xver, nver, xmin, xmax)
	call alimr (yver, nver, ymin, ymax)
	if (xmin < 0.5 || xmax > (IM_LEN(im,1) + 0.5) || ymin < 0.5 || ymax >
	    (IM_LEN(im,2) + 0.5))
	    ier = PY_OUTOFBOUNDS
	else
	    ier = PY_OK

	# Find the minimum and maximum image line numbers.
	ymin = max (0.5, min (real (IM_LEN (im,2) + 0.5), ymin))
	ymax = min (real (IM_LEN(im,2) + 0.5), max (0.5, ymax))
	linemin = min (int (ymin + 0.5), int (IM_LEN(im,2)))
	linemax = min (int (ymax + 0.5), int (IM_LEN(im,2)))

	# Set up the line segment parameters and initialize fluxes and areas.
	x1 = 0.5
	x2 = IM_LEN(im,1) + 0.5
	lx = x2 - x1
	flux = 0.0d0
	area = 0.0d0

	# Loop over the range of lines of interest.
	do i = linemin, linemax {

	    # Read in image line.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		next

	    # Find all the x intersection points of image line and polygon.
	    if (ymin > i)
		ld = min (i + 1, linemax)
	    else if (ymax < i)
		ld = max (i - 1, linemin)
	    else
	        ld = i
	    nintr = ap_yclip (xver, yver, Memr[work1], Memr[work2],
	        Memr[xintr],  nver, lx, ld)
	    if (nintr <= 0)
		next
	    fctny = min (i + 0.5, ymax) - max (i - 0.5, ymin)

	    # Sort the x intersection points
	    call asrtr (Memr[xintr], Memr[xintr], nintr)

	    # Integrate the flux in each line segment.
	    fluxx = 0.0d0
	    areax = 0.0d0
	    do j = 1, nintr, 2 {

		# Compute the line segment limits.
		xmin = min (real (IM_LEN(im,1) + 0.5), max (0.5,
		    Memr[xintr+j-1]))
		xmax = min (real (IM_LEN(im,1) + 0.5), max (0.5,
		    Memr[xintr+j]))
		colmin = min (int (xmin + 0.5), int (IM_LEN(im,1)))
		colmax = min (int (xmax + 0.5), int (IM_LEN(im,1)))

		# Sum the contribution from a particular line segment.
		do k = colmin, colmax {
	            fctnx = min (k + 0.5, xmax) - max (k - 0.5, xmin)
		    fluxx = fluxx + fctnx * Memr[buf+k-1]
		    areax = areax + fctnx
		}
	    }

	    # Add the line sum to the total.
	    area = area + areax * fctny
	    flux = flux + fluxx * fctny
	}

	call sfree (sp)

	# Return the appropriate error code.
	if (area <= 0.0d0)
	    return (PY_NOPIX)
	else if (ier != PY_OK)
	    return (ier)
	else
	    return (PY_OK)
end


# AP_BYYFIT -- Measure the total flux inside a polygon while searching for
# bad pixels at the same time.

int procedure ap_byyfit (im, xver, yver, nver, datamin, datamax, flux, area,
	badpix)

pointer	im		# pointer to IRAF image
real	xver[ARB]	# x coordinates of the vertices
real	yver[ARB]	# y coordinates of the vertices:
int	nver		# number of vertices
real	datamin		# minimum good data value
real	datamax		# maximum good data value
double	flux		# flux interior to the polygon
double	area		# approximate area of polygon
int	badpix		# are there bad pixels

int	i, j, k, linemin, linemax, colmin, colmax, nintr, ier
pointer	sp, work1, work2, xintr, buf
real	xmin, xmax, ymin, ymax, x1, x2, lx, ld
double	fluxx, areax, fctnx, fctny
int	ap_yclip()
pointer	imgl2r()

begin
	# Check that polygon has at least 3 vertices plus a closing vertex.
	if (nver < 4) {
	    flux = INDEFD
	    area = 0.0d0
	    badpix = NO
	    return (PY_NOPOLYGON)
	}

	# Allocate working space.
	call smark (sp)
	call salloc (work1, nver, TY_REAL)
	call salloc (work2, nver, TY_REAL)
	call salloc (xintr, nver, TY_REAL)

	# Find minimum and maximum y values of the polygon vertices and
	# compute the minimum and maximum image line limits.

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
	linemin = max (1, min (int (ymin + 0.5), int (IM_LEN(im,2))))
	linemax = max (1, min (int (ymax + 0.5), int (IM_LEN(im,2))))

	# Set up line segment parameters and initialize fluxes.
	x1 = 0.5
	x2 = IM_LEN(im,1) + 0.5
	lx = x2 - x1
	flux = 0.0d0
	area = 0.0d0

	# Loop over the range of lines of interest.
	badpix = NO
	do i = linemin, linemax {

	    # Read in the image line.
	    buf = imgl2r (im, i)
	    if (buf == EOF)
		next

	    # Find all the intersection points.
	    if (ymin > i)
		ld = min (i + 1, linemax)
	    else if (ymax < i)
		ld = max (i - 1, linemin)
	    else
	        ld = i
	    nintr = ap_yclip (xver, yver, Memr[work1], Memr[work2],
	        Memr[xintr], nver, lx, ld)
	    if (nintr <= 0)
		next
	    fctny = min (i + 0.5, ymax) - max (i - 0.5, ymin)

	    # Sort the x intersection points
	    call asrtr (Memr[xintr], Memr[xintr], nintr)

	    # Integrate the flux in the line segment
	    fluxx = 0.0d0
	    areax = 0.0d0
	    do j = 1, nintr, 2 {

		# Compute the line segment limits.
		xmin = min (real (IM_LEN(im,1) + 0.5), max (0.5,
		    Memr[xintr+j-1]))
		xmax = min (real (IM_LEN(im,1) + 0.5), max (0.5, Memr[xintr+j]))
		colmin = min (int (xmin + 0.5), int (IM_LEN(im,1)))
		colmax = min (int (xmax + 0.5), int (IM_LEN(im,1)))

		# Sum the contribution from a particular line segment.
		do k = colmin, colmax {
	            fctnx = min (k + 0.5, xmax) - max (k - 0.5, xmin)
		    fluxx = fluxx + fctnx * Memr[buf+k-1]
		    areax = areax + fctnx
		    if (Memr[buf+k-1] < datamin || Memr[buf+k-1] > datamax)
			badpix = YES
		}
	    }

	    # Add the line sum to the total.
	    area = area + areax * fctny
	    flux = flux + fluxx * fctny
	}

	call sfree (sp)

	if (area <= 0.0d0)
	    return (PY_NOPIX)
	if (badpix == YES)
	    return (PY_BADDATA)
	else if (ier != PY_OK)
	    return (ier)
	else
	    return (PY_OK)
end


# AP_YCLIP -- Compute the intersection of an image line with a polygon defined
# by a list of vertices.  The output is a list of ranges stored in the array
# xranges. Two work additional work arrays xintr and slope are required for
# the computation.

int procedure ap_yclip (xver, yver, xintr, slope, xranges, nver, lx, ld)

real	xver[ARB]		# x vertex coords
real	yver[ARB]		# y vertex coords
real	xintr[ARB]		# work array of x intersection points
real	slope[ARB]		# work array of y slopes at intersection points
real	xranges[ARB]		# x line segments
int	nver			# number of vertices
real	lx, ld 			# equation of image line

bool	collinear
int	i, j, nintr, nplus, nzero, nneg, imin, imax, nadd
real	u1, u2, u1u2, dx, dy, dd, xa, wa

begin
	# Compute the intersection points of the image line and the polygon.
	collinear = false
	nplus = 0
	nzero = 0
	nneg = 0
	nintr = 0
	#u1 = - lx * yver[1] + ld
	u1 = lx * (- yver[1] + ld)
	do i = 2, nver {

	    #u2 = - lx * yver[i] + ld
	    u2 = lx * (- yver[i] + ld)
	    u1u2 = u1 * u2

	    # Does the polygon side intersect the image line ?
	    if (u1u2 <= 0.0) {


		# Compute the x intersection coordinate if the point of
		# intersection is not a vertex.

		if ((u1 != 0.0) && (u2 != 0.0)) {

		    dy = yver[i-1] - yver[i]
		    dx = xver[i-1] - xver[i]
		    dd = xver[i-1] * yver[i] - yver[i-1] * xver[i]
		    #xa = (dx * ld - lx * dd)
		    xa = lx * (dx * ld - dd)
		    wa = dy * lx
		    nintr = nintr + 1
		    xranges[nintr] = xa / wa
		    slope[nintr] = -dy
		    if (slope[nintr] < 0.0)
			nneg = nneg + 1
		    else if (slope[nintr] > 0.0)
			nplus = nplus + 1
		    else
			nzero = nzero + 1
		    collinear = false

		# For each collinear line segment add two intersection
		# points. Remove interior collinear intersection points.

		} else if (u1 == 0.0 && u2 == 0.0) {

		    if (! collinear) {
		        nintr = nintr + 1
			xranges[nintr] = xver[i-1]
			if (i == 2)
			    slope[nintr] = yver[1] - yver[nver-1]
			else
			    slope[nintr] = yver[i-1] - yver[i-2]
		        if (slope[nintr] < 0.0)
			    nneg = nneg + 1
		        else if (slope[nintr] > 0.0)
			    nplus = nplus + 1
		        else
			    nzero = nzero + 1
		        nintr = nintr + 1
		        xranges[nintr] = xver[i]
			slope[nintr] = 0.0
			nzero = nzero + 1
		    } else {
		        xranges[nintr] = xver[i]
			slope[nintr] = 0.0
			nzero = nzero + 1
		    }
		    collinear = true

		# If the intersection point is a vertex add it to the
		# list if it is not collinear with the next point. Add
		# another point to the list if the vertex is at the
		# apex of an acute angle.

		} else if (u1 != 0.0) {

		    if (i == nver) {
		        dx = (xver[2] - xver[nver])
			dy = (yver[2] - yver[nver])
			dd = dy * (yver[nver-1] - yver[nver])
		    } else {
			dx = (xver[i+1] - xver[i])
			dy = (yver[i+1] - yver[i])
			dd = dy * (yver[i-1] - yver[i])
		    }

		    # Test whether the point is collinear with the point
		    # ahead. If it is not include the intersection point. 

		    if (dy != 0.0) {
			nintr = nintr + 1
			xranges[nintr] = xver[i]
			slope[nintr] = yver[i] - yver[i-1]
		        if (slope[nintr] < 0.0)
			    nneg = nneg + 1
		        else if (slope[nintr] > 0.0)
		            nplus = nplus + 1
		        else
			    nzero = nzero + 1
		    }

		    # If the intersection point is an isolated vertex add
		    # another point to the list.

		    if (dd > 0.0) {
			nintr = nintr + 1
			xranges[nintr] = xver[i]
			slope[nintr] = dy
		        if (slope[nintr] < 0.0)
			    nneg = nneg + 1
		        else if (slope[nintr] > 0.0)
		            nplus = nplus + 1
		        else
			    nzero = nzero + 1
		    }

		    collinear = false

		} else
		    collinear = false
	    } else
		collinear = false

	    u1 = u2
	}

	# Join up any split collinear line segments.
	if (collinear && (slope[1] == 0.0)) {
	    xranges[1] = xranges[nintr-1]
	    slope[1] = slope[nintr-1]
	    nintr = nintr - 2
	    nzero = nzero - 2
	}

	# Return the number of intersection points if there are no interior
	# collinear line segments.
	if (nzero == 0 || nplus == 0 || nneg == 0)
	    return (nintr)

	# Find the minimum and maximum intersection points.
	call ap_alimr (xranges, nintr, u1, u2, imin, imax)

	# Check for vertices at the ends of the ranges.

	u1 = xranges[min(imin,imax)] - xranges[1]
	u2 = xranges[nintr] - xranges[max(imin,imax)]

	# Vertices were traversed in order of increasing x.
	if ((u1 >= 0.0 && u2 > 0.0) || (u1 > 0.0 && u2 >= 0.0) ||
	    (u1 == u2 && imax > imin)) {
	    do i = imax + 1, nintr {
		if (xranges[i] != xranges[i-1])
		    break
		imax = i
	    }
	    do i = imin - 1, 1, -1 {
		if (xranges[i] != xranges[i+1])
		    break
		imin = i
	    }
	}

	# Vertices were traversed in order of decreasing x.
	if ((u1 <= 0.0 && u2 < 0.0) || (u1 < 0.0 && u2 <= 0.0) || 
	    (u1 == u2 && imax < imin)) {
	    do i = imin + 1, nintr {
		if (xranges[i] != xranges[i-1])
		    break
		imin = i
	    }
	    do i = imax - 1, 1, -1 {
		if (xranges[i] != xranges[i+1])
		    break
		imax = i
	    }
	}

	# Reorder the x ranges and slopes if necessary. 
	if ((imax < imin) && ! (imin == nintr && imax == 1)) {
	    call amovr (xranges, xintr, nintr)
	    do i = 1, imax
	        xranges[nintr-imax+i] = xintr[i]
	    do i = imin, nintr
	        xranges[i-imax] = xintr[i]
	    call amovr (slope, xintr, nintr)
	    do i = 1, imax
	        slope[nintr-imax+i] = xintr[i]
	    do i = imin, nintr
	        slope[i-imax] = xintr[i]
	} else if ((imin < imax) && ! (imin == 1 && imax == nintr)) {
	    call amovr (xranges, xintr, nintr)
	    do i = 1, imin
		xranges[nintr-imin+i] = xintr[i]
	    do i = imax, nintr
		xranges[i-imin] = xintr[i]
	    call amovr (slope, xintr, nintr)
	    do i = 1, imin
		slope[nintr-imin+i] = xintr[i]
	    do i = imax, nintr
		slope[i-imin] = xintr[i]
	}

	# Add any extra intersection points that are required to deal with
	# the collinear line segments.

	nadd = 0
	for (i = 1; i <= nintr-2; ) {
	    if (slope[i] * slope[i+2] > 0.0) {
		i = i + 2
	    } else {
		nadd = nadd + 1
		xranges[nintr+nadd] = xranges[i+1]
		for (j = i + 3; j <= nintr; j = j + 1) {
		    if (slope[i] * slope[j] > 0)
			break
		    nadd = nadd + 1
		    xranges[nintr+nadd] = xranges[j-1]
		}
		i = j
	    }
	}

	return (nintr + nadd)
end
