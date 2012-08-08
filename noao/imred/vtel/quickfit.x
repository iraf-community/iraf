include <mach.h>
include <imhdr.h>
include	"vt.h"

define	SZ_VTPBUF	4096		# Size of limb point buffer.

# QUICKFIT -- Given a fulldisk solar image, find the parameters of an ellipse
# that best fits the limb.  First the points on the limb are determined using
# the squibby brightness, then an initial guess for the limb parameters is
# made, and finally a least squares fit is made by an iterative method.

procedure t_quickfit()

char	image[SZ_FNAME]				# image to find the limb on
int	threshold				# squibby limb threshold
bool	verbose					# verbose flag

pointer	pb					# buffer for saving limb points
int	npoints, rejects			# number of limb pts, rejects
real	x, y, a, b				# x, y, a, b (a = z0)
real	rguess, rpercent			# initial guess at r, % rejects
errchk	limbfind, efit
pointer	im, sp

pointer	immap()
int	clgeti()
bool	clgetb()
errchk	immap, limbfind

begin
	call smark (sp)
	call salloc (pb, 2*SZ_VTPBUF, TY_INT)

	# Get parameters from the cl.
	call clgstr ("image", image, SZ_FNAME)
	threshold = clgeti ("threshold")
	verbose = clgetb ("verbose")

	# Open image.
	im = immap (image, READ_WRITE, 0)

	# Get the point buffer and npoints.
	iferr (call limbfind (im, Memi[pb], npoints, threshold, rguess,
	    verbose))
	    call eprintf("Error getting limbpoints.\n")
	if (verbose) {
	    call printf ("\nrguess = %g\n")
	        call pargr (rguess)
	        call flush (STDOUT)
	}

	# Fit the ellipse.
	b = rguess
	a = rguess
	x = real(DIM_VTFD)/2.
	y = real(DIM_VTFD)/2.
	iferr (call efit (Memi[pb], npoints, x, y, a, b, rejects, verbose))
	    call eprintf ("Error fitting elipse.\n")

	rpercent = real(rejects)/real(npoints)
	if (verbose) {
	    call printf ("\nTotal number of limbpoints found was %d\n")
	        call pargi (npoints)
	    call printf ("Number of limbpoints rejected was %d\n")
	        call pargi (rejects)
	    call printf ("Fraction of limb points rejected = %g\n")
	        call pargr (rpercent)
	        call flush (STDOUT)
	}

	# Put ellipse parameters in image header.
	call imaddr (im, "E_XCEN", x)
	call imaddr (im, "E_YCEN", y)
	call imaddr (im, "E_XSMD", a)
	call imaddr (im, "E_YSMD", b)

	# Close the image.
	call imunmap (im)

	call sfree (sp)
end


# LIMBFIND - Find all of the points on the image that determine the
# limb.  This is done line by line.

procedure limbfind (imageptr, pointbuf, npoints, threshold, rguess, verbose)

pointer	imageptr		# pointer to image
int	pointbuf[SZ_VTPBUF,2]	# buffer in which to store limb points
int	npoints			# number of points
int	threshold		# squibby threshold
real	rguess			# first guess at radius
bool	verbose			# verbose flag

int	rowspace, halfwidth, leftsave, rightsave, y
int	numpix, numrow, leftx, rightx, yesno
int	month, day, year, hour, minute, second, obsdate, obstime
real	b0, l0
pointer	lpg

pointer	imgl2s()
int	clgeti(), imgeti()
errchk	ephem, flocr, florr, imgl2s

begin
	# Get date and time from the header.
	obsdate = imgeti (imageptr, "OBS_DATE")
	obstime = imgeti (imageptr, "OBS_TIME")

	# Calculate the month/day/year.
	month = obsdate/10000
	day = obsdate/100 - 100 * (obsdate/10000)
	year = obsdate - 100 * (obsdate/100)

	# Calculate the hour:minute:second.
	hour = int(obstime/3600)
	minute = int((obstime - hour * 3600)/60)
	second = obstime - hour * 3600 - minute * 60
	if (verbose) {
	    call printf("date and time of this image = %d/%d/%d, %d:%d:%d\n")
	        call pargi(month)
	        call pargi(day)
	        call pargi(year)
	        call pargi(hour)
	        call pargi(minute)
	        call pargi(second)
	    call flush (STDOUT)
	}

	# Get rowspace and halfwidth from the cl.
	halfwidth = clgeti("halfwidth")
	rowspace = clgeti("rowspace")

	numpix = IM_LEN(imageptr, 1)
	numrow = IM_LEN(imageptr, 2)
	npoints = 0

	# Get rguess from ephem.
	iferr (call ephem (month, day, year, hour, minute, second, rguess,
	    b0, l0, verbose))
	    call eprintf ("Error getting ephemeris data.\n")

	# Put b0 and l0 in the image header.
	call imaddr (imageptr, "B_ZERO", b0)
	call imaddr (imageptr, "L_ZERO", l0)

	# Get central row to start with and find its limb points.
	lpg = imgl2s (imageptr, numrow/2)
	yesno = YES
	iferr (call flocr (Mems[lpg], numpix, pointbuf, numrow, npoints, leftx,
	    rightx, threshold, yesno))
	    call eprintf ("Error in 'find limb on center row(flocr)'\n")
	if (yesno == NO)
	    call error (0,"Failure to find initial limb points, quickfit dies")

	leftsave = leftx
	rightsave = rightx

	# Find the limb points for the lower half of the image.
	yesno = YES
	y = numrow/2-rowspace
	while (y >= 1) {

	    # Read this line in from the image.
	    lpg = imgl2s (imageptr, y)

	    # Find its limb points.
	    iferr (call florr (Mems[lpg], numpix, pointbuf, npoints, numrow,
	        y, leftx, rightx, threshold, yesno, rguess, halfwidth))
		call eprintf ("Error in florr.\n")
	    if (yesno == NO)
		break
	    if (abs(y-numrow/2) > rguess)
		break
	    if ((int(rowspace * (rguess**2 -
		real(y-numrow/2)**2)**.5/rguess)) >= 1)
	        y = y - int(rowspace * (rguess**2 -
		    real(y-numrow/2)**2)**.5/rguess)
	    else
		y = y - 1
	}

	# Find the limb points for the upper half of the image.

	# Restore the pointers to the limb at disk center.
	leftx = leftsave
	rightx = rightsave
	yesno = NO
	y = numrow/2+rowspace

	while (y <= numrow) {
	    # Read this line in from the image.
	    lpg = imgl2s (imageptr, y)

	    # Find its limb points.
	    iferr (call florr (Mems[lpg], numpix, pointbuf, npoints, numrow,
		y, leftx, rightx, threshold, yesno, rguess, halfwidth))
		call eprintf ("Error in florr.\n")

	    # If we couldn't find any limb points then it's time to go.
	    if (yesno == NO)
		break

	    # If we are beyond the limb vertically then its time to go.
	    if (abs(y-numrow/2) > rguess)
		break

	    # If the calculated rowspacing gets less than 1, just set it to 1.
	    if ((int(rowspace * (rguess**2 -
		real(y-numrow/2)**2)**.5/rguess)) >= 1) {
	        y = y + int(rowspace * (rguess**2 -
		    real(y-numrow/2)**2)**.5/rguess)
	    } else
		y = y + 1
	}
end


# FLOCR -- Find Limbpoints On Center Row.  Since this is the first row
# to be searched, we have no idea of approximately where the limb points
# will be found in the row as we have in florr.  We search from the endpoints
# of the row inward until the squibby brightness crosses the threshold.

procedure flocr (array, numpix, pointbuf, npoints, numrow, leftx, rightx,
		    threshold, yesno)

short	array[numpix]		# line of image
int	pointbuf[SZ_VTPBUF,2]	# limb point storage array
int	numpix			# number of pixels in line
int	npoints			# number of limb points
int	numrow			# which row this is in image
int	leftx			# return left boundary position here
int	rightx			# return right boundary position here
int	threshold		# squibby brightness limb threshold
int	yesno			# return yes if we found the limb

int	i, j, foundi, foundj

begin
	# Start at beginning and end of array and work in.
	i = 1
	j = numpix

	# Flags that indicate when a limbpoint has been found.
	foundi = 0
	foundj = 0

	while (i <= j) {
	    if (foundi == 0) {
		if (and(int(array[i]), 17B) >= threshold) {
		    foundi = 1
		    npoints = npoints + 1
		    pointbuf[npoints,1] = i
		    pointbuf[npoints,2] = numrow/2
		    leftx = i
		}
		if (i == j) {
		    yesno = NO
		    return
		}
	    }

	    if (foundj == 0) {
		if (and(int(array[j]), 17B) >= threshold) {
		    foundj = 1
		    npoints = npoints + 1
		    pointbuf[npoints,1] = j
		    pointbuf[npoints,2] = numrow/2
		    rightx = j
		}
	    }
	    if ((foundi == 1) && (foundj == 1))
		break
	    i = i + 1
	    j = j - 1
	}
end


# FLORR -- Find Limbpoints On Random Row. Since we know the approximate
# positions of the limbpoints based on their positions on the ajacent
# row, we can restrict the range of x positions to be searched to those
# within a certain distance of those positions.  These ranges we will
# call windows.  Each window is checked for validity before it is
# searched for the limbpoints, if invalid a correct window is found.

procedure florr (array, numpix, pointbuf, npoints, numrow, y, leftx, rightx,
	threshold, yesno, rguess, halfwidth)

short	array[numpix]		# line of image
int	pointbuf[SZ_VTPBUF,2]	# limb point storage array
int	numpix			# number of pixels in line
int	npoints			# number of limb points
int	numrow			# which row this is in image
int	leftx			# return left boundary position here
int	rightx			# return right boundary position here
int	threshold		# squibby brightness limb threshold
int	yesno			# return yes if we found the limb
int	halfwidth		# halfwidth of limb search window
real	rguess			# radius for sun guess

int	i, j, y

begin
	# Windows are leftx plus or minus halfwidth and rightx plus or
	# minus halfwidth.  Before searching windows, check them for
	# validity and call newwindow if necessary.

	# Check for validity means the endpoint we expect to be outside
	# the limb should have a squibby brightness less than the
	# threshold and the inside the limb endpoint should have a
	# squibby brightness greater than the threshold.

	# if invalid...
	if ((and(int(array[max(1,(leftx-halfwidth))]),17B) >= threshold) ||
	    (and(int(array[leftx+halfwidth]),17B) < threshold)) {

	    # if we are getting too far from the center (outside limb)
	    # then return flag for no limbpoints.

	    if (abs(y-numrow/2) > int(rguess)) {
		yesno = NO
		return
	    }

	    # Otherwise calculate a new leftx for this row.
	    leftx = -((int(rguess**2) - (y-numrow/2)**2)**.5) + numrow/2
	}

	# If we now have a valid window...
	if ((and(int(array[max(1,(leftx-halfwidth))]),17B) < threshold) &&
	    (and(int(array[leftx+halfwidth]),17B) >= threshold)) {

	    # Search window for limb point.
	    do i = max(1,(leftx-halfwidth)), leftx+halfwidth {

		# When we find it add it to the limbpoints array and
		# break out of the do loop

	        if (and(int(array[i]), 17B) >= threshold) {

		    # Set the 'we found it' flag.
		    yesno = YES

		    npoints = npoints + 1
		    pointbuf[npoints,1] = i
		    pointbuf[npoints,2] = y
		    leftx = i
		    break
	        }
	    }
	}

	# Same stuff for the right hand window.
	if ((and(int(array[min(numpix,(rightx+halfwidth))]),17B) >=
	    threshold) || (and(int(array[rightx-halfwidth]),17B) < threshold)) {
	    if (abs(y-numrow/2) > int(rguess)) {
		yesno = NO
		return
	    }
	    rightx = (int(rguess**2) - (y-numrow/2)**2)**.5 + numrow/2
	}

	if ((and(int(array[min(numpix,(rightx+halfwidth))]),17B) < threshold) &&
	    (and(int(array[rightx-halfwidth]),17B) >= threshold)) {
	    do j = min(numpix,(rightx+halfwidth)), rightx-halfwidth, -1 {
	        if (and(int(array[j]), 17B) >= threshold) {
		    yesno = YES
		    npoints = npoints + 1
		    pointbuf[npoints,1] = j
		    pointbuf[npoints,2] = y
		    rightx = j
		    break
	        }
	    }
	}
end


# EFIT - Find the best fitting ellipse to the limb points.  We iterate
# 10 times, this seems to converge very well.
# Algorithm due to Jack Harvey.

procedure efit (pointbuf, npoints, xzero, yzero, azero, bzero, rejects,
	verbose)

int	pointbuf[SZ_VTPBUF,2]		# buffer containing limb points
int	npoints 			# number of limb points
real	xzero, yzero, azero, bzero	# return elipse parameters
int	rejects				# number of points rejected
bool	verbose				# verbose flag

int	i, j, ij, n
real	xcenter, ycenter, a, b, a2, b2, a3, b3
real	z[6,6]
real	x1, y1, x2, y2, q[5], fn, sq
real	rejectcoeff

real	clgetr()

begin
	# Get the least squares rejection coefficient.
	rejectcoeff = clgetr("rejectcoeff")
	xcenter = xzero
	ycenter = yzero
	a = azero
	b = azero

	do ij = 1, 10 {
	    a2 = a**2
	    a3 = a2 * a
	    b2 = b**2
	    b3 = b2 * b
	    sq = 0.

	    do i = 1, 6
		do j = 1, 6
		    z[i,j] = 0

	    fn = 0.
	    rejects = 0

	    do n = 1, npoints {
		x1 = real(pointbuf[n,1]) - xcenter
		y1 = real(pointbuf[n,2]) - ycenter
		x2 = x1**2
		y2 = y1**2
		q[1] = x1/a2
		q[2] = y1/b2
		q[3] = -x2/a3
		q[4] = -y2/b3
		q[5] = .5 * (1. - x2/a2 - y2/b2)

		# Reject a point if it is too far from the approximate ellipse.
		if (abs(q[5]) >= rejectcoeff) {
		    rejects = rejects + 1
		    next
		}

		sq = sq + q[5]

		do i = 1, 5
		    do j = i, 5
			z[i,j+1] = z[i,j+1] + q[i] * q[j]

		fn = fn + 1.
	    }

	    sq = sq/fn
	    call flush(STDOUT)
	    call lstsq (z, 6, fn)
	    if (z(5,3) > 3.)
		z(5,3) = 3.
	    if (z(5,3) < -3.)
		z(5,3) = -3.
	    if (z(5,4) > 3.)
		z(5,4) = 3.
	    if (z(5,4) < -3.)
		z(5,4) = -3.
	    if (z(5,1) > 10.)
		z(5,1) = 10.
	    if (z(5,1) < -10.)
		z(5,1) = -10.
	    if (z(5,2) > 10.)
		z(5,2) = 10.
	    if (z(5,2) < -10.)
		z(5,2) = -10.
	    a = a + z[5,3]
	    b = b + z[5,4]
	    xcenter = xcenter - z[5,1]
	    ycenter = ycenter - z[5,2]

	    if (verbose) {
		call printf ("x = %f, y = %f, a = %f, b = %f, sq = %13.10f\n")
		    call pargr (xcenter)
		    call pargr (ycenter)
		    call pargr (a)
		    call pargr (b)
		    call pargr (sq)
		call flush (STDOUT)
	    }
	}

	if (verbose) {
	    call printf ("\nCoordinates of center are x = %f, y = %f\n")
	        call pargr(xcenter)
	        call pargr(ycenter)
	    call printf ("xsemidiameter = %f, ysemidiameter = %f\n")
	        call pargr(a)
	        call pargr(b)
	    call flush (STDOUT)
	}

	xzero = xcenter
	yzero = ycenter
	azero = a
	bzero = b
end
