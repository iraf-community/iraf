include <imhdr.h>
include <math.h>
include "xregister.h"

# RG_GXTRANSFORM -- Open the reference points file and the read the
# coordinates of the reference points in the reference image. Return
# the reference points file name and descriptor.

int procedure rg_gxtransform (list, xc, reffile)

int	list		#I list of reference points files
pointer	xc		#I pointer to the cross-correlation structure
char	reffile[ARB]	#O the output reference points file name

int	tdf
pointer	sp, line, pxref, pyref
real	x1, y1, x2, y2, x3, y3
int	fntgfnb(), open(), getline(), nscan()
pointer	rg_xstatp()

begin
	# Get some working memory.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)

	# Get the points to the reference point lists.
	pxref = rg_xstatp (xc, XREF)
	pyref = rg_xstatp (xc, YREF)
	call aclrr (Memr[rg_xstatp(xc, XREF)], MAX_NREF)
	call aclrr (Memr[rg_xstatp(xc, YREF)], MAX_NREF)

	# Open the reference points file and read the coordinates.
	while (fntgfnb (list, reffile, SZ_FNAME) != EOF) {

	    iferr { 

		# Open the reference file.
	        tdf = open (reffile, READ_ONLY, TEXT_FILE) 
		call aclrr (Memr[pxref], MAX_NREF)
		call aclrr (Memr[pyref], MAX_NREF)

		# Read up to three valid reference points from the list.
	        while (getline (tdf, Memc[line]) != EOF) {
		    call sscan (Memc[line])
			call gargr (x1)
			call gargr (y1)
			call gargr (x2)
			call gargr (y2)
			call gargr (x3)
			call gargr (y3)
		    if (nscan () >= 2)
			break
		}

		# Store the reference points.
		if (nscan () == 2) {
		    Memr[pxref] = x1
		    Memr[pyref] = y1
		    call rg_xseti (xc, NREFPTS, 1)
		} else if (nscan () == 4) {
		    Memr[pxref] = x1
		    Memr[pyref] = y1
		    Memr[pxref+1] = x2
		    Memr[pyref+1] = y2
		    call rg_xseti (xc, NREFPTS, 2)
		} else if (nscan () == 6) {
		    Memr[pxref] = x1
		    Memr[pyref] = y1
		    Memr[pxref+1] = x2
		    Memr[pyref+1] = y2
		    Memr[pxref+2] = x3
		    Memr[pyref+2] = y3
		    call rg_xseti (xc, NREFPTS, 2)
		} else
		    call rg_xseti (xc, NREFPTS, 0)

	    } then { 
		call rg_xseti (xc, NREFPTS, 0)
	    }
	}

	call sfree (sp)

	return (tdf)
end


# RG_ITRANSFORM -- Compute the transformation from the input image to the
# reference image interactively.

procedure rg_itransform (xc, imr, im, id)

pointer	xc		#I pointer to the cross-correlation stucture
pointer	imr		#I pointer to the reference image
pointer	im		#I pointer to the input image
pointer	id		#I pointer to the display device

int	nref, nstar, wcs, key
pointer	sp, cmd, x, y, pxref, pyref, ptrans
real	wx, wy
int	clgcur()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)
	call salloc (x, MAX_NREF, TY_REAL)
	call salloc (y, MAX_NREF, TY_REAL)
	call aclrr (Memr[x], MAX_NREF)
	call aclrr (Memr[y], MAX_NREF)

	# Get the pointers.
	pxref = rg_xstatp (xc, XREF)
	pyref = rg_xstatp (xc, YREF)
	ptrans = rg_xstatp (xc, TRANSFORM)

	# Mark up to three reference stars.
	nref = 0
	call printf ("Mark reference star %d with the image cursor [q=quit]: ")
	    call pargi (nref + 1)
	while ((nref < MAX_NREF) && clgcur ("icommands", wx, wy, wcs, key,
	    Memc[cmd], SZ_LINE) != EOF) {
	    if (key == 'q') {
		call printf ("\n")
		break
	    }
	    if (wx < 0.5 || wx > IM_LEN(imr,1) + 0.5) {
		call printf ("\n")
		next
	    }
	    if (wy < 0.5 || wy > IM_LEN(imr,2) + 0.5) {
		call printf ("\n")
		next
	    }
	    call printf ("%g  %g\n")
		call pargr (wx)
		call pargr (wy)
	    Memr[pxref+nref] = wx
	    Memr[pyref+nref] = wy
	    nref = nref + 1
	    call rg_xseti (xc, NREFPTS, nref)
	    if (nref >= MAX_NREF)
		break
	    call printf (
	        "Mark reference star %d with the image cursor [q=quit]: ")
	        call pargi (nref + 1)
	}

	# Mark the corresponding input image stars.
	if (nref > 0) {

	    nstar = 0
	    call printf ("Mark image star %d with the image cursor [q=quit]: ")
	        call pargi (nstar + 1)
	    while ((nstar < nref) && clgcur ("icommands", wx, wy, wcs, key,
	        Memc[cmd], SZ_LINE) != EOF) {
	        if (key == 'q') {
		    call printf ("\n")
		    break
		}
	        if (wx < 0.5 || wx > IM_LEN(im,1) + 0.5) {
		    call printf ("\n")
		    next
	        }
	        if (wy < 0.5 || wy > IM_LEN(im,2) + 0.5) {
		    call printf ("\n")
		    next
	        }
	        call printf ("%g  %g\n")
		    call pargr (wx)
		    call pargr (wy)
	        Memr[x+nstar] = wx
	        Memr[y+nstar] = wy
	        nstar = nstar + 1
	        if (nstar >= MAX_NREF)
		    break
	        call printf (
	            "Mark image star %d with the image cursor [q=quit]: ")
	            call pargi (nstar + 1)
	    }

	    # Compute the transformation.
	    if (nstar > 0) {
	        switch (nstar) {
	        case 0:
	            call rg_xshift (Memr[pxref], Memr[pyref], Memr[pxref],
	                Memr[pyref], Memr[ptrans])
	        case 1:
	            call rg_xshift (Memr[x], Memr[y], Memr[pxref], Memr[pyref], 
	                Memr[ptrans])
	        #case 2:
	            #call rg_xtwostar (Memr[x], Memr[y], Memr[pxref],
		        #Memr[pyref], Memr[ptrans])
	        #case 3:
	            #call rg_xthreestar (Memr[x], Memr[y], Memr[pxref],
		        #Memr[pyref], Memr[ptrans])

		default:
	            call rg_xshift (Memr[pxref], Memr[pyref], Memr[pxref],
	                Memr[pyref], Memr[ptrans])
	        }
	    }
	}

	call sfree (sp)
end


# RG_XTRANSFORM -- Compute the transformation from the input image to
# the reference image

procedure rg_xtransform (tfd, xc)

int	tfd		#I the reference points file descriptor
pointer	xc		#I the cross-correlation file descriptor

int	nref
pointer	sp, line, x, y, pxref, pyref, ptrans
int	getline(), rg_xstati(), nscan()
pointer	rg_xstatp()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (line, SZ_LINE, TY_CHAR)
	call salloc (x, MAX_NREF, TY_REAL)
	call salloc (y, MAX_NREF, TY_REAL)
	call aclrr (Memr[x], MAX_NREF)
	call aclrr (Memr[y], MAX_NREF)

	# Get the pointers to the reference image data.
	nref = rg_xstati (xc, NREFPTS)
	pxref = rg_xstatp (xc, XREF)
	pyref = rg_xstatp (xc, YREF)
	ptrans = rg_xstatp (xc, TRANSFORM)

	# Read the input image reference points.
	while ((nref > 0) && getline (tfd, Memc[line]) != EOF) {
	    call sscan (Memc[line])
		call gargr (Memr[x])
		call gargr (Memr[y])
		call gargr (Memr[x+1])
		call gargr (Memr[y+1])
		call gargr (Memr[x+2])
		call gargr (Memr[y+2])
	    if (nscan() >= 2 * nref)
		break
	}

	# Compute the transform.
	if (nscan () < 2 * nref) {
	    call rg_xshift (Memr[pxref], Memr[pyref], Memr[pxref], Memr[pyref],
	        Memr[ptrans])
	} else {
	    switch (nref) {
	    case 0:
	        call rg_xshift (Memr[pxref], Memr[pyref], Memr[pxref],
	            Memr[pyref], Memr[ptrans])
	    case 1:
	        call rg_xshift (Memr[x], Memr[y], Memr[pxref], Memr[pyref], 
	            Memr[ptrans])
	    case 2:
	        call rg_xtwostar (Memr[x], Memr[y], Memr[pxref], Memr[pyref],
		    Memr[ptrans])
	    case 3:
	        call rg_xthreestar (Memr[x], Memr[y], Memr[pxref], Memr[pyref],
		    Memr[ptrans])
	    }
	}

	call sfree (sp)
end


# RG_ETRANSFORM -- Evaulate the current transform at a single point.

procedure rg_etransform (xc, xin, yin, xout, yout)

pointer	xc		#I pointer to the cross-correlation structure
real	xin, yin	#I the input x and y values
real	xout, yout	#O the output x and y values

pointer	ptrans
pointer	rg_xstatp

begin
	ptrans = rg_xstatp (xc, TRANSFORM)
	xout = Memr[ptrans] * xin + Memr[ptrans+1] * yin + Memr[ptrans+2]
	yout = Memr[ptrans+3] * xin + Memr[ptrans+4] * yin + Memr[ptrans+5]
end


# RG_XSHIFT -- Compute the transformation coefficients required to define a
# simple shift using a single data point.

procedure rg_xshift (xref, yref, xlist, ylist, coeff)

real	xref[ARB]		#I x reference coordinates
real	yref[ARB]		#I y reference coordinates
real	xlist[ARB]		#I x input coordinates
real	ylist[ARB]		#I y input coordinates
real	coeff[ARB]		#O output coefficient array

begin
	# Compute the x transformation.
	coeff[1] = 1.0
	coeff[2] = 0.0
	coeff[3] = xref[1] - xlist[1]

	# Compute the y transformation.
	coeff[4] = 0.0
	coeff[5] = 1.0
	coeff[6] = yref[1] - ylist[1]
end


# RG_XTWOSTAR -- Compute the transformation coefficients required to
# define a simple shift, magnification which is the same in x and y,
# and rotation using two data points.

procedure rg_xtwostar (xref, yref, xlist, ylist, coeff)

real	xref[ARB]		#I x reference coordinates
real	yref[ARB]		#I y reference coordinates
real	xlist[ARB]		#I x input coordinates
real	ylist[ARB]		#I y input coordinates
real	coeff[ARB]		#O coefficient array

real	rot, mag, dxlis, dylis, dxref, dyref, cosrot, sinrot
real	rg_xposangle()

begin
	# Compute the deltas.
	dxlis = xlist[2] - xlist[1]
	dylis = ylist[2] - ylist[1]
	dxref = xref[2] - xref[1]
	dyref = yref[2] - yref[1]

	# Compute the required rotation angle.
	rot = rg_xposangle (dxref, dyref) - rg_xposangle (dxlis, dylis)
	cosrot = cos (rot)
	sinrot = sin (rot)

	# Compute the required magnification factor.
	mag = dxlis ** 2 + dylis ** 2
	if (mag <= 0.0)
	    mag = 0.0
	else
	    mag = sqrt ((dxref ** 2 + dyref ** 2) / mag)

	# Compute the transformation coefficicents.
	coeff[1] = mag * cosrot
	coeff[2] = - mag * sinrot
	coeff[3] = xref[1] - mag * cosrot * xlist[1] + mag * sinrot * ylist[1]
	coeff[4] = mag * sinrot
	coeff[5] = mag * cosrot
	coeff[6] = yref[1] - mag * sinrot * xlist[1] - mag * cosrot * ylist[1]
end


# RG_THREESTAR -- Compute the transformation coefficients required to define
# x and y shifts, x and ymagnifications, a rotation and skew, and a possible
# axis flip using three tie points.

procedure rg_xthreestar (xref, yref, xlist, ylist, coeff)

real	xref[ARB]		#I x reference coordinates
real	yref[ARB]		#I y reference coordinates
real	xlist[ARB]		#I x input coordinates
real	ylist[ARB]		#I y input coordinates
real	coeff[ARB]		#O coefficient array

real	dx23, dx13, dx12, dy23, dy13, dy12, det
bool	fp_equalr()

begin
	# Compute the deltas.
	dx23 = xlist[2] - xlist[3]
	dx13 = xlist[1] - xlist[3]
	dx12 = xlist[1] - xlist[2]
	dy23 = ylist[2] - ylist[3]
	dy13 = ylist[1] - ylist[3]
	dy12 = ylist[1] - ylist[2]

	# Compute the determinant.
	det = xlist[1] * dy23 - xlist[2] * dy13 + xlist[3] * dy12
	if (fp_equalr (det, 0.0)) {
	    call rg_xtwostar (xref, yref, xlist, ylist, coeff)
	    return
	}

	# Compute the x transformation.
	coeff[1] = (xref[1] * dy23 - xref[2] * dy13 + xref[3] * dy12) / det
	coeff[2] = (-xref[1] * dx23 + xref[2] * dx13 - xref[3] * dx12) / det
	coeff[3] = (xref[1] * (xlist[2] * ylist[3] - xlist[3] * ylist[2]) +
	    xref[2] * (ylist[1] * xlist[3] - xlist[1] * ylist[3]) +
	    xref[3] * (xlist[1] * ylist[2] - ylist[1] * xlist[2])) / det

	# Compute the y transformation.
	coeff[4] = (yref[1] * dy23 - yref[2] * dy13 + yref[3] * dy12) / det
	coeff[5] = (-yref[1] * dx23 + yref[2] * dx13 - yref[3] * dx12) / det
	coeff[6] = (yref[1] * (xlist[2] * ylist[3] - xlist[3] * ylist[2]) +
	    yref[2] * (ylist[1] * xlist[3] - xlist[1] * ylist[3]) +
	    yref[3] * (xlist[1] * ylist[2] - ylist[1] * xlist[2])) / det
end


# RG_XPOSANGLE -- Compute the position angle of a 2D vector. The angle is
# measured counter-clockwise from the positive x axis.

real procedure rg_xposangle (x, y)

real	x		#I x vector component
real	y		#I y vector component

real	theta
bool	fp_equalr()

begin
	if (fp_equalr (y, 0.0)) {
	    if (x > 0.0)
		theta = 0.0
	    else if (x < 0.0)
		theta = PI
	    else
		theta = 0.0
	} else if (fp_equalr (x, 0.0)) {
	    if (y > 0.0)
		theta = PI / 2.0
	    else if (y < 0.0)
		theta = 3.0 * PI / 2.0
	    else
		theta = 0.0
	} else if (x > 0.0 && y > 0.0) {	# 1st quadrant
	    theta = atan (y / x)
	} else if (x > 0.0 && y < 0.0) {	# 4th quadrant
	    theta = 2.0 * PI + atan (y / x)
	} else if (x < 0.0 && y > 0.0) {	# 2nd quadrant
	    theta = PI + atan (y / x)
	} else if (x < 0.0 && y < 0.0) {	# 3rd quadrant
	    theta = PI + atan (y / x)
	}

	return (theta)
end
