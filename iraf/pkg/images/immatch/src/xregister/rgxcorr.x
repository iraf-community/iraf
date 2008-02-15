include <imhdr.h>
include <math.h>
include <math/gsurfit.h>
include "xregister.h"

# RG_XCORR -- Compute the shift shift for an image relative to a reference
# image using cross-correlation techniques.

int procedure rg_xcorr (imr, im1, db, dformat, xc)

pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
pointer	db		#I pointer to the shifts database 
int	dformat		#I write shifts file in database format ?
pointer	xc		#I pointer to the cross-correlation structure

pointer	sp, image, imname
real	xshift, yshift
bool	streq()
int	rg_xstati(), fscan(), nscan()
errchk	rg_cross(), rg_xfile()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call rg_xstats (xc, IMAGE, Memc[image], SZ_FNAME)

	# Initialize.
	xshift = 0.0
	yshift = 0.0

	# Compute the average shift for the image.
	switch (rg_xstati (xc, CFUNC)) {
	case XC_DISCRETE, XC_DIFFERENCE, XC_FOURIER:

	    # Write out the parameters.
	    if (dformat == YES)
	        call rg_xdbparams (db, xc)

	    # Compute the cross-correlation function.
	    call rg_cross (imr, im1, xc, NULL, xshift, yshift)
	    call rg_xsetr (xc, TXSHIFT, xshift)
	    call rg_xsetr (xc, TYSHIFT, yshift)

	    # Write out the results for the individual regions.
	    if (dformat == YES)
	        call rg_xwreg (db, xc)

	    # Write out the total shifts.
	    if (dformat == YES)
	        call rg_xdbshift (db, xc)
	    else {
		call fprintf (db, "%s  %g  %g\n")
		    call pargstr (Memc[image])
		    call pargr (xshift)
		    call pargr (yshift)
	    }

	    # Set the x and y lags for the next picture.
	    if (rg_xstati (xc, NREFPTS) > 0) {
	        call rg_xseti (xc, XLAG, 0)
	        call rg_xseti (xc, YLAG, 0)
	    } else if (IS_INDEFI (rg_xstati (xc, DXLAG)) ||
	        IS_INDEFI (rg_xstati (xc, DYLAG))) {
	        call rg_xseti (xc, XLAG, nint (-xshift))
	        call rg_xseti (xc, YLAG, nint (-yshift))
	    } else {
	        call rg_xseti (xc, XLAG, rg_xstati (xc, XLAG) + rg_xstati (xc,
	            DXLAG))
	        call rg_xseti (xc, YLAG, rg_xstati (xc, YLAG) + rg_xstati (xc,
	            DYLAG))
	    }

	case XC_FILE:
	    if (dformat == YES)
	        call rg_xfile (db, xc, xshift, yshift)
	    else {
		if (fscan (db) != EOF) {
		    call gargwrd (Memc[imname], SZ_FNAME)
		    call gargr (xshift)
		    call gargr (yshift)
		    if (! streq (Memc[imname], Memc[image]) || nscan() != 3) {
			xshift = 0.0
			yshift = 0.0
		    }
		} else {
		    xshift = 0.0
		    yshift = 0.0
		}
	    }
	    call rg_xsetr (xc, TXSHIFT, xshift)
	    call rg_xsetr (xc, TYSHIFT, yshift)

	default:
	    call error (0, "The correlation function is undefined.")
	}

	call sfree (sp)

	return (NO)
end


# RG_CROSS -- Compute the cross-correlation function for all the regions
# using discrete, fourier, or difference techniques and compute the position
# of its peak using one of several centering algorithms.

procedure rg_cross (imr, im1, xc, gd, xavshift, yavshift)

pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
pointer	xc		#I pointer to the cross correlation structure
pointer	gd		#I pointer to graphics stream
real	xavshift	#O x coord shift
real	yavshift	#O y coord shift

int	i, nregions, ngood
pointer	pxshift, pyshift
real	xshift, yshift
int	rg_xstati(), rg_xcget(), rg_xfget()
pointer	rg_xstatp()

begin
	# Get the pointers.
	pxshift = rg_xstatp (xc, XSHIFTS)
	pyshift = rg_xstatp (xc, YSHIFTS)
	nregions = rg_xstati (xc, NREGIONS)

	# Loop over the regions.
	xavshift = 0.0
	yavshift = 0.0
	ngood = 0
	do i = 1, nregions {

	    # Compute the cross_correlation function.
	    switch (rg_xstati (xc, CFUNC)) {
	    case XC_DISCRETE, XC_DIFFERENCE:
	        if (rg_xcget (xc, imr, im1, i) == ERR) {
		    Memr[pxshift+i-1] = INDEFR
		    Memr[pyshift+i-1] = INDEFR
		    if (rg_xstatp (xc, XCOR) != NULL)
		        call mfree (rg_xstatp (xc, XCOR), TY_REAL)
		    call rg_xsetp (xc, XCOR, NULL)
		    next
	        }
	    case XC_FOURIER:
	        if (rg_xfget (xc, imr, im1, i) == ERR) {
		    Memr[pxshift+i-1] = INDEFR
		    Memr[pyshift+i-1] = INDEFR
		    if (rg_xstatp (xc, XCOR) != NULL)
		        call mfree (rg_xstatp (xc, XCOR), TY_REAL)
		    call rg_xsetp (xc, XCOR, NULL)
		    next
	        }
	    default:
		call error (0, "The correlation function is undefined")
	    }

	    # Find the peak of the cross-correlation function.
	    call rg_fit (xc, i, gd, xshift, yshift)

	    # Accumulate the shifts.
	    xavshift = xavshift + xshift
	    yavshift = yavshift + yshift
	    ngood = ngood + 1
	}

	# Compute the average shift.
	if (ngood > 0) {
	    xavshift = xavshift / ngood
	    yavshift = yavshift / ngood
	}
end


# RG_XFILE -- Read the average x and y shifts from the shifts database.

procedure rg_xfile (db, xc, xshift, yshift)

pointer	db		#I pointer to the database
pointer	xc		#I pointer to the cross correlation structure
real	xshift		#O shift in x
real	yshift		#O shift in y

int	rec
pointer	sp, str
int	dtlocate()
real	dtgetr()
errchk	dtlocate(), dtgetr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call rg_xstats (xc, RECORD, Memc[str], SZ_LINE)
	iferr {
	    rec = dtlocate (db, Memc[str])
	    xshift = dtgetr (db, rec, "xshift")
	    yshift = dtgetr (db, rec, "yshift")
	} then {
	    xshift = 0.0
	    yshift = 0.0
	}
	
	call sfree (sp)
end


# RG_ICROSS -- Compute the cross-correlation function for a given region.

int procedure rg_icross (xc, imr, im1, nreg)

pointer	xc		#I pointer to the cross-correlation structure
pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
int	nreg		#I the index of the current region

int	stat
pointer	pxshift, pyshift
int	rg_xstati(), rg_xcget(), rg_xfget()
pointer	rg_xstatp()

begin
	pxshift = rg_xstatp (xc, XSHIFTS)
	pyshift = rg_xstatp (xc, YSHIFTS)

	switch (rg_xstati (xc, CFUNC)) {
	case XC_DISCRETE, XC_DIFFERENCE:
	    stat = rg_xcget (xc, imr, im1, nreg)
	    if (stat == ERR) {
		Memr[pxshift+nreg-1] = INDEFR
		Memr[pyshift+nreg-1] = INDEFR
		if (rg_xstatp (xc, XCOR) != NULL)
		    call mfree (rg_xstatp (xc, XCOR), TY_REAL)
		call rg_xsetp (xc, XCOR, NULL)
	    }
	case XC_FOURIER:
	    stat = rg_xfget (xc, imr, im1, nreg)
	    if (stat == ERR) {
		Memr[pxshift+nreg-1] = INDEFR
		Memr[pyshift+nreg-1] = INDEFR
		if (rg_xstatp (xc, XCOR) != NULL)
		    call mfree (rg_xstatp (xc, XCOR), TY_REAL)
		call rg_xsetp (xc, XCOR, NULL)
	    }
	case XC_FILE:
	    stat = OK
	}

	return (stat)
end


# RG_XCGET -- Compute the convolution using the discrete or difference
# correlation functions.

int procedure rg_xcget (xc, imr, im1, i)

pointer	xc		#I pointer to the cross-correlation structure
pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to input image image
int	i		#I index of region

int	stat, xwindow, ywindow, nrimcols, nrimlines, nimcols, nimlines
int	nrcols, nrlines, ncols, nlines
int	xlag, ylag, nborder, rc1, rc2, rl1, rl2, c1, c2, l1, l2
pointer	sp, str, coeff, rbuf, ibuf, xcor
pointer	prc1, prc2, prl1, prl2, przero, prxslope, pryslope, border
real	rxlag, rylag
int	rg_xstati(), rg_border()
pointer	rg_xstatp(), rg_ximget()
real	rg_xstatr()

define	nextregion_	10

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (coeff, max (GS_SAVECOEFF + 6, 9), TY_REAL)
	rbuf = NULL
	ibuf = NULL

	# Check for regions.
	if (i > rg_xstati (xc, NREGIONS)) {
	    stat = ERR
	    goto nextregion_
	}

	# Get the image sizes.
	nrimcols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    nrimlines = 1
	else
	    nrimlines = IM_LEN(imr,2)
	nimcols = IM_LEN(im1,1)
	if (IM_NDIM(im1) == 1)
	    nimlines = 1
	else
	    nimlines = IM_LEN(im1,2)

	# Get the reference region pointers.
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)
	przero = rg_xstatp (xc, RZERO)
	prxslope = rg_xstatp (xc, RXSLOPE)
	pryslope = rg_xstatp (xc, RYSLOPE)

	# Compute the reference region limits.
	rc1 = max (1, min (int (nrimcols), Memi[prc1+i-1]))
	rc2 = min (int (nrimcols), max (1, Memi[prc2+i-1]))
	rl1 = max (1, min (int (nrimlines), Memi[prl1+i-1]))
	rl2 = min (int (nrimlines), max (1, Memi[prl2+i-1]))
	nrcols = rc2 - rc1 + 1
	nrlines = rl2 - rl1 + 1

	# Move to the next reference region if current region is off the image.
	if (rc1 > nrimcols || rc2 < 1 || rl1 > nrimlines || rl2 < 1) {
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Reference section: %s[%d:%d,%d:%d] is off image.\n")
		call pargstr (Memc[str])
		call pargi (rc1)
		call pargi (rc2)
		call pargi (rl1)
		call pargi (rl2)
	    stat = ERR
	    goto nextregion_
	}

	# Check the window sizes.
	xwindow = rg_xstati (xc, XWINDOW)
	if (nrlines == 1)
	    ywindow = 1
	else
	    ywindow = rg_xstati (xc, YWINDOW)

	# Move to next ref regions if current region is too small.
	if (nrcols < xwindow || (IM_NDIM(imr) == 2 && nrlines < ywindow)) {
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_LINE)
	    call eprintf (
	    "Reference section: %s[%d:%d,%d:%d] has too few points.\n")
		call pargstr (Memc[str])
		call pargi (rc1)
		call pargi (rc2)
		call pargi (rl1)
		call pargi (rl2)
	    stat = ERR
	    goto nextregion_
	}

	# Apply the transformation if defined or lag to the ref regions.
	if (rg_xstati (xc, NREFPTS) > 0) {
	    call rg_etransform (xc, (rc1 + rc2) / 2.0, (rl1 + rl2) / 2.0,
		rxlag, rylag)
	    xlag = rxlag - (rc1 + rc2) / 2.0
	    if (ywindow == 1)
	        ylag = 0
	    else
	        ylag = rylag - (rl1 + rl2) / 2.0
	} else {
	    xlag = rg_xstati (xc, XLAG)
	    if (ywindow == 1)
	        ylag = 0
	    else
	        ylag = rg_xstati (xc, YLAG)
	}

	# Get the input image limits.
	c1 = rc1 + xlag - xwindow / 2
	c2 = rc2 + xlag + xwindow / 2
	l1 = rl1 + ylag - ywindow / 2
	l2 = rl2 + ylag + ywindow / 2
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Move to the next ref region if input region is off image.
	if (c1 > nimcols || c2 < 1 || l1 > nimlines || l2 < 1) {
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Image section: %s[%d:%d,%d:%d] is off image.\n")
		call pargstr (Memc[str])
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    stat = ERR
	    goto nextregion_
	}

	# Move to the next ref region if input region is less than 3 by 3.
	if ((ncols < xwindow) || (IM_NDIM(im1) == 2 && nlines < ywindow)) {
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Image section: %s[%d:%d,%d:%d] has too few points.\n")
		call pargstr (Memc[str])
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    stat = ERR
	    goto nextregion_
	}

	# Get the input reference and input image data.
	rbuf = rg_ximget (imr, rc1, rc2, rl1, rl2)
	if (rbuf == NULL) {
	    stat = ERR
	    goto nextregion_
	}
	ibuf = rg_ximget (im1, c1, c2, l1, l2)
	if (ibuf == NULL) {
	    stat = ERR
	    goto nextregion_
	}

	# Do the background subtraction.

	# Compute the zero point, x slope and y slope of ref image.
	if (IS_INDEFR(Memr[przero+i-1]) || IS_INDEFR(Memr[prxslope+i- 1]) ||
	    IS_INDEFR(Memr[pryslope+i-1])) {
	    if (IS_INDEFI(rg_xstati (xc, BORDER))) {
		call rg_xscale (xc, Memr[rbuf], nrcols * nrlines, nrcols,
		    nrlines, rg_xstatr (xc, BVALUER), Memr[coeff])
	    } else {
		    border = NULL
		    nborder = rg_border (Memr[rbuf], nrcols, nrlines,
		        max (0, nrcols - 2 * rg_xstati (xc, BORDER)),
		        max (0, nrlines - 2 * rg_xstati (xc, BORDER)),
			border)
		    call rg_xscale (xc, Memr[border], nborder, nrcols,
			nrlines, rg_xstatr (xc, BVALUER), Memr[coeff])
		    if (border != NULL)
		        call mfree (border, TY_REAL)
	    }

	    # Save the coefficients.
	    Memr[przero+i-1] = Memr[coeff]
	    Memr[prxslope+i-1] = Memr[coeff+1]
	    Memr[pryslope+i-1] = Memr[coeff+2]
	}

	call rg_subtract (Memr[rbuf], nrcols, nrlines, Memr[przero+i-1],
	    Memr[prxslope+i-1], Memr[pryslope+i-1])

	# Compute the zero point, and the x and y slopes of input image.
	if (IS_INDEFI(rg_xstati (xc, BORDER))) {
	    call rg_xscale (xc, Memr[ibuf], ncols * nlines, ncols,
		nlines, rg_xstatr (xc, BVALUE), Memr[coeff])
	} else {
	    border = NULL
	    nborder = rg_border (Memr[ibuf], ncols, nlines,
	        max (0, ncols - 2 * rg_xstati (xc, BORDER)),
	        max (0, nlines - 2 * rg_xstati (xc, BORDER)),
		border)
	    call rg_xscale (xc, Memr[border], nborder, ncols, nlines,
		rg_xstatr (xc, BVALUE), Memr[coeff])
	    if (border != NULL)
		call mfree (border, TY_REAL)
	}

	# Subtract the baseline.
	call rg_subtract (Memr[ibuf], ncols, nlines, Memr[coeff],
	    Memr[coeff+1], Memr[coeff+2])

	# Apodize the data.
	if (rg_xstatr (xc, APODIZE) > 0.0) {
	    call rg_apodize (Memr[rbuf], nrcols, nrlines, rg_xstatr (xc,
		APODIZE), YES)
	    call rg_apodize (Memr[ibuf], ncols, nlines, rg_xstatr (xc,
	        APODIZE), YES)
	}

	# Spatially filter the data with a Laplacian.
	switch (rg_xstati (xc, FILTER)) {
	case XC_LAPLACE:
	    call rg_xlaplace (Memr[rbuf], nrcols, nrlines, 1.0)
	    call rg_xlaplace (Memr[ibuf], ncols, nlines, 1.0)
	default:
	    ;
	}

	# Allocate space for the cross-correlation function.
	if (rg_xstatp (xc, XCOR) == NULL) {
	    call malloc (xcor, xwindow * ywindow, TY_REAL)
	    call rg_xsetp (xc, XCOR, xcor)
	} else {
	    xcor = rg_xstatp (xc, XCOR)
	    call realloc (xcor, xwindow * ywindow, TY_REAL)
	    call rg_xsetp (xc, XCOR, xcor)
	}

	# Clear the correlation function.
	call aclrr (Memr[xcor], xwindow * ywindow)

	# Compute the cross-correlation function.
	if (rg_xstati (xc, CFUNC) == XC_DISCRETE) {
	    call rg_xconv (Memr[rbuf], nrcols, nrlines, Memr[ibuf], ncols,
	        nlines, Memr[xcor], xwindow, ywindow)
	} else {
	    call rg_xdiff (Memr[rbuf], nrcols, nrlines, Memr[ibuf], ncols,
	        nlines, Memr[xcor], xwindow, ywindow)
	}

	stat = OK

nextregion_

	# Free memory.
	call sfree (sp)
	if (rbuf != NULL)
	    call mfree (rbuf, TY_REAL)
	if (ibuf != NULL)
	    call mfree (ibuf, TY_REAL)
	if (stat == ERR)
	    return (ERR)
	else
	    return (OK)
end


# RG_XFGET -- Compute the cross-correlation function using Fourier techniques.

int procedure rg_xfget (xc, imr, im1, i)

pointer	xc		#I pointer to the cross-correlation structure
pointer	imr		#I pointer to the reference image
pointer	im1		#I pointer to the input image
int	i		#I index of the current region

int	rc1, rc2, rl1, rl2, nrcols, nrlines, c1, c2, l1, l2, ncols, nlines
int	nrimcols, nrimlines, nimcols, nimlines
int	xwindow, ywindow, xlag, nxfft, nyfft, ylag, stat, nborder
pointer	sp, str, coeff, xcor, rbuf, ibuf, fft, border
pointer	prc1, prc2, prl1, prl2, przero, prxslope, pryslope
real	rxlag, rylag
int	rg_xstati(), rg_border(), rg_szfft()
pointer	rg_xstatp(), rg_ximget()
real	rg_xstatr()

define	nextregion_	11

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (coeff, max (GS_SAVECOEFF+6, 9), TY_REAL)

	# Check for number of regions.
	if (i > rg_xstati (xc, NREGIONS)) {
	    stat = ERR
	    goto nextregion_
	}

	# Allocate space for the cross-correlation function.
	nrimcols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    nrimlines = 1
	else
	    nrimlines = IM_LEN(imr,2)
	nimcols = IM_LEN(im1,1)
	if (IM_NDIM(im1) == 1)
	    nimlines = 1
	else
	    nimlines = IM_LEN(im1,2)

	# Get the regions pointers.
	prc1 = rg_xstatp (xc, RC1)
	prc2 = rg_xstatp (xc, RC2)
	prl1 = rg_xstatp (xc, RL1)
	prl2 = rg_xstatp (xc, RL2)
	przero = rg_xstatp (xc, RZERO)
	prxslope = rg_xstatp (xc, RXSLOPE)
	pryslope = rg_xstatp (xc, RYSLOPE)

	# Get the reference subraster region.
	rc1 = max (1, min (int (nrimcols), Memi[prc1+i-1]))
	rc2 = min (int (nrimcols), max (1, Memi[prc2+i-1]))
	rl1 = max (1, min (int (nrimlines), Memi[prl1+i-1]))
	rl2 = min (int (nrimlines), max (1, Memi[prl2+i-1]))
	nrcols = rc2 - rc1 + 1
	nrlines = rl2 - rl1 + 1

	# Go to next region if the reference region is off the image.
	if (rc1 > nrimcols || rc2 < 1 || rl1 > nrimlines || rl2 < 1) {
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Reference section: %s[%d:%d,%d:%d] is off image.\n")
		call pargstr (Memc[str])
		call pargi (rc1)
		call pargi (rc2)
		call pargi (rl1)
		call pargi (rl2)
	    stat = ERR
	    goto nextregion_
	}

	# Check the window sizes.
	xwindow = rg_xstati (xc, XWINDOW)
	if (nrlines == 1)
	    ywindow = 1
	else
	    ywindow = rg_xstati (xc, YWINDOW)

	# Go to the next region if the reference region has too few points.
	if ((nrcols < xwindow) || (IM_NDIM(im1) == 2 && nrlines < ywindow)) {
	    call rg_xstats (xc, REFIMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Reference section: %s[%d:%d,%d:%d] has too few points.\n")
		call pargstr (Memc[str])
		call pargi (rc1)
		call pargi (rc2)
		call pargi (rl1)
		call pargi (rl2)
	    stat = ERR
	    goto nextregion_
	}

	# Apply the transformation if defined or the lag.
	if (rg_xstati (xc, NREFPTS) > 0) {
	    call rg_etransform (xc, (rc1 + rc2) / 2.0, (rl1 + rl2) / 2.0,
		rxlag, rylag)
	    xlag = rxlag - (rc1 + rc2) / 2.0
	    if (ywindow == 1)
		ylag = 0
	    else
	        ylag = rylag - (rl1 + rl2) / 2.0
	} else {
	    xlag = rg_xstati (xc, XLAG)
	    if (ywindow == 1)
		ylag = 0
	    else
	        ylag = rg_xstati (xc, YLAG)
	}

	# Get the input image subraster regions.
	c1 = rc1 + xlag
	c2 = rc2 + xlag
	l1 = rl1 + ylag
	l2 = rl2 + ylag
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1

	# Go to next region if region is off the image.
	if (c1 > nimcols || c2 < 1 || l1 > nimlines || l2 < 1) {
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Image section: %s[%d:%d,%d:%d] is off image.\n")
		call pargstr (Memc[str])
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    stat = ERR
	    goto nextregion_
	}

	# Go to next region if region has too few points.
	if ((ncols < xwindow) || (IM_NDIM(im1) == 2 && nlines < ywindow)) {
	    call rg_xstats (xc, IMAGE, Memc[str], SZ_LINE)
	    call eprintf (
		"Image section: %s[%d:%d,%d:%d] has too few points.\n")
		call pargstr (Memc[str])
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    stat = ERR
	    goto nextregion_
	}

	# Figure out how big the Fourier transform has to be, given
	# the size of the reference subraster, the window size and
	# the fact that the FFT must be a power of 2.

	nxfft = rg_szfft (nrcols, xwindow)
	if (ywindow == 1)
	    nyfft = 1
	else
	    nyfft = rg_szfft (nrlines, ywindow)
	call calloc (fft, 2 * nxfft * nyfft, TY_REAL)

	# Get the input reference and input image data.
	rbuf = NULL
	rbuf = rg_ximget (imr, rc1, rc2, rl1, rl2)
	if (rbuf == NULL) {
	    stat = ERR
	    goto nextregion_
	}

	# Do the background subtraction.

	# Compute the zero point, x slope and y slope of ref image.
	if (IS_INDEFR(Memr[przero+i-1]) || IS_INDEFR(Memr[prxslope+i- 1]) ||
	    IS_INDEFR(Memr[pryslope+i-1])) {
	    if (IS_INDEFI(rg_xstati (xc, BORDER))) {
		call rg_xscale (xc, Memr[rbuf], nrcols * nrlines, nrcols,
		    nrlines, rg_xstatr (xc, BVALUER), Memr[coeff])
	    } else {
		    border = NULL
		    nborder = rg_border (Memr[rbuf], nrcols, nrlines,
		        max (0, nrcols - 2 * rg_xstati (xc, BORDER)),
		        max (0, nrlines - 2 * rg_xstati (xc, BORDER)),
			border)
		    call rg_xscale (xc, Memr[border], nborder, nrcols,
			nrlines, rg_xstatr (xc, BVALUER), Memr[coeff])
		    if (border != NULL)
		        call mfree (border, TY_REAL)
	    }

	    # Save the coefficients.
	    Memr[przero+i-1] = Memr[coeff]
	    Memr[prxslope+i-1] = Memr[coeff+1]
	    Memr[pryslope+i-1] = Memr[coeff+2]
	}

	call rg_subtract (Memr[rbuf], nrcols, nrlines, Memr[przero+i-1],
	    Memr[prxslope+i-1], Memr[pryslope+i-1])

	# Apodize the data.
	if (rg_xstatr (xc, APODIZE) > 0.0)
	    call rg_apodize (Memr[rbuf], nrcols, nrlines, rg_xstatr (xc,
		APODIZE), YES)

	# Spatially filter the data with a Laplacian.
	switch (rg_xstati (xc, FILTER)) {
	case XC_LAPLACE:
	    call rg_xlaplace (Memr[rbuf], nrcols, nrlines, 1.0)
	default:
	    ;
	}

	# Load the reference data into the  FFT.
	call rg_rload (Memr[rbuf], nrcols, nrlines, Memr[fft], nxfft, nyfft)
	call mfree (rbuf, TY_REAL)

	ibuf = NULL
	ibuf = rg_ximget (im1, c1, c2, l1, l2)
	if (ibuf == NULL) {
	    stat = ERR
	    goto nextregion_
	}

	# Compute the zero point, and the x and y slopes of input image.
	if (IS_INDEFI(rg_xstati (xc, BORDER))) {
	    call rg_xscale (xc, Memr[ibuf], ncols * nlines, ncols,
		nlines, rg_xstatr (xc, BVALUE), Memr[coeff])
	} else {
	    border = NULL
	    nborder = rg_border (Memr[ibuf], ncols, nlines,
	        max (0, ncols - 2 * rg_xstati (xc, BORDER)),
	        max (0, nlines - 2 * rg_xstati (xc, BORDER)),
		border)
	    call rg_xscale (xc, Memr[border], nborder, ncols, nlines,
		rg_xstatr (xc, BVALUE), Memr[coeff])
	    if (border != NULL)
		call mfree (border, TY_REAL)
	}

	# Subtract the baseline.
	call rg_subtract (Memr[ibuf], ncols, nlines, Memr[coeff],
	    Memr[coeff+1], Memr[coeff+2])

	# Apodize the data.
	if (rg_xstatr (xc, APODIZE) > 0.0)
	    call rg_apodize (Memr[ibuf], ncols, nlines, rg_xstatr (xc,
	        APODIZE), YES)

	# Spatially filter the data with a Laplacian.
	switch (rg_xstati (xc, FILTER)) {
	case XC_LAPLACE:
	    call rg_xlaplace (Memr[ibuf], ncols, nlines, 1.0)
	default:
	    ;
	}

	# Load the image data into the FFT.
	call rg_iload (Memr[ibuf], ncols, nlines, Memr[fft], nxfft, nyfft)
	call mfree (ibuf, TY_REAL)

	# Normalize the data.
	call rg_fnorm (Memr[fft], nrcols, nrlines, nxfft, nyfft)

	# Compute the cross-correlation function.
	call rg_fftcor (Memr[fft], nxfft, nyfft)

	# Allocate space for the correlation function.
	if (rg_xstatp (xc, XCOR) == NULL) {
	    call malloc (xcor, xwindow * ywindow, TY_REAL)
	    call rg_xsetp (xc, XCOR, xcor)
	} else {
	    xcor = rg_xstatp (xc, XCOR)
	    call realloc (xcor, xwindow * ywindow, TY_REAL)
	    call rg_xsetp (xc, XCOR, xcor)
	}

	# Move the valid lags into the crosscorrelation array
	call rg_movexr (Memr[fft], nxfft, nyfft, Memr[xcor], xwindow, ywindow)

	# Free space.
	call mfree (fft, TY_REAL)

	stat = OK

nextregion_

	call sfree (sp)
	if (stat == ERR)
	    return (ERR)
	else
	    return (OK)
end


# RG_XIMGET -- Fill a buffer from a specified region of the image.

pointer procedure rg_ximget (im, c1, c2, l1, l2)

pointer	im		#I pointer to the iraf image
int	c1, c2		#I column limits in the input image
int	l1, l2		#I line limits in the input image

int	i, ncols, nlines, npts
pointer	ptr, index, buf
pointer	imgs1r(), imgs2r()

begin
	ncols = c2 - c1 + 1
	nlines = l2 - l1 + 1
	npts = ncols * nlines
	call malloc (ptr, npts, TY_REAL)

	index = ptr
	do i = l1, l2 {
	    if (IM_NDIM(im) == 1)
	        buf = imgs1r (im, c1, c2)
	    else
	        buf = imgs2r (im, c1, c2, i, i)
	    call amovr (Memr[buf], Memr[index], ncols)
	    index = index + ncols
	}

	return (ptr)
end


# RG_XLAPLACE -- Compute the Laplacian of an image subraster in place.

procedure rg_xlaplace (data, nx, ny, rho)

real	data[nx,ARB]		#I the input array
int	nx, ny			#I the size of the input/output data array
real	rho			#I the pixel to pixel correlation factor

int	i, inline, outline, nxk, nyk, nxc
pointer	sp, lineptrs, ptr
real	rhosq, kernel[3,3]
data	nxk /3/, nyk /3/

begin
	# Define the kernel.
	rhosq = rho * rho
	kernel[1,1] = rhosq
	kernel[2,1] = -rho * (1.0 + rhosq)
	kernel[3,1] = rhosq
	kernel[1,2] = -rho * (1.0 + rhosq)
	kernel[2,2] = (1.0 + rhosq) * (1 + rhosq)
	kernel[3,2] = -rho * (1.0 + rhosq)
	kernel[1,3] = rhosq
	kernel[2,3] = -rho * (1.0 + rhosq)
	kernel[3,3] = rhosq

	# Set up an array of line pointers.
	call smark (sp)
	call salloc (lineptrs, nyk, TY_POINTER)

	# Allocate working space.
	nxc = nx + 2 * (nxk / 2)
	do i = 1, nyk
	    call salloc (Memi[lineptrs+i-1], nxc, TY_REAL) 

	inline = 1 - nyk / 2
	do i = 1, nyk - 1 {
	    if (inline < 1) {
	        call amovr (data[1,1], Memr[Memi[lineptrs+i]+nxk/2], nx)
	        Memr[Memi[lineptrs+i]] = data[1,1]
	        Memr[Memi[lineptrs+i]+nxc-1] = data[nx,1]
	    } else {
	        call amovr (data[1,i-1], Memr[Memi[lineptrs+i]+nxk/2], nx)
	        Memr[Memi[lineptrs+i]] = data[1,i-1]
	        Memr[Memi[lineptrs+i]+nxc-1] = data[nx,i-1]
	    }
	    inline = inline + 1
	}

	# Generate the output image line by line
	do outline = 1, ny {

	    # Scroll the input buffers
	    ptr = Memi[lineptrs] 
	    do i = 1, nyk - 1
		Memi[lineptrs+i-1] = Memi[lineptrs+i]
	    Memi[lineptrs+nyk-1] = ptr 

	    # Read in new image line
	    if (inline > ny) {
		call amovr (data[1,ny], Memr[Memi[lineptrs+nyk-1]+nxk/2],
		    nx)
	        Memr[Memi[lineptrs+nyk-1]] = data[1,ny]
	        Memr[Memi[lineptrs+nyk-1]+nxc-1] = data[nx,ny]
	    } else {
		call amovr (data[1,inline], Memr[Memi[lineptrs+nyk-1]+nxk/2],
		    nx)
	        Memr[Memi[lineptrs+nyk-1]] = data[1,inline]
	        Memr[Memi[lineptrs+nyk-1]+nxc-1] = data[nx,inline]
	    }

	    # Generate output image line
	    call aclrr (data[1,outline], nx)
	    do i = 1, nyk
		call acnvr (Memr[Memi[lineptrs+i-1]], data[1,outline], nx,
		    kernel[1,i], nxk)

	    inline = inline + 1
	}

	# Free the image buffer pointers
	call sfree (sp)
end


# RG_XCONV -- Compute the cross-correlation function directly in the spatial
# domain. 

procedure rg_xconv (ref, nrcols, nrlines, image, ncols, nlines, xcor, xwindow,
	ywindow)

real	ref[nrcols,nrlines]	#I the input reference subraster
int	nrcols, nrlines		#I size of the reference subraster
real	image[ncols,nlines]	#I the input image subraster
int	ncols, nlines		#I size of the image subraster
real	xcor[xwindow,ywindow]	#O the output cross-correlation function
int	xwindow, ywindow	#I size of the cross-correlation function

int	lagx, lagy, i, j
real	meanr, facr, meani, faci, sum
real	asumr()
#real	cxmin, cxmax

begin
	meanr = asumr (ref, nrcols * nrlines) / (nrcols * nrlines)
	facr = 0.0
	do j = 1, nrlines {
	    do i = 1, nrcols
		facr = facr + (ref[i,j] - meanr) ** 2
	}
	if (facr <= 0.0)
	    facr = 1.0
	else
	    facr = sqrt (facr)

	do lagy = 1, ywindow {
	    do lagx = 1, xwindow {
		meani = 0.0
		do j = 1, nrlines {
		    do i = 1, nrcols 
			meani = meani + image[i+lagx-1,j+lagy-1]
		}
		meani = meani / (nrcols * nrlines)
		faci = 0.0
		sum = 0.0
		do j = 1, nrlines {
		    do i = 1, nrcols {
			faci = faci + (image[i+lagx-1,j+lagy-1] - meani) ** 2
			sum = sum + (ref[i,j] - meanr)  *
			    (image[i+lagx-1,j+lagy-1] - meani)
		    }
		}
		if (faci <= 0.0)
		    faci = 1.0
		else
		    faci = sqrt (faci)
		xcor[lagx,lagy] = sum / facr / faci
	    }
	}
end


# RG_XDIFF -- Compute the error function at each of several templates.

procedure rg_xdiff (ref, nrcols, nrlines, image, ncols, nlines, xcor, xwindow,
	ywindow)

real	ref[nrcols,nrlines]	#I reference subraste
int	nrcols, nrlines		#I size of the reference subraster
real	image[ncols,nlines]	#I image subraster
int	ncols, nlines		#I size of image subraster
real	xcor[xwindow,ywindow]	#O crosscorrelation function
int	xwindow, ywindow	#I size of correlation function

int	lagx, lagy, i, j
real	meanr, meani, sum, cormin, cormax
real	asumr()


begin
	meanr = asumr (ref, nrcols * nrlines) / (nrcols * nrlines)
	do lagy = 1, ywindow {
	    do lagx = 1, xwindow {
		meani = 0.0
		do j = 1, nrlines {
		    do i = 1, nrcols
			meani = meani + image[i+lagx-1,j+lagy-1]
		}
		meani = meani / (nrcols * nrlines)
		sum = 0.0
		do j = 1, nrlines {
		    do i = 1, nrcols {
			sum = sum + abs ((ref[i,j] - meanr) -
			    (image[i+lagx-1,j+lagy-1] - meani))
		    }
		}
		xcor[lagx,lagy] = sum 
	    }
	}

	call alimr (xcor, xwindow * ywindow, cormin, cormax)
	call adivkr (xcor, cormax, xcor, xwindow * ywindow)
	call asubkr (xcor, 1.0, xcor, xwindow * ywindow)
	call anegr (xcor, xcor, xwindow * ywindow)
end

