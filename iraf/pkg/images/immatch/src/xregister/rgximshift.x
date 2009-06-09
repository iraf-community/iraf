include <imhdr.h>
include <imset.h>
include <math/iminterp.h>

define	NYOUT		16	# number of lines output at once
define	NMARGIN		3	# number of boundary pixels required	
define	NMARGIN_SPLINE3	16	# number of spline boundary pixels required


# RG_XSHIFTIM - Shift a 1 or 2D image by a fractional pixel amount
# x and y

procedure rg_xshiftim (im1, im2, xshift, yshift, interpstr, boundary_type,
        constant)

pointer		im1		#I pointer to input image
pointer		im2		#I pointer to output image
real		xshift		#I shift in x direction
real		yshift		#I shift in y direction
char		interpstr[ARB]	#I type of interpolant
int		boundary_type	#I type of boundary extension
real		constant	#I value of constant for boundary extension

size_t	sz_val
int	interp_type
pointer	sp, str
bool	fp_equalr()
int	strdic()
long	lnint(), lint()

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (str, sz_val, TY_CHAR)
	interp_type = strdic (interpstr, Memc[str], SZ_FNAME, II_BFUNCTIONS)

	if (interp_type == II_NEAREST)
	    call rg_xishiftim (im1, im2, lnint(xshift), lnint(yshift),
	        interp_type, boundary_type, constant)
	else if (fp_equalr (xshift, aint(xshift)) &&
		 fp_equalr (yshift, aint(xshift)))
	    call rg_xishiftim (im1, im2, lint(xshift), lint(yshift),
	        interp_type, boundary_type, constant)
	else
	    call rg_xfshiftim (im1, im2, xshift, yshift, interpstr,
		boundary_type, constant)
	call sfree (sp)
end


# RG_XISHIFTIM -- Shift a 2-D image by integral pixels in x and y.

procedure rg_xishiftim (im1, im2, nxshift, nyshift, interp_type, boundary_type,
	constant)

pointer	im1			#I pointer to the input image
pointer	im2			#I pointer to the output image
long	nxshift, nyshift	#I shift in x and y
int	interp_type		#I type of interpolant
int	boundary_type		#I type of boundary extension
real	constant		#I constant for boundary extension

size_t	sz_val
long	l_val
long	ixshift, iyshift
pointer	buf1, buf2
long	v[IM_MAXDIM]
size_t	ncols, nlines
long	i, x1col, x2col, yline, nbpix

long	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
long	labs(), lmod()
pointer	imgs2s(), imgs2i(), imgs2l(), imgs2r(), imgs2d(), imgs2x()
errchk	impnls, impnli, impnll, impnlr, impnld, impnlx
errchk	imgs2s, imgs2i, imgs2l, imgs2r, imgs2d, imgs2x
string	wrerr "ISHIFTXY: Error writing in image."

begin
	ixshift = nxshift
	iyshift = nyshift

	ncols = IM_LEN(im1,1)
	nlines = IM_LEN(im1,2)

	# Cannot shift off image.
	if (ixshift < -ncols || ixshift > ncols)
	    call error (3, "ISHIFTXY: X shift out of bounds.")
	if (iyshift < -nlines || iyshift > nlines)
	    call error (4, "ISHIFTXY: Y shift out of bounds.")

	# Calculate the shift.
	switch (boundary_type) {
	case BT_CONSTANT,BT_REFLECT,BT_NEAREST:
	    ixshift = min (ncols, max (-ncols, ixshift))
	    iyshift = min (nlines, max (-nlines, iyshift))
	case BT_WRAP:
	    ixshift = lmod(ixshift, ncols)
	    iyshift = lmod(iyshift, nlines)
	}

	# Set the boundary extension values.
	nbpix = max (labs(ixshift), labs(iyshift))
	call imsetl (im1, IM_NBNDRYPIX, nbpix)
	call imseti (im1, IM_TYBNDRY, boundary_type)
	if (boundary_type == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Get column boundaries in the input image.
	x1col = max (-ncols + 1, - ixshift + 1) 
	x2col = min (2 * ncols,  ncols - ixshift)

	l_val = 1
	sz_val = IM_MAXDIM
	call amovkl (l_val, v, sz_val)

	# Shift the image using the appropriate data type operators.
	switch (IM_PIXTYPE(im1)) {
	case TY_SHORT:
	    do i = 1, nlines {
	        if (impnls (im2, buf2, v) == EOF)
		    call error (5, wrerr)
		yline = i - iyshift
		buf1 = imgs2s (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (5, wrerr)
		call amovs (Mems[buf1], Mems[buf2], ncols)
	    }
	case TY_USHORT, TY_INT:
	    do i = 1, nlines {
	        if (impnli (im2, buf2, v) == EOF)
		    call error (5, wrerr)
		yline = i - iyshift
		buf1 = imgs2i (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (5, wrerr)
		call amovi (Memi[buf1], Memi[buf2], ncols)
	    }
	case TY_LONG:
	    do i = 1, nlines {
	        if (impnll (im2, buf2, v) == EOF)
		    call error (5, wrerr)
		yline = i - iyshift
		buf1 = imgs2l (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (5, wrerr)
		call amovl (Meml[buf1], Meml[buf2], ncols)
	    }
	case TY_REAL:
	    do i = 1, nlines {
	        if (impnlr (im2, buf2, v) == EOF)
		    call error (5, wrerr)
		yline = i - iyshift
		buf1 = imgs2r (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (5, wrerr)
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	case TY_DOUBLE:
	    do i = 1, nlines {
	        if (impnld (im2, buf2, v) == EOF)
		    call error (0, wrerr)
		yline = i - iyshift
		buf1 = imgs2d (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, wrerr)
		call amovd (Memd[buf1], Memd[buf2], ncols)
	    }
	case TY_COMPLEX:
	    do i = 1, nlines {
	        if (impnlx (im2, buf2, v) == EOF)
		    call error (0, wrerr)
		yline = i - iyshift
		buf1 = imgs2x (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (0, wrerr)
		call amovx (Memx[buf1], Memx[buf2], ncols)
	    }
	default:
	    call error (6, "ISHIFTXY: Unknown IRAF type.")
	}
end



# RG_XFSHIFTIM -- Shift a 1 or 2D image by a fractional pixel amount
# in x and y.

procedure rg_xfshiftim (im1, im2, xshift, yshift, interpstr, boundary_type,
        constant)

pointer		im1		#I pointer to input image
pointer		im2		#I pointer to output image
real		xshift		#I shift in x direction
real		yshift		#I shift in y direction
char		interpstr[ARB]	#I type of interpolant
int		boundary_type	#I type of boundary extension
real		constant	#I value of constant for boundary extension

long	l_val
int	interp_type, nsinc, nincr
long	i
size_t	ncols, nlines
long	nbpix, fstline, lstline, nxymargin
long	cin1, cin2, nxin, lin1, lin2, nyin
long	lout1, lout2, nyout
real	xshft, yshft, deltax, deltay, dx, dy, cx, ly
pointer	sp, x, y, msi, sinbuf, soutbuf
bool	fp_equalr()
int	msigeti()
long	lint()
pointer	imps2r()
real	aabs()

errchk	imgs2r, imps2r
errchk	msiinit, msifree, msifit, msigrid
errchk	smark, salloc, sfree

begin
	ncols = IM_LEN(im1,1)
	nlines = IM_LEN(im1,2)

	# Check for out of bounds shift.
	if (xshift < -ncols || xshift > ncols)
	    call error (0, "XC_SHIFTIM: X shift out of bounds.")
	if (yshift < -nlines || yshift > nlines)
	    call error (0, "XC_SHIFTIM: Y shift out of bounds.")

	# Get the real shift.
	if (boundary_type == BT_WRAP) {
	    xshft = amod(xshift, real(ncols))
	    yshft = amod(yshift, real(nlines))
	} else {
	    xshft = xshift
	    yshft = yshift
	}

	# Allocate temporary space.
	call smark (sp)
	call salloc (x, 2 * ncols, TY_REAL)
	call salloc (y, 2 * nlines, TY_REAL)
	sinbuf = NULL

	# Define the x and y interpolation coordinates.
	dx = aabs(xshft - aint(xshft))
	if (fp_equalr (dx, 0.0))
	    deltax = 0.0
	else if (xshft > 0.)
	    deltax = 1. - dx
	else
	    deltax = dx
	dy = aabs(yshft - aint(yshft))
	if (fp_equalr (dy, 0.0))
	    deltay = 0.0
	else if (yshft > 0.)
	    deltay = 1. - dy
	else
	    deltay = dy

	# Initialize the 2-D interpolation routines.
	call msitype (interpstr, interp_type, nsinc, nincr, cx)
	if (interp_type == II_BILSINC || interp_type == II_BISINC)
	    call msisinit (msi, interp_type, nsinc, 1, 1,
	        deltax - anint(deltax), deltay - anint(deltay), 0.0)
	else
	    call msisinit (msi, interp_type, nsinc, 1, 1, cx, cx, 0.0)

	# Set boundary extension parameters.
	if (interp_type == II_BISPLINE3)
	    nxymargin = NMARGIN_SPLINE3
	else if (interp_type == II_BISINC || interp_type == II_BILSINC)
	    nxymargin = msigeti (msi, II_MSINSINC)
	else
	    nxymargin = NMARGIN
	nbpix = max (lint(aabs(xshft)+1.0), lint(aabs(yshft)+1.0)) + nxymargin
	call imsetl (im1, IM_NBNDRYPIX, nbpix)
	call imseti (im1, IM_TYBNDRY, boundary_type)
	if (boundary_type == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Define the x interpolation coordinates.
	deltax = deltax + nxymargin
        if (interp_type == II_BIDRIZZLE) {
            do i = 1, ncols {
                Memr[x+2*i-2] = i + deltax - 0.5
                Memr[x+2*i-1] = i + deltax + 0.5
            }
        } else {
	    do i = 1, ncols
	        Memr[x+i-1] = i + deltax
	}

	# Define the y interpolation coordinates.
	deltay = deltay + nxymargin
        if (interp_type == II_BIDRIZZLE) {
            do i = 1, NYOUT {
                Memr[y+2*i-2] = i + deltay - 0.5
                Memr[y+2*i-1] = i + deltay + 0.5
            }
        } else {
	    do i = 1, NYOUT
	        Memr[y+i-1] = i + deltay
	}

	# Define column range in the input image.
	cx = 1. - nxymargin - xshft
	if ((cx <= 0.) &&  (! fp_equalr (dx, 0.0)))
	    cin1 = lint(cx) - 1
	else
	    cin1 = lint(cx)
	cin2 = ncols - xshft + nxymargin + 1
	nxin = cin2 - cin1 + 1

	# Loop over output sections.
	for (lout1 = 1; lout1 <= nlines; lout1 = lout1 + NYOUT) { 

	    # Define range of output lines.
	    lout2 = min (lout1 + NYOUT - 1, nlines)
	    nyout = lout2 - lout1 + 1

	    # Define correspoding range of input lines.
	    ly = lout1 - nxymargin - yshft
	    if ((ly <= 0) && (! fp_equalr (dy, 0.0)))
	        lin1 = lint(ly) - 1
	    else
		lin1 = lint(ly)
	    lin2 = lout2 - yshft + nxymargin + 1
	    nyin = lin2 - lin1 + 1

	    # Get appropriate input image section and compute the coefficients.
	    if ((sinbuf == NULL) || (lin1 < fstline) || (lin2 > lstline)) {
		fstline = lin1
		lstline = lin2
		call rg_buf (im1, cin1, cin2, lin1, lin2, sinbuf)
	        call msifit (msi, Memr[sinbuf], nxin, nyin, nxin)
	    }

	    # Output the image section.
	    l_val = 1
	    soutbuf = imps2r (im2, l_val, ncols, lout1, lout2)
	    if (soutbuf == EOF)
		call error (0, "GSHIFTXY: Error writing output image.")

	    # Evaluate the interpolant.
	    call msigrid (msi, Memr[x], Memr[y], Memr[soutbuf], ncols, nyout,
		ncols)
	}

	call msifree (msi)
	call sfree (sp)
end


# RG_BUF -- Procedure to provide a buffer of image lines with minimum reads

procedure rg_buf (im, col1, col2, line1, line2, buf)

pointer	im		#I pointer to input image
long	col1, col2	#I column range of input buffer
long	line1, line2	#I line range of input buffer
pointer	buf		#I buffer

long	i, nclast, llast1, llast2, nllast
size_t	ncols, nlines
pointer	buf1, buf2

pointer	imgs2r()

begin
	ncols = col2 - col1 + 1
	nlines = line2 - line1 + 1

	if (buf == NULL) {
	    call malloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	} else if ((nlines != nllast) || (ncols != nclast)) {
	    call realloc (buf, ncols * nlines, TY_REAL)
	    llast1 = line1 - nlines
	    llast2 = line2 - nlines
	}

	if (line1 < llast1) {
	    do i = line2, line1, -1 {
		if (i > llast1)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	} else if (line2 > llast2) {
	    do i = line1, line2 {
		if (i < llast2)
		    buf1 = buf + (i - llast1) * ncols
		else
		    buf1 = imgs2r (im, col1, col2, i, i)
		buf2 = buf + (i - line1) * ncols
		call amovr (Memr[buf1], Memr[buf2], ncols)
	    }
	}

	llast1 = line1
	llast2 = line2
	nclast = ncols
	nllast = nlines
end
