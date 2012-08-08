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

int	interp_type
pointer	sp, str
bool	fp_equalr()
int	strdic()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	interp_type = strdic (interpstr, Memc[str], SZ_FNAME, II_BFUNCTIONS)

	if (interp_type == II_NEAREST)
	    call rg_xishiftim (im1, im2, nint (xshift), nint (yshift),
	        interp_type, boundary_type, constant)
	else if (fp_equalr (xshift, real (int (xshift))) && fp_equalr (yshift,
	    real (int (xshift))))
	    call rg_xishiftim (im1, im2, int (xshift), int (yshift),
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
int	nxshift, nyshift	#I shift in x and y
int	interp_type		#I type of interpolant
int	boundary_type		#I type of boundary extension
real	constant		#I constant for boundary extension

int	ixshift, iyshift
pointer	buf1, buf2
long	v[IM_MAXDIM]
int	ncols, nlines, nbpix
int	i, x1col, x2col, yline

int	impnls(), impnli(), impnll(), impnlr(), impnld(), impnlx()
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
	    ixshift = mod (ixshift, ncols)
	    iyshift = mod (iyshift, nlines)
	}

	# Set the boundary extension values.
	nbpix = max (abs (ixshift), abs (iyshift))
	call imseti (im1, IM_NBNDRYPIX, nbpix)
	call imseti (im1, IM_TYBNDRY, boundary_type)
	if (boundary_type == BT_CONSTANT)
	    call imsetr (im1, IM_BNDRYPIXVAL, constant)

	# Get column boundaries in the input image.
	x1col = max (-ncols + 1, - ixshift + 1) 
	x2col = min (2 * ncols,  ncols - ixshift)

	call amovkl (long (1), v, IM_MAXDIM)

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
	case TY_INT:
	    do i = 1, nlines {
	        if (impnli (im2, buf2, v) == EOF)
		    call error (5, wrerr)
		yline = i - iyshift
		buf1 = imgs2i (im1, x1col, x2col, yline, yline)
		if (buf1 == EOF)
		    call error (5, wrerr)
		call amovi (Memi[buf1], Memi[buf2], ncols)
	    }
	case TY_USHORT, TY_LONG:
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

int	i, interp_type, nsinc, nincr
int	ncols, nlines, nbpix, fstline, lstline, nxymargin
int	cin1, cin2, nxin, lin1, lin2, nyin
int	lout1, lout2, nyout
real	xshft, yshft, deltax, deltay, dx, dy, cx, ly
pointer	sp, x, y, msi, sinbuf, soutbuf
bool	fp_equalr()
int	msigeti()
pointer	imps2r()

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
	    xshft = mod (xshift, real (ncols))
	    yshft = mod (yshift, real (nlines))
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
	dx = abs (xshft - int (xshft))
	if (fp_equalr (dx, 0.0))
	    deltax = 0.0
	else if (xshft > 0.)
	    deltax = 1. - dx
	else
	    deltax = dx
	dy = abs (yshft - int (yshft))
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
	        deltax - nint (deltax), deltay - nint (deltay), 0.0)
	else
	    call msisinit (msi, interp_type, nsinc, 1, 1, cx, cx, 0.0)

	# Set boundary extension parameters.
	if (interp_type == II_BISPLINE3)
	    nxymargin = NMARGIN_SPLINE3
	else if (interp_type == II_BISINC || interp_type == II_BILSINC)
	    nxymargin = msigeti (msi, II_MSINSINC)
	else
	    nxymargin = NMARGIN
	nbpix = max (int (abs(xshft)+1.0), int (abs(yshft)+1.0)) + nxymargin
	call imseti (im1, IM_NBNDRYPIX, nbpix)
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
	    cin1 = int (cx) - 1
	else
	    cin1 = int (cx)
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
	        lin1 = int (ly) - 1
	    else
		lin1 = int (ly)
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
	    soutbuf = imps2r (im2, 1, ncols, lout1, lout2)
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
int	col1, col2	#I column range of input buffer
int	line1, line2	#I line range of input buffer
pointer	buf		#I buffer

int	i, ncols, nlines, nclast, llast1, llast2, nllast
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
