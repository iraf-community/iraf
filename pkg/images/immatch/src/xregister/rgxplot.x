include <imhdr.h>
include <gset.h>

# RG_XPLINE -- Plot a line of reference and input image.

procedure rg_xpline (gd, imr, im, nliner, xshift, yshift)

pointer	gd		#I pointer to the graphics stream
pointer	imr		#I pointer to the reference image
pointer	im		#I pointer to the image
int	nliner		#I the reference line
int	xshift		#I x shift
int	yshift		#I y shift

int	i, rncols, rnlines, incols, inlines
pointer	sp, title, xr, xi, ptrr, ptri
real	ymin, ymax, tymin, tymax
int	strlen()
pointer	imgl1r(), imgl2r()

begin
	# Return if no graphics stream.
	if (gd == NULL)
	    return

	# Check for valid line number.
	rncols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    rnlines = 1
	else
	    rnlines = IM_LEN(imr,2)
	incols = IM_LEN(im,1)
	if (IM_NDIM(im) == 1)
	    inlines = 1
	else
	    inlines = IM_LEN(im,2)
	if ((nliner < 1) || (nliner > rnlines))
	    return
	if (((nliner + yshift) < 1) || ((nliner + yshift) > inlines)) 
	    return

	# Allocate working space.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (xr, rncols, TY_REAL)
	call salloc (xi, rncols, TY_REAL)

	# Initialize the x data data.
	do i = 1, rncols {
	    Memr[xr+i-1] = i
	    Memr[xi+i-1] = i - xshift
	}

	# Initalize the y data.
	if (IM_NDIM(imr) == 1)
	    ptrr = imgl1r (imr)
	else
	    ptrr = imgl2r (imr, nliner)
	if (IM_NDIM(im) == 1)
	    ptri = imgl1r (im)
	else
	    ptri = imgl2r (im, nliner + yshift)
	call alimr (Memr[ptrr], rncols, ymin, ymax)
	call alimr (Memr[ptri], incols, tymin, tymax)
	ymin = min (ymin, tymin)
	ymax = max (ymax, tymax)

	# Construct the title.
	call sprintf (Memc[title], SZ_LINE,
	    "Refimage: %s  Image: %s\n")
	    call pargstr (IM_HDRFILE(imr))
	    call pargstr (IM_HDRFILE(im))
	call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	    "Refline (solid): %d  Inline (dashed): %d  Xlag: %d  Ylag: %d")
	    call pargi (nliner)
	    call pargi (nliner + yshift)
	    call pargi (xshift)
	    call pargi (yshift)

	# Set up the axes labels and window.
	call gclear (gd)
	call gswind (gd, 1.0, real(rncols), ymin, ymax)
	call glabax (gd, Memc[title], "Column Number", "Counts")

	# Plot the two lines.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, Memr[xr], Memr[ptrr], rncols)
	call gseti (gd, G_PLTYPE, GL_DASHED)
	call gpline (gd, Memr[xi], Memr[ptri], incols)
	call gflush (gd)

	call sfree (sp)
end


# RG_XPCOL -- Plot a column in the reference and input image.

procedure rg_xpcol (gd, imr, im, ncolr, xshift, yshift)

pointer	gd		#I pointer to the graphics stream
pointer	imr		#I pointer to the reference image
pointer	im		#I pointer to the image
int	ncolr		#I the line number
int	xshift		#I xshift to be applied
int	yshift		#I yshift to be applied

int	i, rncols, rnlines, incols, inlines
pointer	sp, title, xr, xi, ptrr, ptri
real	ymin, ymax, tymin, tymax
int	strlen()
pointer	imgs1r(), imgs2r()

begin
	# Return if no graphics stream.
	if (gd == NULL)
	    return

	# Check for valid column number.
	rncols = IM_LEN(imr,1)
	if (IM_NDIM(imr) == 1)
	    rnlines = 1
	else
	    rnlines = IM_LEN(imr,2)
	incols = IM_LEN(im,1)
	if (IM_NDIM(im) == 1)
	    inlines = 1
	else
	    inlines = IM_LEN(im,2)
	if ((ncolr < 1) || (ncolr > rncols))
	    return
	if (((ncolr - xshift) < 1) || ((ncolr - xshift) > incols))
	    return

	# Allocate valid working space.
	call smark (sp)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (xr, rnlines, TY_REAL)
	call salloc (xi, inlines, TY_REAL)

	# Initialize the data.
	do i = 1, rnlines {
	    Memr[xr+i-1] = i
	    Memr[xi+i-1] = i - yshift
	}
	if (IM_NDIM(imr) == 1)
	    ptrr = imgs1r (imr, ncolr, ncolr)
	else
	    ptrr = imgs2r (imr, ncolr, ncolr, 1, rnlines)
	if (IM_NDIM(im) == 1)
	    ptri = imgs1r (im, ncolr + xshift, ncolr + xshift)
	else
	    ptri = imgs2r (im, ncolr + xshift, ncolr + xshift, 1, inlines)
	call alimr (Memr[ptrr], rnlines, ymin, ymax)
	call alimr (Memr[ptri], inlines, tymin, tymax)
	ymin = min (ymin, tymin)
	ymax = max (ymax, tymax)

	# Construct the title.
	call sprintf (Memc[title], SZ_LINE, "Refimage: %s Image: %s\n")
	    call pargstr (IM_HDRFILE(imr))
	    call pargstr (IM_HDRFILE(im))
	call sprintf (Memc[title+strlen(Memc[title])], SZ_LINE,
	    "Refcol (solid): %d  Imcol (dashed): %d  Xlag: %d Ylag: %d")
	    call pargi (ncolr)
	    call pargi (ncolr + xshift)
	    call pargi (xshift)
	    call pargi (yshift)

	# Set up the labels and the axes.
	call gclear (gd)
	call gswind (gd, 1.0, real (rnlines), ymin, ymax)
	call glabax (gd, Memc[title], "Line Number", "Counts")

	# Plot the profile.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, Memr[xr], Memr[ptrr], rnlines)
	call gseti (gd, G_PLTYPE, GL_DASHED)
	call gpline (gd, Memr[xi], Memr[ptri], rnlines)
	call gflush (gd)

	call sfree (sp)
end


# RG_XCPLINE -- Plot a line of the 2D correlation function.

procedure rg_xcpline (gd, title, data, nx, ny, nline)

pointer	gd		#I pointer to the graphics stream
char	title[ARB]	#I title for the plot
real	data[nx,ARB]	#I the input data array	
int	nx, ny		#I dimensions of the input data array
int	nline		#I the line number

int	i
pointer	sp, str, x
real	ymin, ymax

begin
	# Return if no graphics stream.
	if (gd == NULL)
	    return

	# Check for valid line number.
	if (nline < 1 || nline > ny)
	    return

	# Allocate some working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, nx, TY_REAL)

	# Initialize the data.
	do i = 1, nx
	    Memr[x+i-1] = i
	call alimr (data[1,nline], nx, ymin, ymax)

	# Set up the labels and the axes.
	call gclear (gd)
	call gswind (gd, 1.0, real (nx), ymin, ymax)
	call glabax (gd, title, "X Lag", "X-Correlation Function")

	# Plot the line profile.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, Memr[x], data[1,nline], nx)
	call gflush (gd)

	call sfree (sp)
end


# RG_XCPCOL -- Plot a column of the cross-correlation function.

procedure rg_xcpcol (gd, title, data, nx, ny, ncol)

pointer	gd		#I pointer to the graphics stream
char	title[ARB]	#I title of the column plot
real	data[nx,ARB]	#I the input data array
int	nx, ny		#I the dimensions of the input data array
int	ncol		#I line number

int	i
pointer	sp, x, y
real	ymin, ymax

begin
	# Return if no graphics stream.
	if (gd == NULL)
	    return

	# Check for valid column number.
	if (ncol < 1 || ncol > nx)
	    return

	# Initialize.
	call smark (sp)
	call salloc (x, ny, TY_REAL)
	call salloc (y, ny, TY_REAL)

	# Get the data to be plotted.
	do i = 1, ny {
	    Memr[x+i-1] = i
	    Memr[y+i-1] = data[ncol,i]
	}
	call alimr (Memr[y], ny, ymin, ymax)

	# Set up the labels and the axes.
	call gclear (gd)
	call gswind (gd, 1.0, real (ny), ymin, ymax)
	call glabax (gd, title, "Y Lag", "X-Correlation Function") 

	# Plot the profile.
	call gseti (gd, G_PLTYPE, GL_SOLID)
	call gpline (gd, Memr[x], Memr[y], ny)
	call gflush (gd)

	call sfree (sp)
end


# RG_XMKPEAK -- Procedure to mark the peak from a correlation function
# contour plot.

procedure rg_xmkpeak (gd, xwindow, ywindow, xshift, yshift)

pointer	gd		#I pointer to the graphics stream
int	xwindow		#I x dimension of correlation function
int	ywindow		#I y dimension of correlation function
real	xshift		#O x shift
real	yshift		#O y shift

int	wcs, key
pointer	sp, cmd
real	wx, wy
int	clgcur()

begin
	if (gd == NULL)
	    return

	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call printf ("Mark peak of the cross correlation function\n")
	if (clgcur ("gcommands", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)
	    ;
	if (wx < 1.0 || wx > real (xwindow) || wy < 1.0 || wy >
	    real (ywindow)) {
	    xshift = 0.0
	    yshift = 0.0
	} else {
	    xshift = wx - (1 + xwindow) / 2
	    yshift = wy - (1 + ywindow) / 2
	}

	call sfree (sp)
end
