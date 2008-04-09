# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>

task	mktest	= t_mktest,
	sigl2	= t_sigl2,
	wrimage = t_wrimage,
	zscale	= t_zscale,
	rcur	= t_rcur

define	TWOPI	6.23


# MKTEST -- Make a test image containing a circularly symetric sinusoid.

procedure t_mktest()

char	imname[SZ_FNAME]
long	nx, ny
long	i, j
real	period, xcen, ycen, radius
pointer	im, line

long	clgetl()
real	clgetr()
pointer	immap(), impl2r()
include	<nullptr.inc>

begin
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, NEW_IMAGE, NULLPTR)

	nx = clgetl ("nx")
	ny = clgetl ("ny")
	period = clgetr ("period")

	IM_LEN(im,1) = nx
	IM_LEN(im,2) = ny

	xcen = (nx + 1) / 2.0
	ycen = (ny + 1) / 2.0

	do j = 1, ny {
	    line = impl2r (im, j)
	    do i = 1, nx {
		radius = sqrt ((i - xcen) ** 2 + (j - ycen) ** 2)
		Memr[line+i-1] = sin ((radius / period) * TWOPI) * 255.0
	    }
	}

	call imunmap (im)
end


# READ -- Benchmark scaled input procedure.

procedure t_sigl2 ()

char	imname[SZ_FNAME]
pointer	im, si, buf
long	i, xblk, yblk
size_t	nx, ny
real	rval0, rval1
pointer	sigl2_setup(), sigl2s(), immap()
include	<nullptr.inc>

begin
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, READ_ONLY, NULLPTR)

	nx = IM_LEN(im,1)
	ny = IM_LEN(im,2)

	xblk = INDEFL
	yblk = INDEFL
	rval0 = nx
	rval1 = ny
	si = sigl2_setup (im, 1.0,rval0,nx,xblk, 1.0,rval1,ny,yblk,0)

	do i = 1, ny
	    buf = sigl2s (si, i)

	call sigl2_free (si)
	call imunmap (im)
end


# WRIMAGE -- Benchmark image output as used in the display program.

procedure t_wrimage ()

char	imname[SZ_FNAME]
long	i, ncols, nlines, c_1
pointer	im, buf
long	clgetl()
pointer	immap(), imps2s()
include	<nullptr.inc>

begin
	c_1 = 1

	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, NEW_IMAGE, NULLPTR)

	ncols  = clgetl ("ncols")
	nlines = clgetl ("nlines")

	IM_LEN(im,1) = ncols
	IM_LEN(im,2) = nlines
	IM_PIXTYPE(im) = TY_SHORT

	do i = 1, nlines {
	    buf = imps2s (im, c_1, ncols, i, i)
	}

	call imunmap (im)
end


# ZSCALE -- Test the zscale procedure, used to determine the smallest range of
# greyscale values which preserves the most information in an image.

procedure t_zscale()

char	imname[SZ_FNAME]
size_t	sample_size, len_stdline
real	z1, z2, contrast
long	clgetl()
real	clgetr()
pointer	im, immap()
include	<nullptr.inc>

begin	
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, READ_ONLY, NULLPTR)

	sample_size = clgetl ("npix")
	len_stdline = clgetl ("stdline")
	contrast    = clgetr ("contrast")

	call zscale (im, z1, z2, contrast, sample_size, len_stdline)
	call printf ("z1=%g, z2=%g\n")
	    call pargr (z1)
	    call pargr (z2)
end


# RCUR -- Try reading the image cursor.

procedure t_rcur()

real	x, y
int	wcs, key
int	wci, pause
char	device[SZ_FNAME]
char	strval[SZ_LINE]

bool	clgetb()
int	btoi(), clgeti(), imdrcur()

begin
	call clgstr ("device", device, SZ_FNAME)
	wci = clgeti ("wcs")
	pause = btoi (clgetb ("pause"))

	while (imdrcur (device, x,y,wcs,key,strval,SZ_LINE, wci,pause) != EOF) {
	    call printf ("%8.2f %8.2f %d %o %s\n")
		call pargr (x)
		call pargr (y)
		call pargi (wcs)
		call pargi (key)
		call pargstr (strval)
	    if (key == 'q')
		break
	}
end
