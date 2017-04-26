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
int	nx, ny
int	i, j
real	period, xcen, ycen, radius
pointer	im, line

int	clgeti()
real	clgetr()
pointer	immap(), impl2r()

begin
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, NEW_IMAGE, 0)

	nx = clgeti ("nx")
	ny = clgeti ("ny")
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
int	i, nx, ny, xblk, yblk
pointer	sigl2_setup(), sigl2s(), immap()

begin
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, READ_ONLY, 0)

	nx = IM_LEN(im,1)
	ny = IM_LEN(im,2)

	xblk = INDEFI
	yblk = INDEFI
	si = sigl2_setup (im, 1.0,real(nx),nx,xblk, 1.0,real(ny),ny,yblk,0)

	do i = 1, ny
	    buf = sigl2s (si, i)

	call sigl2_free (si)
	call imunmap (im)
end


# WRIMAGE -- Benchmark image output as used in the display program.

procedure t_wrimage ()

char	imname[SZ_FNAME]
int	i, ncols, nlines
pointer	im, buf
int	clgeti()
pointer	immap(), imps2s()

begin
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, NEW_IMAGE, 0)

	ncols  = clgeti ("ncols")
	nlines = clgeti ("nlines")

	IM_LEN(im,1) = ncols
	IM_LEN(im,2) = nlines
	IM_PIXTYPE(im) = TY_SHORT

	do i = 1, nlines
	    buf = imps2s (im, 1, ncols, i, i)

	call imunmap (im)
end


# ZSCALE -- Test the zscale procedure, used to determine the smallest range of
# greyscale values which preserves the most information in an image.

procedure t_zscale()

char	imname[SZ_FNAME]
int	sample_size, len_stdline
real	z1, z2, contrast
int	clgeti()
real	clgetr()
pointer	im, immap()

begin	
	call clgstr ("imname", imname, SZ_FNAME)
	im = immap (imname, READ_ONLY, 0)

	sample_size = clgeti ("npix")
	len_stdline = clgeti ("stdline")
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
