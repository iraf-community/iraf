include	<imhdr.h>


# T_SKYREPLACE -- Replace objects by sky.  This development code as is not
# used in the package.  It is here to be worked on further when an image
# display interface is added.

procedure t_skyreplace ()

char	image[SZ_FNAME]		# Image to be modified

char	graph[SZ_LINE], display[SZ_LINE], cmd[SZ_LINE]
pointer	im, immap()
int	clgeti(), wcs, key, clgcur(), nrep, skyreplace()
real	wx, wy, xc, yc, r, s

begin
	call clgstr ("image", image, SZ_FNAME)
	call sprintf (graph, SZ_LINE, "contour %s")
	    call pargstr (image)
	call sprintf (display, SZ_LINE, "display %s %d")
	    call pargstr (image)
	    call pargi (clgeti ("frame"))

	im = immap (image, READ_WRITE, 0)
	while (clgcur ("cursor",wx, wy, wcs, key, cmd, SZ_LINE) != EOF) {
	    switch (key) {
	    case 'a':
		r = sqrt ((wx - xc) ** 2 + (wy - yc) ** 2)
		s = 2 * r
	    case 'b':
		nrep = skyreplace (im, xc, yc, r, s)
	    case 'c':
		xc = wx
		yc = wy
	    case 'd':
		call imunmap (im)
		call clcmdw (display)
		im = immap (image, READ_WRITE, 0)
	    case 'g':
		call imunmap (im)
		call clcmdw (graph)
		im = immap (image, READ_WRITE, 0)
	    case 'q':
		break
	    default:
		call printf ("\007")
	    }
	}

	call imunmap (im)
end


define	NSKY	100	# Minimum number of sky points

int procedure skyreplace (im, xc, yc, r, s)

pointer	im		# IMIO pointer
real	xc, yc		# Object center
real	r		# Object aperture radius
real	s		# Sky aperture radius

real	avg, sigma, urand(), mode, find_mode()
long	seed
int	xlen, ylen, nx, nx1, nx2, ny, ny1, ny2, ntotal, nobj, nallsky, nsky[4]
int	i, j, x1, x2, x3, x4, y1, y2, y3, y4, y
pointer	sp, allsky, sky[4], ptr1, ptr2
pointer	datain, dataout, imgs2r(), imps2r()

begin
	xlen = IM_LEN(im,1)
	ylen = IM_LEN(im,2)
	x1 = max (1, int (xc - s))
	x4 = min (xlen, int (xc + s + 0.5))
	y1 = max (1, int (yc - s))
	y4 = min (ylen, int (yc + s + 0.5))
	nx = x4 - x1 + 1
	ny = y4 - y1 + 1
	ntotal = nx * ny

	x2 = max (1, int (xc - r))
	x3 = min (xlen, int (xc + r + 0.5))
	y2 = max (1, int (yc - r))
	y3 = min (xlen, int (yc + r + 0.5))
	nx1 = (x3 - x2 + 1)
	ny1 = (y3 - y2 + 1)
	nobj = nx1 * ny1
	nallsky = ntotal - nobj

	if ((nallsky < NSKY) || (nobj < 1))
	    return (0)

	call smark (sp)
	call salloc (allsky, nallsky, TY_REAL)
	datain = imgs2r (im, x1, x4, y1, y4)
	dataout = imps2r (im, x2, x3, y2, y3)
	ptr2 = allsky

	# First quadrant
	x2 = max (1, int (xc - r))
	x3 = min (xlen, int (xc + 0.5))
	y2 = max (1, int (yc - r))
	y3 = min (xlen, int (yc + 0.5))
	nx1 = x3 - x1 + 1
	nx2 = x3 - x2
	ny1 = y2 - y1
	ny2 = y3 - y2 + 1
	nsky[1] = nx1 * ny1 + nx2 * ny2
	sky[1] = ptr2

	if (nsky[1] > 0) {
	    ptr1 = datain
	    for (y=y1; y<y2; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx1)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx1
	    }
	    for (; y<=y3; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx2)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx2
	    }
	}

	# Second quadrant
	x2 = max (1, int (xc + 1.5))
	x3 = min (xlen, int (xc + r + 0.5))
	y2 = max (1, int (yc - r))
	y3 = min (xlen, int (yc + 0.5))
	nx1 = x4 - x2 + 1
	nx2 = x4 - x3
	ny1 = y2 - y1
	ny2 = y3 - y2 + 1
	nsky[2] = nx1 * ny1 + nx2 * ny2
	sky[2] = ptr2

	if (nsky[2] > 0) {
	    ptr1 = datain + x2 - x1
	    for (y=y1; y<y2; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx1)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx1
	    }
	    ptr1 = ptr1 + x3 - x2 + 1
	    for (; y<=y3; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx2)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx2
	    }
	}

	# Third quadrant
	x2 = max (1, int (xc - r))
	x3 = min (xlen, int (xc + 0.5))
	y2 = max (1, int (yc + 1.5))
	y3 = min (xlen, int (yc + r + 0.5))
	nx1 = x3 - x2
	nx2 = x3 - x1 + 1
	ny1 = y3 - y2 + 1
	ny2 = y4 - y3
	nsky[3] = nx1 * ny1 + nx2 * ny2
	sky[3] = ptr2

	if (nsky[3] > 0) {
	    ptr1 = datain + (y2 - y1) * nx
	    for (y=y2; y<=y3; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx1)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx1
	    }
	    for (; y<=y4; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx2)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx2
	    }
	}

	# Fourth quadrant
	x2 = max (1, int (xc + 1.5))
	x3 = min (xlen, int (xc + r + 0.5))
	y2 = max (1, int (yc + 1.5))
	y3 = min (xlen, int (yc + r + 0.5))
	nx1 = x4 - x3
	nx2 = x4 - x2 + 1
	ny1 = y3 - y2 + 1
	ny2 = y4 - y3
	nsky[4] = ny1 * nx1 + ny2 * nx2
	sky[4] = ptr2

	if (nsky[4] > 0) {
	    ptr1 = datain + (y2 - y1) * nx + x3 - x1 + 1
	    for (y=y2; y<=y3; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx1)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx1
	    }
	    ptr1 = ptr1 - (x3 - x2 + 1)
	    for (; y<=y4; y=y+1) {
	        call amovr (Memr[ptr1], Memr[ptr2], nx2)
	        ptr1 = ptr1 + nx
	        ptr2 = ptr2 + nx2
	    }
	}

	# This part is for doing a gradient correction.  It is not implemented.
#	if ((nsky[1]>NSKY)&&(nsky[2]>NSKY)&&(nsky[3]>NSKY)&&(nsky[4]>NSKY)) {
#	    call asrtr (Memr[sky[1]], Memr[sky[1]], nsky[1])
#	    call asrtr (Memr[sky[2]], Memr[sky[2]], nsky[2])
#	    call asrtr (Memr[sky[3]], Memr[sky[3]], nsky[3])
#	    call asrtr (Memr[sky[4]], Memr[sky[4]], nsky[4])

	    # Add a gradient correction here.

#	    seed = dataout
#	    do i = dataout, dataout+nobj-1 {
#	        j = 4 * urand (seed) + 1
#	        k = 0.95 * nsky[j] * urand (seed)
#	        Memr[i] = Memr[sky[j]+k]
#	    }
#	} else {
	    call asrtr (Memr[allsky], Memr[allsky], nallsky)

	    # Find the mean and sigma excluding the outer 20%
	    x1 = 0.1 * nallsky
	    x2 = 0.9 * nallsky
	    call aavgr (Memr[allsky+x1-1], x2-x1+1, avg, sigma)
	    mode = find_mode (Memr[allsky], nallsky, nallsky / 20)
	    call printf ("Mean = %g,  Median = %g,  Mode = %g\n")
		call pargr (avg)
		call pargr (Memr[allsky+nallsky/2-1])
		call pargr (mode)
	    for (x1=0; (x1<nallsky)&&(Memr[allsky+x1]<avg-3*sigma); x1=x1+1)
		;
	    for (x2=nallsky-1; (x2>0)&&(Memr[allsky+x2]>avg+3*sigma); x2=x2-1)
		;
	    nx = x2 - x1 - 1

	    seed = dataout
	    do i = dataout, dataout+nobj-1 {
	        j = nx * urand (seed) + x1
	        Memr[i] = Memr[allsky+j]
	    }
#	}

	call sfree (sp)
	return (nobj)
end

real procedure find_mode (data, npts, n)

real	data[npts]		# Data
int	npts			# Number of data points
int	n			# Bin size

int	x, xlast, xmin
real	sumx, sumy, sumxx, sumxy, a, amin
pointer	sp, slope

begin
	call smark (sp)
	call salloc (slope, npts - n, TY_REAL)

	sumx = 0.
	sumy = 0.
	sumxx = 0.
	sumxy = 0.

	x = 0
	xlast = 0
	while (x < n) {
	    x = x + 1
	    sumx = sumx + x
	    sumy = sumy + data[x]
	    sumxx = sumxx + x ** 2
	    sumxy = sumxy + x * data[x]
	}
	amin = (n * sumxy - sumx * sumy) / (n * sumxx - sumx ** 2)
	xmin = (x + xlast) / 2
	Memr[slope] = amin

	while (x < npts - n) {
	    x = x + 1
	    xlast = xlast + 1
	    sumx = sumx + x - xlast
	    sumy = sumy + data[x] - data[xlast]
	    sumxx = sumxx + x * x - xlast * xlast
	    sumxy = sumxy + x * data[x] - xlast * data[xlast]

	    a = (n * sumxy - sumx * sumy) / (n * sumxx - sumx ** 2)
	    if (a < amin) {
		amin = a
		xmin = (x + xlast) / 2
	    }
	    Memr[slope+xlast] = a
	}

	call gplotv (Memr[slope+11], npts-2*n-22, 1., real (npts-2*n-22), "")
	call sfree (sp)
	return (data[xmin])
end
