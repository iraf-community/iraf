include <imhdr.h>
include <error.h>
include	<mach.h>
include <gset.h>

define	EXTRA_HT	0.1
define	SZ_TITLE	512

# T_RADPLT -- Generate a radial profile plot around a star center.

procedure t_radplt()

char	ifile[SZ_FNAME]
int	infile, nfiles

pointer	im
int	cboxsize, rboxsize
real	xinit, yinit, xcntr, ycntr

int	clpopni(), clplen(), clgfil()
int	clgeti()
real	clgetr()
pointer	immap()

begin
	# Get file names
	infile = clpopni ("input")
	nfiles = clplen (infile)

	# Get x and y initial
	xinit = clgetr ("x_init")
	yinit = clgetr ("y_init")

	# Get box size to use for centering
	cboxsize = clgeti ("cboxsize")

	# Get box size to use for radial plot
	rboxsize = clgeti ("rboxsize")

	# Loop over all images
	while (clgfil (infile, ifile, SZ_FNAME) != EOF) {
	    iferr (im = immap (ifile, READ_ONLY, 0)) {
		call eprintf ("[%s] not found\n")
		call pargstr (ifile)
		next
	    }

	    # Find star center
	    call mpc_cntr (im, xinit, yinit, cboxsize, xcntr, ycntr)

	    # Plot profile
	    call mpc_rplot (im, ifile, xcntr, ycntr, rboxsize)

	    call printf ("[%s] x:%7.2f   y:%7.2f\n")
		call pargstr (ifile)
		call pargr (xcntr)
		call pargr (ycntr)

	    call imunmap (im)
	}
end


# MPC_CNTR -- Compute star center using MPC algorithm.

procedure mpc_cntr (im, xstart, ystart, boxsize, xcntr, ycntr)

pointer	im
real	xstart, ystart
int	boxsize
real	xcntr, ycntr

int	x1, x2, y1, y2, half_box
int	ncols, nrows, nx, ny, try
real	xinit, yinit
pointer	bufptr, sp, x_vect, y_vect
int	imgs2r()

begin
	half_box = (boxsize - 1) / 2
	xinit = xstart
	yinit = ystart

	# Mark region to extract - use box size
	ncols = IM_LEN (im, 1)
	nrows = IM_LEN (im, 2)
	try = 0

	repeat {
	    x1 = amax1 (xinit - half_box, 1.0) +0.5
	    x2 = amin1 (xinit + half_box, real(ncols)) +0.5
	    y1 = amax1 (yinit - half_box, 1.0) +0.5
	    y2 = amin1 (yinit + half_box, real(nrows)) +0.5
	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # Extract region around center
	    bufptr = imgs2r (im, x1, x2, y1, y2)

	    # Collapse to two 1-D arrays
	    call smark (sp)
	    call salloc (x_vect, nx, TY_REAL)
	    call salloc (y_vect, ny, TY_REAL)

	    call aclrr (Memr[x_vect], nx)
	    call aclrr (Memr[y_vect], ny)

	    # Sum all rows
	    call mpc_rowsum (Memr[bufptr], Memr[x_vect], nx, ny)

	    # Sum all columns
	    call mpc_colsum (Memr[bufptr], Memr[y_vect], nx, ny)

	    # Find centers
	    call mpc_getcenter (Memr[x_vect], nx, xcntr)
	    call mpc_getcenter (Memr[y_vect], ny, ycntr)

	    # Add in offsets
	    xcntr = xcntr + x1
	    ycntr = ycntr + y1

	    call sfree (sp)
	    try = try + 1
	    if (try == 1) {
		if ((abs(xcntr-xinit) > 1.0) || (abs(ycntr-yinit) > 1.0)) {
		    xinit = xcntr
		    yinit = ycntr
		}
	    } else
		break
	}
end


# MPC_RPLOT -- Plot intensity as a function of radial distance.

procedure mpc_rplot (im, imname, xcntr, ycntr, rboxsize)

pointer	im
char	imname[ARB]
real	xcntr, ycntr
int	rboxsize

int	x1, x2, y1, y2, half_box
pointer	bufptr, title, sp, gp, op
int	ncols, nrows, nx, ny, i, j
real	xinit, yinit, radval, intval, ymin, ymax, xlen
int	imgs2r(), strlen()
pointer	gopen()

begin
	call smark (sp)
	call salloc (title, SZ_TITLE, TY_CHAR)

	half_box = (rboxsize - 1) / 2
	xinit = xcntr
	yinit = ycntr

	# Mark region to extract - use box size
	ncols = IM_LEN(im,1)
	nrows = IM_LEN(im,2)

	x1 = amax1 (xinit - half_box, 1.0) +0.5
	x2 = amin1 (xinit + half_box, real(ncols)) +0.5
	y1 = amax1 (yinit - half_box, 1.0) +0.5
	y2 = amin1 (yinit + half_box, real(nrows)) +0.5
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1

	# Extract region around center.
	bufptr = imgs2r (im, x1, x2, y1, y2)

	# Begin plotting.
	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)

	call mpc_aminmax (Memr[bufptr], nx, ny, ymin, ymax)
	ymax = ymax + EXTRA_HT * (ymax-ymin)
	ymin = ymin - EXTRA_HT * (ymax-ymin)

	xlen = 1.5 * rboxsize / 2
	call gswind (gp, 0.0, xlen, ymin, ymax)

	call sysid (Memc[title], SZ_LINE)
	op = title + strlen (Memc[title])
	call sprintf (Memc[op], SZ_TITLE-SZ_LINE,
	    "\nRadial Plot of %s at [%0.2f,%0.2f]\n")
	    call pargstr (imname)
	    call pargr (xcntr)
	    call pargr (ycntr)

	call glabax (gp, Memc[title], "Pixels", "Counts")

	do i = 1, ny
	    do j = 1, nx {
		call mpc_radius (Memr[bufptr], nx, ny, j, i, xcntr-x1+1, 
		    ycntr-y1+1, radval, intval)
		call gmark (gp, radval, intval, GM_PLUS, -.005*xlen,
		    -0.007*(ymax-ymin))
	    }

	call gclose (gp)
	call sfree (sp)
end


# AMINMAX -- Compute min and max of two-d array.

procedure mpc_aminmax (a, nx, ny, ymin, ymax)

int	nx, ny
real	a[nx,ny]
real	ymin, ymax

int	i, j

begin
	ymin = a[1,1]
	ymax = ymin

	do i = 1, ny
	    do j = 1, nx {
		ymin = amin1 (ymin, a[j,i])
		ymax = amax1 (ymax, a[j,i])
	    }
end


# RADIUS -- Compute radius from center.

procedure mpc_radius (a, nx, ny, i, j, xc, yc, radval, intval)

real	a[nx, ny]
int	nx, ny, i, j
real	xc, yc, dx, dy, radval, intval

begin
	dx = xc - i
	dy = yc - j
	radval = sqrt (dx**2 + dy**2)
	intval = a[i,j]
end


# ROWSUM -- Sum all rows in a raster

procedure mpc_rowsum (v, row, nx, ny)

int	nx, ny
real	v[nx,ny]
real	row[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		row[j] = row[j] + v[j,i]
end


# COLSUM -- Sum all columns in a raster.

procedure mpc_colsum (v, col, nx, ny)

int	nx, ny
real	v[nx,ny]
real	col[ARB]

int	i, j

begin
	do i = 1, ny
	    do j = 1, nx
		col[j] = col[j] + v[i,j]
end


# GETCENTER -- Compute center of gravity of array.

procedure mpc_getcenter (v, nv, vc)

real	v[ARB]
int	nv
real	vc

int	i
real	sum1, sum2, sigma, cont

begin
	# Assume continuum level is at endpoints
	# Compute first moment
	sum1 = 0.0
	sum2 = 0.0

	call aavgr (v, nv, cont, sigma)

	do i = 1, nv
	    if (v[i] > cont) {
	        sum1 = sum1 + (i-1) * (v[i] - cont)
	        sum2 = sum2 + (v[i] - cont)
	    }

	# Determine center
	vc = sum1 / sum2
end
