# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>

define	MAXPTS		20

task	plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8,
	balls, ticks, vdm


procedure plot1()

real	v[5]
int	i
int	fd, open()
pointer	gp, gopen()

begin
	do i = 1, 5
	    v[i] = i ** 2

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gswind (gp, 1., 5., INDEF, INDEF)
	call gascale (gp, v, 5, 2)
	call glabax (gp, "Y = X ** 2", "X-AXIS", "Y-AXIS")
	call gvline (gp, v, 5, 1., 5.)

	call gclose (gp)
	call close (fd)
end


procedure plot2()

int	i
real	x[512], y[512]
real	xc, yc, xs, ys
int	fd, open()
pointer	gp, gopen()

begin
	do i = 1, 512 {
	    x[i] = ((i - 256.0) / 16.)
	    if (abs(x[i]) < EPSILON)
		y[i] = 1.0
	    else
		y[i] = sin (x[i]) / x[i]
	}

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gascale (gp, x, 512, 1)
	call gascale (gp, y, 512, 2)
	call glabax (gp, "The SINC Function", "X-AXIS", "Y-AXIS")
	call gpline (gp, x, y, 512)

	xc = 8
	yc = .25
	xs = 3.2
	ys = 0.1

	do i = 1, 10 {
	    call gmark (gp, xc, yc, GM_CIRCLE, -xs, -ys)
	    xc = xc + xs / 5
	    yc = yc + ys / 5
	    xs = xs * 1.25
	    ys = ys * 1.5
	}

	call gclose (gp)
	call close (fd)
end


procedure plot3()

int	i
real	x[512], y[512]
int	fd, open()
pointer	gp, gopen()

begin
	do i = 1, 512 {
	    x[i] = ((i - 256.0) / 8.)
	    if (abs(x[i]) < EPSILON)
		y[i] = 1.0
	    else
		y[i] = sin (x[i]) / x[i]
	}

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gseti (gp, G_DRAWGRID, YES)
	call gascale (gp, x, 512, 1)
	call gascale (gp, y, 512, 2)
	call glabax (gp, "The SINC Function", "X-AXIS", "Y-AXIS")
	call gpline (gp, x, y, 512)

	call gclose (gp)
	call close (fd)
end


procedure plot4()

int	i
real	x[512], y[512]
int	fd, open()
pointer	gp, gopen()

begin
	do i = 1, 512 {
	    x[i] = (i - 256.0) / 4.
	    if (abs(x[i]) < EPSILON)
		y[i] = 2.0 * 1E4
	    else
		y[i] = (sin (x[i]) / x[i] + 1.0) * 1E4
	}

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gseti (gp, G_YTRAN, GW_LOG)
	call gascale (gp, x, 512, 1)
	call gascale (gp, y, 512, 2)
	call glabax (gp, "Log of The SINC Function", "X-AXIS", "Y-AXIS")
	call gpline (gp, x, y, 512)

	call gclose (gp)
	call close (fd)
end


procedure plot5()

int	fd
int	open(), clgeti()
real	x1, x2, clgetr()
pointer	gp, gopen()

begin
	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	x1 = clgetr ("x1")
	x2 = clgetr ("x2")

	call gseti (gp, G_NMINOR, clgeti ("nminor"))
	call gseti (gp, G_XTRAN, GW_LOG)
	call gseti (gp, G_YTRAN, GW_LOG)
	call gsetr (gp, G_MINORWIDTH, 1.0)
	call gswind (gp, x1, x2, 0.001, 1000.0)
	call glabax (gp, "Log Scaling", "X-AXIS", "Y-AXIS")

	call gclose (gp)
	call close (fd)
end


procedure plot6()

int	i
long	seed
real	size, urand()
int	fd, open(), clgeti()
pointer	gp, gopen()
data	seed /3/

begin
	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gseti (gp, G_ASPECT, clgeti("aspect"))
	call glabax (gp, "", "", "")

	do i = 1, 300 {
	    size = real (nint (urand(seed) * 4 + .5))
	    call gmark (gp, urand(seed), urand(seed), GM_BOX, size, size)
	}

	call gclose (gp)
	call close (fd)
end


procedure plot7()

int	i
real	x[8192], y[8192]
int	fd, open()
pointer	gp, gopen()

begin
	do i = 1, 8192 {
	    x[i] = ((i - 4096.0) / 128.)
	    if (abs(x[i]) < EPSILON)
		y[i] = 1.0
	    else
		y[i] = sin (x[i]) / x[i]
	    y[i] = y[i] + cos ((i-1) * 0.392699) * .001
	}

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gseti (gp, G_DRAWGRID, YES)
	call gascale (gp, x, 8192, 1)
	call gascale (gp, y, 8192, 2)
	call glabax (gp, "The SINC Function", "X-AXIS", "Y-AXIS")
	call gpline (gp, x, y, 8192)

	call gclose (gp)
	call close (fd)
end


procedure balls()

int	i, j, m, npts, nsteps
long	seed
real	p[MAXPTS,2], d[MAXPTS,2]
real	urand()
int	fd, open(), clgeti()
pointer	gp, gopen()

begin
	npts = max(1, min(MAXPTS, clgeti ("npoints")))
	nsteps = max (10, clgeti ("nsteps"))

	# iferr (call delete ("x_mc"))
	    # ;
	# fd = open ("x_mc", NEW_FILE, BINARY_FILE)
	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	# call glabax (gp, "Bouncing Balls", "", "")

	# Set the initial conditions.
	do i = 1, npts
	    do j = 1, 2 {
		p[i,j] = urand (seed)
		d[i,j] = max (0.01, urand (seed) * .1)
		if (mod (i, 2) == 0)
		    d[i,j] = -d[i,j]
	    }

	# Draw the trajectories.
	do m = 1, nsteps
	    do i = 1, npts {
		call gseti (gp, G_PMLTYPE, GL_CLEAR)
		call gmark (gp, p[i,1], p[i,2], GM_DIAMOND, 4., 4.)

		do j = 1, 2 {
		    p[i,j] = p[i,j] + d[i,j]
		    if (p[i,j] < 0) {
			p[i,j] = -p[i,j]
			d[i,j] = -d[i,j]
		    } else if (p[i,j] > 1) {
			p[i,j] = 1 - (p[i,j] - 1)
			d[i,j] = -d[i,j]
		    }
		}
	    
		call gseti (gp, G_PMLTYPE, GL_SOLID)
		call gmark (gp, p[i,1], p[i,2], GM_DIAMOND, 4., 4.)

		call gflush (gp)
	    }

	call gclose (gp)
	call close (fd)
end


procedure ticks()

real	x1, x2, p1, p2
int	rough_nticks
int	logflag
real	tick1, step, linearity

bool	clgetb()
int	btoi(), clgeti()
real	gt_linearity(), clgetr(), elogr()

begin
	x1 = clgetr ("x1")
	x2 = clgetr ("x2")
	rough_nticks = clgeti ("nticks")
	logflag = btoi (clgetb ("log"))

	if (logflag == YES) {
	    p1 = elogr (x1)
	    p2 = elogr (x2)
	} else {
	    p1 = x1
	    p2 = x2
	}

	linearity = gt_linearity (x1, x2)
	call gtickr (p1, p2, rough_nticks, logflag, tick1, step)

	call printf ("tick1=%g, step=%g, linearity=%g\n")
	    call pargr (tick1)
	    call pargr (step)
	    call pargr (linearity)
end


procedure plot8()

int	i
real	x[512], y[512]
int	fd
pointer	gp, gopen()

begin
	do i = 1, 512 {
	    x[i] = ((i - 256.0) / 8.)
	    if (abs(x[i]) < EPSILON)
		y[i] = 1.0
	    else
		y[i] = sin (x[i]) / x[i]
	}

	fd = STDGRAPH
	gp = gopen ("stdgraph", NEW_FILE, fd)

	call gseti (gp, G_DRAWAXES, 1)
	call gseti (gp, G_SETAXISPOS, YES)
	call gsetr (gp, G_AXISPOS1, 0.0)

	call gseti (gp, G_DRAWGRID, YES)
	call gascale (gp, x, 512, 1)
	call gascale (gp, y, 512, 2)
	call glabax (gp, "", "", "")
	call gpline (gp, x, y, 512)
	call gtext (gp, -20., 0.80, "The Sinc Function", "hj=c,vj=b")
	call gtext (gp, -20., 0.75, "y = sin(x) / x", "hj=c,vj=b")

	call gclose (gp)
	call close (fd)
end


# VDM -- Test output of a plot to the virtual device metafile.

procedure vdm()

real	v[5]
int	i
pointer	gp, gopen()

begin
	do i = 1, 5
	    v[i] = i ** 2

	gp = gopen ("vdm", NEW_FILE, STDGRAPH)

	call gswind (gp, 1., 5., INDEF, INDEF)
	call gascale (gp, v, 5, 2)
	call glabax (gp, "Y = X ** 2", "X-AXIS", "Y-AXIS")
	call gvline (gp, v, 5, 1., 5.)

	call gclose (gp)
end
