# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<imhdr.h>
include	<mach.h>
include	<gset.h>

define	SEGLEN		10
define	SZ_PLOTTITLE	512
define	KEYSFILE	"lib$scr/implot.key"


# IMPLOT -- Image plot program.  An interactive, cursor driven program for
# plotting lines and columns of images.  This is an early version of the program
# lacking averaging and interaction with the image display cursor.
#
#	implot (image [,line])
#
# Keystrokes:
#
#	?		help
#	a		plot the average of a range of lines of columns
#	c		plot column at position of cursor
#	e		expand plot by marking corners of new window
#	j		move down
#	k		move up
#	l		plot line at position of cursor
#	o		overplot next vector
#	s		print statistics on a region
#
#
# In addition to the above keystrokes, the following ':' escapes are recognized
# by the program:
#
#	:a N		set number of lines or columns to average
#	:c M [N]	plot column M or avg of M to N
#	:i imagename	open a new image for input
#	:l M [N]	plot line M or avg of M to N
#	:o		overplot
#	:log+,log-	enable, disable log scaling in Y
#	:step N		set step size for j,k
#	:solid		overplot with solid, not dashed, lines
#	:x x1 x2	fix plot window in X (no args to unfix)
#	:y y1 y2	fix plot window in Y (no args to unfix)

procedure t_implot()

char	image[SZ_FNAME]
char	command[SZ_FNAME]
char	device[SZ_FNAME]

int	xnticks, ynticks
bool	overplot, lineplot, logscale, erase, rescale[2], p_rescale[2]
int	key, wcs, ip, i1, i2, n, linetype, nltypes, linestep, navg
int	npix, nlines, ncols, line, col, shift, step, center, p_navg
real	x, y, px, py, qx, qy, x1, x2, y1, y2
real	median, mean, sigma, sum
pointer	im, gp, old, new

real	asumr(), amedr()
int	clgeti(), clgcur(), ctoi(), ctor(), ggeti()
pointer	gopen(), immap()

define	line_ 91
define	col_ 92
define	replotline_ 93
define	replotcol_ 94
string	bell "\007"
string	again "again:"

begin
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	call clgstr ("device", device, SZ_FNAME)
	gp = gopen (device, NEW_FILE, STDGRAPH)

	ncols = IM_LEN(im,1)
	if (IM_NDIM(im) <= 0)
	    call error (1, "image has no pixels")
	else if (IM_NDIM(im) > 1)
	    nlines = IM_LEN(im,2)
	else
	    nlines = 1

	if (clgeti ("$nargs") > 1)
	    line = clgeti ("line")
	else
	    line = max(1, min(nlines, (nlines + 1) / 2))

	p_rescale[1] = true
	rescale[1]   = true
	p_rescale[2] = true
	rescale[2]   = true

	logscale  = false
	overplot  = false
	lineplot  = true
	erase     = false
	xnticks	  = -1
	ynticks	  = -1

	linestep  = 1
	linetype  = 1
	nltypes   = ggeti (gp, "lt")
	p_navg	  = 1
	navg	  = 1
	step	  = max (1, nlines / 10)
	old	  = NULL

	npix = max (ncols, nlines)
	call malloc (old, npix, TY_REAL)
	call malloc (new, npix, TY_REAL)

	call imp_getvector (im, new, line, navg, lineplot)
	npix = ncols

	call gseti (gp, G_NMINOR, 0)
	call gseti (gp, G_PLTYPE, 1)
	call gseti (gp, G_WCS, 2)

	call imp_plotvector (gp, im, Memr[new], ncols, nlines, real(line),
	    navg, lineplot, rescale, image)

	while (clgcur ("coords", x, y, wcs, key, command, SZ_FNAME) != EOF) {
	    if (key == 'q')
		break

	    # WCS 2 is the data Y scale (WCS 1 = pixel Y scale).
	    call gseti (gp, G_WCS, 2)

	    switch (key) {
	    case 'a':
		# Plot the average over a range of lines or columns marked
		# interactively with the cursor.

		x1 = x; y1 = y
		call printf (again)
		if (clgcur ("gcur", x2, y2, wcs, key, command, SZ_FNAME) == EOF)
		    return

		if (abs(x2-x1) > abs(y2-y1)) {
		    # Range is in X.

		    center = nint ((x1 + x2) / 2.)
		    navg = abs (x2 - x1) + 1
		    if (lineplot) {
			col = center
			goto col_
		    } else {
			line = center
			goto line_
		    }

		} else {
		    # Range is in Y.

		    call gctran (gp, x1, y1, x1, y1, 2, 1)
		    call gctran (gp, x2, y2, x2, y2, 2, 1)
		    center = nint ((y1 + y2) / 2.)
		    navg = abs (y2 - y1) + 1

		    if (lineplot) {
			line = center
			goto line_
		    } else {
			col = center
			goto col_
		    }
		}

	    case 'j', 'k':
		# Move viewport into image up (k) or down (j).  This is done
		# by erasing the old data vector and drawing a new one.

		erase = true
		navg = p_navg
		overplot = true
		call amovr (Memr[new], Memr[old], npix)

		shift = step
		if (key == 'j')
		    shift = -shift
		    
		if (lineplot) {
		    line = line + shift
		    goto line_
		} else {
		    col  = col  + shift
		    goto col_
		}

	    case 'l':
		# Plot a line.
		if (lineplot) {
		    call gctran (gp, x, y, px, py, 2, 1)
		    line = max(1, min(nlines, nint(py)))
		} else
		    line = max(1, min(nlines, nint(x)))
		navg = p_navg
line_
		lineplot = true
		line = max(1, min(nlines, line))
		call imp_getvector (im, new, line, navg, lineplot)
		npix = ncols
replotline_
		if (overplot) {
		    if (erase) {
			# Erase old vector and replace it with new vector.

			call imp_redraw (gp, Memr[old], Memr[new], npix)
			erase = false

		    } else {
			# Overplot new vector. 

			linetype = linetype + linestep
			if (linetype > nltypes)
			    linetype = 1
			call gseti (gp, G_PLTYPE, linetype)
			call gvline (gp, Memr[new], ncols, 1., real(ncols))
		    }

		    call imp_markpos (gp, real(line))
		    overplot = false

		} else {
		    call gclear (gp)
		    call gseti (gp, G_WCS, 2)
		    if (logscale)
			call gseti (gp, G_YTRAN, GW_LOG)
		    call gseti (gp, G_NMINOR, 0)
		    if (xnticks >= 0)
			call gseti (gp, G_XNMAJOR, xnticks)
		    if (ynticks >= 0)
			call gseti (gp, G_YNMAJOR, ynticks)
		    linetype = 1
		    call imp_plotvector (gp, im, Memr[new], ncols, nlines,
			real(line), navg, lineplot, rescale, image)
		    rescale[1] = p_rescale[1]
		    rescale[2] = p_rescale[2]
		}

	    case 'c':
		# Plot a column.
		if (lineplot)
		    col = max(1, min(ncols, nint(x)))
		else {
		    call gctran (gp, x, y, px, py, 2, 1)
		    col = max(1, min(ncols, nint(py)))
		}
		navg = p_navg
col_
		lineplot = false
		col = max(1, min(ncols, col))
		call imp_getvector (im, new, col, navg, lineplot)
		npix = nlines
replotcol_
		if (overplot) {
		    if (erase) {
			# Erase old vector and replace it with new vector.

			call imp_redraw (gp, Memr[old], Memr[new], npix)
			erase = false

		    } else {
			linetype = linetype + linestep
			if (linetype > nltypes)
			    linetype = 1
			call gseti (gp, G_PLTYPE, linetype)
			call gvline (gp, Memr[new], nlines, 1., real(nlines))
		    }

		    call imp_markpos (gp, real(col))
		    overplot = false

		} else {
		    call gclear (gp)
		    call gseti (gp, G_WCS, 2)
		    if (logscale)
			call gseti (gp, G_YTRAN, GW_LOG)
		    call gseti (gp, G_NMINOR, 0)
		    if (xnticks >= 0)
			call gseti (gp, G_XNMAJOR, xnticks)
		    if (ynticks >= 0)
			call gseti (gp, G_YNMAJOR, ynticks)
		    linetype = 1
		    call imp_plotvector (gp, im, Memr[new], nlines, ncols,
			real(col), navg, lineplot, rescale, image)
		    rescale[1] = p_rescale[1]
		    rescale[2] = p_rescale[2]
		}

	    case 'e':
		# Expand plot by marking corners of new window.  We are called
		# with the coords of the lower left corner.

		x1 = x; y1 = y
		call printf (again)
		if (clgcur ("gcur", x2, y2, wcs, key, command, SZ_FNAME) == EOF)
		    return

		rescale[1] = false
		rescale[2] = false
		p_rescale[1] = true
		p_rescale[2] = true

		# If the cursor moved only in X, with negligible range in Y,
		# expand only in X.  Do the comparisons in NDC space to avoid
		# scaling problems.

		call gctran (gp, x1, y1, px, py, 2, 0)
		call gctran (gp, x2, y2, qx, qy, 2, 0)

		if (abs (py - qy) < .01) {
		    y1 = INDEF;  y2 = INDEF
		    rescale[2] = true
		}
		call imp_swind (x1, x2, y1, y2)

		if (lineplot)
		    goto replotline_
		else
		    goto replotcol_

	    case 'o':
		overplot = true

	    case 's':
		# Statistics.
		x1 = x
		call printf (again)
		if (clgcur ("gcur", x2, y, wcs, key, command, SZ_FNAME) == EOF)
		    break

		i1 = max(1, min(npix, nint(x1)))
		i2 = max(1, min(npix, nint(x2)))
		if (i1 > i2) {
		    n = i1
		    i1 = i2
		    i2 = n
		} else if (i1 == i2)
		    i2 = i1 + 1

		n = i2 - i1 + 1
		call aavgr (Memr[new+i1-1], n, mean, sigma)
		median = amedr (Memr[new+i1-1], n)
		sum = asumr (Memr[new+i1-1], n)

		call printf ("median=%g, mean=%g, rms=%g, sum=%g, npix=%d\n")
		    call pargr (median)
		    call pargr (mean)
		    call pargr (sigma)
		    call pargr (sum)
		    call pargi (n)

	    case '?':
		# Print command summary.
		call gpagefile (gp, KEYSFILE, "implot cursor commands")

	    case ':':
		# Command mode.
		for (ip=1;  IS_WHITE (command[ip]);  ip=ip+1)
		    ;
		if (command[ip] == 'o') {
		    overplot = true
		    ip = ip + 1
		}

		switch (command[ip]) {
		case 'a':
		    # Set number of lines or columns to average.
		    ip = ip + 1
		    if (ctoi (command, ip, p_navg) <= 0) {
			call printf (bell)
			p_navg = 1
		    }

		case 'i':
		    # Open a different image.
		    call imunmap (im)
		    ip = ip + 1
		    while (IS_WHITE (command[ip]))
			ip = ip + 1

		    iferr (im = immap (command[ip], READ_ONLY, 0)) {
			call erract (EA_WARN)
			im = immap (image, READ_ONLY, 0)

		    } else if (IM_NDIM(im) <= 0) {
			call eprintf ("image has no pixels\n")
			im = immap (image, READ_ONLY, 0)

		    } else {
			ncols = IM_LEN(im,1)
			if (IM_NDIM(im) > 1)
			    nlines = IM_LEN(im,2)
			else
			    nlines = 1

			npix   = max (ncols, nlines)
			call strcpy (command[ip], image, SZ_FNAME)
			call realloc (old, npix, TY_REAL)
			call realloc (new, npix, TY_REAL)
		    }

		case 'l':
		    if (command[ip+1] != 'o') {
			# Plot a line.
			ip = ip + 1
			if (ctoi (command, ip, i1) <= 0) {
			    call printf (bell)
			    next
			} else if (ctoi (command, ip, i2) <= 0) {
			    line = max(1, min(nlines, i1))
			    navg = p_navg
			    goto line_
			} else {
			    i1 = max(1, min(nlines, i1))
			    i2 = max(1, min(nlines, i2))
			    line = (i1 + i2) / 2
			    navg = max (1, abs (i2 - i1) + 1)
			    goto line_
			}
		    } else {
			# Enable/disable log scaling.
			while (IS_ALPHA(command[ip]))
			    ip = ip + 1
			logscale = (command[ip] == '+')
		    }

		case 'c':
		    # Plot a column.
		    ip = ip + 1
		    if (ctoi (command, ip, i1) <= 0) {
			call printf (bell)
			next
		    } else if (ctoi (command, ip, i2) <= 0) {
			col = max(1, min(ncols, i1))
			navg = p_navg
			goto col_
		    } else {
			i1 = max(1, min(ncols, i1))
			i2 = max(1, min(ncols, i2))
			col  = (i1 + i2) / 2
			navg = max (1, abs (i2 - i1) + 1)
			goto col_
		    }

		case 's':
		    if (command[ip+1] == 'o') {
			# Use only linetype=1 (solid).
			linetype = 1
			linestep = 0
		    } else {
			# Set step size.
			while (IS_ALPHA (command[ip]))
			    ip = ip + 1
			if (ctoi (command, ip, step) <= 0) {
			    call printf (bell)
			    step = 1
			}
		    }
		
		case 'x':
		    # Fix window in X and replot vector.  If no args are given,
		    # unfix the window.

		    ip = ip + 1
		    if (ctor (command, ip, x1) <= 0) {
			rescale[1]   = true
			p_rescale[1] = true
		    } else if (ctor (command, ip, x2) <= 0) {
			call printf (bell)
		    } else {
			call imp_swind (x1, x2, INDEF, INDEF)
			rescale[1]   = false
			p_rescale[1] = false
		    }

		    if (lineplot)
			goto replotline_
		    else
			goto replotcol_
		
		case 'y':
		    # Fix window in Y and replot vector.  If no args are given,
		    # unfix the window.

		    ip = ip + 1
		    if (ctor (command, ip, y1) <= 0) {
			rescale[2]   = true
			p_rescale[2] = true
		    } else if (ctor (command, ip, y2) <= 0) {
			call printf (bell)
		    } else {
			call imp_swind (INDEF, INDEF, y1, y2)
			p_rescale[2] = false
			rescale[2]   = false
		    }

		    if (lineplot)
			goto replotline_
		    else
			goto replotcol_

		case 'n':
		    ip = ip + 1
		    if (command[ip] == 'x') {
			while (IS_ALPHA(command[ip]))
			    ip = ip + 1
			if (ctoi (command, ip, xnticks) <= 0)
			    xnticks = -1
		    } else if (command[ip] == 'y') {
			while (IS_ALPHA(command[ip]))
			    ip = ip + 1
			if (ctoi (command, ip, ynticks) <= 0)
			    ynticks = -1
		    } else
			call printf (bell)

		default:
		    call printf (bell)
		}

	    default:
		call printf (bell)
	    }
	}

	call mfree (new, TY_REAL)
	call mfree (old, TY_REAL)

	call gclose (gp)
	call imunmap (im)
end


# IMP_GETVECTOR -- Get a data vector, i.e., line or column or average of
# lines and columns.

procedure imp_getvector (im, v, linecol, navg, lineplot)

pointer	im			# image descriptor
pointer	v			# output vector
int	linecol			# line or column number
int	navg			# number of lines or columns to be averaged
bool	lineplot		# true if line is to be extracted

real	norm
pointer	buf, off
int	x1, x2, y1, y2
int	nx, ny, width, i, ndim
real	asumr()
pointer	imgl2r(), imgs2r(), imgl1r(), imgs1r()
errchk	imgl2r, imgs2r, imgl1r, imgs1r

begin
	ndim = IM_NDIM(im)

	nx = IM_LEN(im,1)
	if (ndim > 1)
	    ny = IM_LEN(im,2)
	else
	    ny = 1

	if (lineplot) {
	    # Extract a line vector.

	    if (ndim == 1) {
		call amovr (Memr[imgl1r(im)], Memr[v], nx)
		return
	    }

	    x1 = 1
	    x2 = nx
	    y1 = max(1, min(ny, linecol - (navg / 2)))
	    y2 = max(1, min(ny, linecol + (navg / 2)))

	    # Compute sum.
	    call aclrr (Memr[v], nx)
	    do i = y1, y2
		call aaddr (Memr[imgl2r(im,i)], Memr[v], Memr[v], nx)

	    # Normalize.
	    width = y2 - y1 + 1
	    if (width > 1)
		call amulkr (Memr[v], 1. / width, Memr[v], nx)

	} else {
	    # Extract a column vector.

	    x1 = max(1, min(nx, linecol - (navg / 2)))
	    x2 = max(1, min(nx, linecol + (navg / 2)))
	    y1 = 1
	    y2 = ny

	    width = x2 - x1 + 1
	    norm  = 1.0 / real(width)

	    if (width > 1) {
		call aclrr (Memr[v], ny)
		do i = y1, y2 {
		    if (ndim == 1) {
			buf = imgs1r (im, x1, x2)
			off = buf
		    } else if (nx > 1024) {
			buf = imgs2r (im, x1, x2, i, i)
			off = buf
		    } else {
			buf = imgl2r (im, i)
			off = buf + x1 - 1
		    }
		    Memr[v+i-1] = asumr (Memr[off], width) * norm
		}
	    } else {
		buf = imgs2r (im, x1, x2, y1, y2)
		call amovr (Memr[buf], Memr[v], ny)
	    }
	}
end


# IMP_PLOTVECTOR -- Plot a line or column vector.

procedure imp_plotvector (gp, im, v, nx, ny, y, navg, lineplot, rescale, image)

pointer	gp			# graphics descriptor
pointer	im			# image descriptor
real	v[ARB]			# data vector
int	nx			# number of pixels in vector
int	ny			# number of pixels on plot-Y axis
real	y			# position on plot Y-axis
int	navg			# number of lines or columns averaged
bool	lineplot		# are we plotting a line or a column
bool	rescale[2]		# rescale plot
char	image[ARB]		# image name

real	junkr
int	i1, i2, npix, maxch
pointer	sp, ip, plot_title, op

real	x1, x2, y1, y2
common	/impcom/ x1, x2, y1, y2

begin
	call smark (sp)
	call salloc (plot_title, SZ_PLOTTITLE, TY_CHAR)

	# Format the plot title, starting with the system banner.
	call sysid (Memc[plot_title], SZ_PLOTTITLE)
	for (op=plot_title;  Memc[op] != '\n' && Memc[op] != EOS;  op=op+1)
	    ;
	Memc[op] = '\n'
	op = op + 1
	maxch = SZ_PLOTTITLE - (op - plot_title)

	# Format the remainder of the plot title.
	if (IM_LEN(im,2) <= 1) {
	    # Plot of a one-dim image.
	    call strcpy (IM_TITLE(im), Memc[op], maxch)

	} else if (navg > 1) {
	    call sprintf (Memc[op], maxch, "Average of %s %d to %d of %s\n%s")
		if (lineplot) {
		    call pargstr ("lines")
		    npix = IM_LEN(im,2)
		} else {
		    call pargstr ("columns")
		    npix = IM_LEN(im,1)
		}

		call pargi (max(1, min(npix, nint(y) - navg / 2)))
		call pargi (max(1, min(npix, nint(y) + navg / 2)))
		call pargstr (image)
		call pargstr (IM_TITLE(im))

	} else {
	    call sprintf (Memc[op], maxch, "%s %d of %s\n%s")
		if (lineplot)
		    call pargstr ("Line")
		else
		    call pargstr ("Column")
		call pargi (nint(y))
		call pargstr (image)
		call pargstr (IM_TITLE(im))
	}

	# Delete trailing newline and any whitespace from image title string.
	# Trailing whitespace causes plot title to not be drawn centered on
	# plot.

	for (ip=plot_title;  Memc[ip] != EOS;  ip=ip+1)
	    ;
	ip = ip - 1
	if (ip > plot_title && Memc[ip] == '\n')
	    ip = ip - 1
	while (ip > plot_title && IS_WHITE(Memc[ip]))
	    ip = ip - 1
	Memc[ip+1] = EOS

	# Autoscale the plot in X and or Y if so indicated.

	call gseti (gp, G_WCS, 2)
	if (rescale[1])
	    call gswind (gp, 1., real(nx), INDEF, INDEF)
	else
	    call gswind (gp, x1, x2, INDEF, INDEF)

	call ggwind (gp, x1, x2, junkr, junkr)
	i1 = max(1,  min(nx, nint(x1)))
	i2 = max(i1, min(nx, nint(x2)))
	npix = max (1, i2 - i1 + 1)

	if (rescale[2])
	    call gascale (gp, v[i1], npix, 2)
	else
	    call gswind (gp, INDEF, INDEF, y1, y2)

	# Save window for next time in case rescale is disabled.
	call ggwind (gp, x1, x2, y1, y2)

	# If the image is two dimensional plot the position within the image
	# of the plotted vector on the plot-Y axis (which may refer to either
	# X or Y on the image).  If the image is one dimensional skip this.

	if (IM_LEN(im,2) > 1) {
	    # Draw all but right axes.
	    call gseti (gp, G_YDRAWAXES, 1)
	    call glabax (gp, Memc[plot_title], "", "")

	    # Draw right axis (Y pixel scale).
	    call imp_setpixelcoords (gp, nx, ny)
	    call gseti (gp, G_WCS, 1)
	    call gseti (gp, G_XDRAWAXES, 0)
	    call gseti (gp, G_YDRAWAXES, 2)
	    call glabax (gp, "", "", "")

	    # Mark position on Y axis.
	    if (abs(y) > .001)
		call gmark (gp, real(nx), y, GM_PLUS, 3., 1.)
	} else
	    call glabax (gp, Memc[plot_title], "", "")

	# Draw data vector.
	call gseti (gp, G_WCS, 2)
	call gvline (gp, v[i1], npix, real(i1), real(i2))

	call sfree (sp)
end


# IMP_SWIND -- Set all or part of the plotting window if autoscaling is not
# desired.

procedure imp_swind (n_x1, n_x2, n_y1, n_y2)

real	n_x1, n_x2		# range of world coords in X
real	n_y1, n_y2		# range of world coords in Y

real	x1, x2, y1, y2
common	/impcom/ x1, x2, y1, y2

begin
	if (!IS_INDEF(n_x1))
	    x1 = n_x1
	if (!IS_INDEF(n_x2))
	    x2 = n_x2
	if (!IS_INDEF(n_y1))
	    y1 = n_y1
	if (!IS_INDEF(n_y2))
	    y2 = n_y2
end


# IMP_SETPIXELCOORDS -- Set wcs 2 to the pixel coordinate system of the image.

procedure imp_setpixelcoords (gp, ncols, nlines)

pointer	gp			# graphics descriptor
int	ncols			# number of columns in image
int	nlines			# number of lines in image

real	wx1, wx2, wy1, wy2
real	vx1, vx2, vy1, vy2

begin
	call gseti (gp, G_WCS, 2)
	call ggview (gp, vx1, vx2, vy1, vy2)
	call ggwind (gp, wx1, wx2, wy1, wy2)

	call gseti (gp, G_WCS, 1)
	call gsview (gp, vx1, vx2, vy1, vy2)
	call gswind (gp, 1., max(2.,real(ncols)), 1., max(2.,real(nlines)))

	call gseti (gp, G_WCS, 2)
end


# IMP_REDRAW -- Erase the old vector and draw a new one in its place.

procedure imp_redraw (gp, old, new, npix)

pointer	gp			# graphics descriptor
real	old[ARB]		# old data vector
real	new[ARB]		# new data vector
int	npix			# length of the data vectors

int	i, n
real	x1, x2

begin
	# Erase the old vector and redraw the new in its place, in segments
	# of length SEGLEN.  These segments must overlap by one pixel to
	# produce a continuous output polyline.

	do i = 1, npix, SEGLEN {
	    n  = min (SEGLEN + 1, npix - i + 1)
	    x1 = i
	    x2 = i + n - 1

	    # Erase next segment of old vector.
	    call gseti (gp, G_PLTYPE, 0)
	    call gvline (gp, old[i], n, x1, x2)

	    # Plot same segment of new vector.
	    call gseti (gp, G_PLTYPE, 1)
	    call gvline (gp, new[i], n, x1, x2)
	}
end


# IMP_MARKPOS -- Mark the line or column number on the right axis of the plot.

procedure imp_markpos (gp, y)

pointer	gp			# graphics descriptor
real	y			# y coord of mark
real	x1, x2, y1, y2

begin
	call gseti (gp, G_WCS, 1)
	call ggwind (gp, x1, x2, y1, y2)
	call gmark (gp, x2, y, GM_PLUS, 3., 4.)
	call gseti (gp, G_WCS, 2)
end
