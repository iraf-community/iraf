# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<imhdr.h>
include	<mach.h>
include	<gset.h>
include	<mwset.h>

define	SEGLEN		10
define	SZ_PLOTTITLE	512
define	KEYSFILE	"lib$scr/implot.key"
define	MAX_COLORS	8


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
#	m		previous image
#	n		next image
#	p		measure profile (mark region and baseline with 2 pos)
#	o		overplot next vector
#	r		redraw
#	s		print statistics on a region
#	/		scroll status line
#
#
# In addition to the above keystrokes, the following ':' escapes are recognized
# by the program:
#
#	:a N		set number of lines or columns to average
#	:c M [N]	plot column M or avg of M to N
#	:f format	set label format
#	:i imagename	open a new image for input
#	:l M [N]	plot line M or avg of M to N
#	:o		overplot
#	:log+,log-	enable, disable log scaling in Y
#	:step N		set step size for j,k
#	:solid		overplot with solid, not dashed, lines
#	:mono		disable coloring of overplotted vectors
#	:x x1 x2	fix plot window in X (no args to unfix)
#	:y y1 y2	fix plot window in Y (no args to unfix)
#	:w wcstype	change world coordinate type

procedure t_implot()

int	list
char	image[SZ_FNAME]
char	wcstype[SZ_FNAME]
char	xlabel[SZ_FNAME]
char	format[SZ_FNAME]
char	fmt[SZ_FNAME]
char	command[SZ_FNAME]
char	device[SZ_FNAME]

int	xnticks, ynticks
bool	overplot, lineplot, logscale, erase, rescale[2], p_rescale[2]
int	key, wcs, ip, i1, i2, n, linetype, color, nltypes, linestep, navg
int	npix, nlines, ncols, line, col, shift, step, p_navg, sline, nim, index
real	x, y, px, py, qx, qy, x1, x2, y1, y2
real	median, mean, sigma, sum
pointer	im, mw, ct, gp, xold, yold, xnew, ynew, sl, ptr

real	asumr(), amedr(), plt_iformatr()
int	clgeti(), clgcur(), ctoi(), ctor(), ggeti(), imtlen(), imaccess()
pointer	gopen(), immap(), mw_openim(), mw_sctran(), sl_getstr()
pointer	imtopenp(), imtrgetim()
errchk	mw_sctran

define	line_ 91
define	col_ 92
define	replotline_ 93
define	replotcol_ 94
define	nextim_ 95
define	quit_ 96
string	bell "\007"
string	again "again:"

begin
	list = imtopenp ("image")
	call clgstr ("device", device, SZ_FNAME)
	gp = gopen (device, NEW_FILE, STDGRAPH)
	call clgstr ("wcs", wcstype, SZ_FNAME)

	if (clgeti ("$nargs") > 1)
	    line = clgeti ("line")
	else
	    line = INDEFI

	p_rescale[1] = true
	rescale[1]   = true
	p_rescale[2] = true
	rescale[2]   = true

	logscale  = false
	overplot  = false
	lineplot  = true
	erase     = false
	xnticks	  = 5
	ynticks	  = 5
	format[1] = EOS

	linestep  = 1
	linetype  = 1
	color     = 1
	nltypes   = ggeti (gp, "lt")
	p_navg	  = 1
	navg	  = 1
	step	  = clgeti ("step")

	# Loop through the images.  Currently this loop is not actually
	# used and instead the 'm' and 'n' keys explicitly change the
	# image.  The 'q' key exits the loop regardless of the position
	# of the list.

	nim = imtlen (list)
	index = 1
	while (imtrgetim (list, index, image, SZ_FNAME) != EOF) {
	    iferr {
		im = NULL; mw = NULL; sl = NULL

		if (imaccess (image, READ_ONLY) == YES) {
		    ptr = immap (image, READ_ONLY, 0); 
		    im = ptr
	   
		} else {
		    call eprintf ("Error opening image '%s'")
			call pargstr (image)
		    goto nextim_
		}

		ptr = mw_openim (im); mw = ptr
		call mw_seti (mw, MW_USEAXMAP, NO)

		ct = mw_sctran (mw, "logical", wcstype, 0)

		ncols = IM_LEN(im,1)
		if (IM_NDIM(im) <= 0)
		    call error (1, "image has no pixels")
		else if (IM_NDIM(im) > 1)
		    nlines = IM_LEN(im,2)
		else
		    nlines = 1

		if (IS_INDEFI(line))
		    line = max(1, min(nlines, (nlines + 1) / 2))

		if (IS_INDEFI(step) || step < 1)
		    step = max (1, nlines / 10)

		npix = max (ncols, nlines)
		call malloc (xold, npix, TY_REAL)
		call malloc (yold, npix, TY_REAL)
		call malloc (xnew, npix, TY_REAL)
		call malloc (ynew, npix, TY_REAL)

		if (!overplot)
		    call gclear (gp)
		call imp_getvector (im, mw, ct, wcstype, xnew, ynew, xlabel,
		    fmt, line, navg, lineplot)
		if (format[1] == '%')
		    call strcpy (format, fmt, SZ_FNAME)
		npix = ncols

		call gsets (gp, G_XTICKFORMAT, fmt)
		if (xnticks >= 0)
		    call gseti (gp, G_XNMAJOR, xnticks)
		if (ynticks >= 0)
		    call gseti (gp, G_YNMAJOR, ynticks)
		call gseti (gp, G_NMINOR, 0)
		call gseti (gp, G_PLTYPE, 1)
		call gseti (gp, G_PLCOLOR, 1)

		call imp_plotvector (gp, im, Memr[xnew], Memr[ynew], ncols,
		    nlines, real(line), navg, lineplot, rescale, image, xlabel)
		overplot = false

		call sl_init (sl, 1)
		while (clgcur ("coords", x, y, wcs, key, command, SZ_FNAME) !=
		    EOF) {
		    if (key == 'q')
quit_			break

		    switch (key) {
		    case 'a':
			# Plot the average over a range of lines or columns
			# marked interactively with the cursor.

			x1 = x; y1 = y
			call printf (again)
			if (clgcur ("gcur", x2, y2, wcs, key, command,
			    SZ_FNAME) == EOF)
			    next

			if (abs(x2-x1) > abs(y2-y1)) {
			    # Range is in X.

			    navg = abs (x2 - x1) + 1
			    if (lineplot) {
				col = nint (min (x1, x2))
				goto col_
			    } else {
				line = nint (min (x1, x2))
				goto line_
			    }

			} else {
			    # Range is in Y.

			    if (lineplot) {
				call imp_tran (gp, x1, y1, x1, y1, Memr[xnew],
				    ncols, nlines)
				call imp_tran (gp, x2, y2, x2, y2, Memr[xnew],
				    ncols, nlines)
				navg = abs (y2 - y1) + 1
				line = nint (min (y1, y2))
				goto line_
			    } else {
				call imp_tran (gp, x1, y1, x1, y1, Memr[xnew],
				    nlines, ncols)
				call imp_tran (gp, x2, y2, x2, y2, Memr[xnew],
				    nlines, ncols)
				navg = abs (y2 - y1) + 1
				col = nint (min (y1, y2))
				goto col_
			    }
			}

		    case 'j', 'k':
			# Move viewport into image up (k) or down (j).  This
			# is done by erasing the old data vector and drawing
			# a new one.

			erase = true
			navg = p_navg
			overplot = true
			call amovr (Memr[xnew], Memr[xold], npix)
			call amovr (Memr[ynew], Memr[yold], npix)

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
			    call imp_tran (gp, x, y, px, py, Memr[xnew], ncols,
				nlines)
			    line = max(1, min(nlines, nint(py)))
			} else {
			    call imp_tran (gp, x, y, px, py, Memr[xnew], nlines,
				ncols)
			    line = max(1, min(nlines, nint(px)))
			}
			navg = p_navg
			line = line - (navg - 1) / 2
line_
			lineplot = true
			line = max(1, min(nlines, line))
			call imp_getvector (im, mw, ct, wcstype, xnew, ynew,
			    xlabel, fmt, line, navg, lineplot)
			if (format[1] == '%')
			    call strcpy (format, fmt, SZ_FNAME)
			npix = ncols
replotline_
			if (overplot) {
			    if (erase) {
				# Erase old vector and replace it with new
				# vector.

				call imp_redraw (gp, Memr[xold], Memr[yold],
				    Memr[xnew], Memr[ynew], npix)
				erase = false

			    } else {
				# Overplot new vector. 

				linetype = linetype + linestep
				if (linetype > nltypes)
				    linetype = 1
				call gseti (gp, G_PLTYPE, linetype)

				color = color + 1
				if (color > MAX_COLORS)
				    color = 1
				call gseti (gp, G_PLCOLOR, color)

				call gpline (gp, Memr[xnew], Memr[ynew], ncols)
			    }

			    call imp_markpos (gp, line, nlines)
			    overplot = false

			} else {
			    call gclear (gp)
			    call gsets (gp, G_XTICKFORMAT, fmt)
			    if (logscale)
				call gseti (gp, G_YTRAN, GW_LOG)
			    call gseti (gp, G_NMINOR, 0)
			    if (xnticks >= 0)
				call gseti (gp, G_XNMAJOR, xnticks)
			    if (ynticks >= 0)
				call gseti (gp, G_YNMAJOR, ynticks)
			    linetype = 1
			    color = 1
			    call imp_plotvector (gp, im, Memr[xnew], Memr[ynew],
				ncols, nlines, real(line), navg, lineplot,
				rescale, image, xlabel)
			    rescale[1] = p_rescale[1]
			    rescale[2] = p_rescale[2]
			}

		    case 'm', 'n':
			if (key == 'm') {
			    if (index > 1)
				index = index - 1
			    else
				next
			} else if (key == 'n') {
nextim_		    	if (index < nim)
				index = index + 1
			    else
				next
			}

			if (imtrgetim (list, index, command, SZ_FNAME) == EOF)
			    break

			# Open a different image.
			call mw_close (mw)
			call imunmap (im)

			iferr (im = immap (command, READ_ONLY, 0)) {
			    call erract (EA_WARN)
			    im = immap (image, READ_ONLY, 0)
			    mw = mw_openim (im)
			    call mw_seti (mw, MW_USEAXMAP, NO)
			    ct = mw_sctran (mw, "logical", wcstype, 0)
			    next
			}

			if (IM_NDIM(im) <= 0) {
			    call eprintf ("image has no pixels\n")
			    im = immap (image, READ_ONLY, 0)
			    mw = mw_openim (im)
			    call mw_seti (mw, MW_USEAXMAP, NO)
			    ct = mw_sctran (mw, "logical", wcstype, 0)
			    next

			} else {
			    mw = mw_openim (im)
			    call mw_seti (mw, MW_USEAXMAP, NO)
			    ct = mw_sctran (mw, "logical", wcstype, 0)

			    ncols = IM_LEN(im,1)
			    if (IM_NDIM(im) > 1)
				nlines = IM_LEN(im,2)
			    else {
				lineplot = true
				nlines = 1
			    }

			    npix   = max (ncols, nlines)
			    call strcpy (command, image, SZ_FNAME)
			    call realloc (xold, npix, TY_REAL)
			    call realloc (yold, npix, TY_REAL)
			    call realloc (xnew, npix, TY_REAL)
			    call realloc (ynew, npix, TY_REAL)

			    if (lineplot)
				goto line_
			    else
				goto col_
			}

		    case 'c':
			# Plot a column.
			if (lineplot) {
			    call imp_tran (gp, x, y, px, py, Memr[xnew], ncols,
				nlines)
			    col = max(1, min(ncols, nint(px)))
			} else {
			    call imp_tran (gp, x, y, px, py, Memr[xnew], nlines,
				ncols)
			    col = max(1, min(ncols, nint(py)))
			}
			navg = p_navg
			col = col - (navg - 1) / 2
col_
			if (nlines == 1) {
			    call printf (bell)
			    next
			}
			lineplot = false
			col = max(1, min(ncols, col))
			call imp_getvector (im, mw, ct, wcstype, xnew, ynew,
			    xlabel, fmt, col, navg, lineplot)
			if (format[1] == '%')
			    call strcpy (format, fmt, SZ_FNAME)
			npix = nlines
replotcol_
			if (overplot) {
			    if (erase) {
				# Erase old vector and replace it with new
				# vector.

				call imp_redraw (gp, Memr[xold], Memr[yold],
				    Memr[xnew], Memr[ynew], npix)
				erase = false

			    } else {
				linetype = linetype + linestep
				if (linetype > nltypes)
				    linetype = 1
				call gseti (gp, G_PLTYPE, linetype)

				color = color + 1
				if (color > MAX_COLORS)
				    color = 1
				call gseti (gp, G_PLCOLOR, color)

				call gpline (gp, Memr[xnew], Memr[ynew], nlines)
			    }

			    call imp_markpos (gp, col, ncols)
			    overplot = false

			} else {
			    call gclear (gp)
			    call gsets (gp, G_XTICKFORMAT, fmt)
			    if (logscale)
				call gseti (gp, G_YTRAN, GW_LOG)
			    call gseti (gp, G_NMINOR, 0)
			    if (xnticks >= 0)
				call gseti (gp, G_XNMAJOR, xnticks)
			    if (ynticks >= 0)
				call gseti (gp, G_YNMAJOR, ynticks)
			    linetype = 1
			    color = 1
			    call imp_plotvector (gp, im, Memr[xnew], Memr[ynew],
				nlines, ncols, real(col), navg, lineplot,
				rescale, image, xlabel)
			    rescale[1] = p_rescale[1]
			    rescale[2] = p_rescale[2]
			}

		    case 'e':
			# Expand plot by marking corners of new window.  We are
			# called with the coords of the lower left corner.

			x1 = x; y1 = y
			call printf (again)
			if (clgcur ("gcur", x2, y2, wcs, key, command,
			    SZ_FNAME) == EOF)
			    next

			rescale[1] = false
			rescale[2] = false
			p_rescale[1] = true
			p_rescale[2] = true

			# If the cursor moved only in X, with negligible range
			# in Y, expand only in X.  Do the comparisons in NDC
			# space to avoid scaling problems.

			call gctran (gp, x1, y1, px, py, wcs, 0)
			call gctran (gp, x2, y2, qx, qy, wcs, 0)

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

		    case 'r':
			if (lineplot)
			    goto replotline_
			else
			    goto replotcol_

		    case 'p':
			# Profile analysis.
			x1 = x
			y1 = y
			call printf (again)
			if (clgcur ("gcur", x2, y2, wcs, key, command,
			    SZ_FNAME) == EOF)
			    next

			call imp_profile (gp, Memr[xnew], Memr[ynew], npix,
			    x1, y1, x2, y2, sl, sline)
			call printf (Memc[sl_getstr(sl,sline)])

		    case 's':
			# Statistics.
			x1 = x
			call printf (again)
			if (clgcur ("gcur", x2, y, wcs, key, command,
			    SZ_FNAME) == EOF)
			    next

			i1 = max(1, min(npix, nint(x1)))
			i2 = max(1, min(npix, nint(x2)))
			if (i1 > i2) {
			    n = i1
			    i1 = i2
			    i2 = n
			} else if (i1 == i2)
			    i2 = i1 + 1

			n = i2 - i1 + 1
			call aavgr (Memr[ynew+i1-1], n, mean, sigma)
			median = amedr (Memr[ynew+i1-1], n)
			sum = asumr (Memr[ynew+i1-1], n)

			call sl_init (sl, 1)
			call sprintf (Memc[sl_getstr(sl,1)], SZ_LINE,
			    "median=%g, mean=%g, rms=%g, sum=%g, npix=%d\n")
			    call pargr (median)
			    call pargr (mean)
			    call pargr (sigma)
			    call pargr (sum)
			    call pargi (n)
			sline = 1
			call printf (Memc[sl_getstr(sl,sline)])

		    case ' ':
			# Print cursor coordinates.
			call sl_init (sl, 1)
			if (lineplot) {
			    call imp_tran (gp, x, y, px, py, Memr[xnew], ncols,
				nlines)
			    col = px
			    call plt_wcscoord (im, mw, ct, wcstype, format, col,
				line, Memr[ynew+col-1], Memc[sl_getstr(sl,1)],
				SZ_LINE)
			} else {
			    call imp_tran (gp, x, y, px, py, Memr[xnew], nlines,
				ncols)
			    line = px
			    call plt_wcscoord (im, mw, ct, wcstype, format, col,
				line, Memr[ynew+line-1], Memc[sl_getstr(sl,1)],
				SZ_LINE)
			}
			sline = 1
			call printf (Memc[sl_getstr(sl,sline)])

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
			    call mw_close (mw)
			    call imunmap (im)
			    ip = ip + 1
			    while (IS_WHITE (command[ip]))
				ip = ip + 1

			    iferr (im = immap (command[ip], READ_ONLY, 0)) {
				call erract (EA_WARN)
				im = immap (image, READ_ONLY, 0)
				mw = mw_openim (im)
				call mw_seti (mw, MW_USEAXMAP, NO)
				ct = mw_sctran (mw, "logical", wcstype, 0)

			    } else if (IM_NDIM(im) <= 0) {
				call eprintf ("image has no pixels\n")
				im = immap (image, READ_ONLY, 0)
				mw = mw_openim (im)
				call mw_seti (mw, MW_USEAXMAP, NO)
				ct = mw_sctran (mw, "logical", wcstype, 0)

			    } else {
				mw = mw_openim (im)
				call mw_seti (mw, MW_USEAXMAP, NO)
				ct = mw_sctran (mw, "logical", wcstype, 0)

				ncols = IM_LEN(im,1)
				if (IM_NDIM(im) > 1)
				    nlines = IM_LEN(im,2)
				else
				    nlines = 1

				npix   = max (ncols, nlines)
				call strcpy (command[ip], image, SZ_FNAME)
				call realloc (xold, npix, TY_REAL)
				call realloc (yold, npix, TY_REAL)
				call realloc (xnew, npix, TY_REAL)
				call realloc (ynew, npix, TY_REAL)
			    }

			case 'w':
			    # Change wcs type.
			    call mw_ctfree (ct)
			    ip = ip + 1
			    while (IS_WHITE (command[ip]))
				ip = ip + 1

			    iferr {
				ct = mw_sctran (mw, "logical", command[ip], 0)
				call strcpy (command[ip], wcstype, SZ_FNAME)
			    } then {
				call erract (EA_WARN)
				ct = mw_sctran (mw, "logical", wcstype, 0)
			    } else {
				# Only replot if WCS command succeeds,
				# otherwise the error message is lost.
				if (lineplot)
				    goto line_
				else
				    goto col_
			    }

			case 'f':
			    # Change label format.
			    ip = ip + 1
			    while (IS_WHITE (command[ip]))
				ip = ip + 1
			    if (command[ip] == '%') {
				call strcpy (command[ip], format, SZ_FNAME)
				call strcpy (format, fmt, SZ_FNAME)
				if (lineplot)
				    goto replotline_
				else
				    goto replotcol_
			    } else if (format[1] == '%') {
				call strcpy (command[ip], format, SZ_FNAME)
				if (lineplot)
				    goto line_
				else
				    goto col_
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
				    line = line - (navg - 1) / 2
				    goto line_
				} else {
				    i1 = max(1, min(nlines, i1))
				    i2 = max(1, min(nlines, i2))
				    line = min (i1, i2)
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
				col = col - (navg - 1) / 2
				goto col_
			    } else {
				i1 = max(1, min(ncols, i1))
				i2 = max(1, min(ncols, i2))
				col  = min (i1, i2)
				navg = max (1, abs (i2 - i1) + 1)
				goto col_
			    }

			case 's':
			    if (command[ip+1] == 'o') {
				# Use only linetype=1 (solid).
				linetype = 1
				linestep = 0
				color = 1
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
			    # Fix window in X and replot vector.  If no args
			    # are given, unfix the window.

			    ip = ip + 1
			    if (ctor (command, ip, x1) <= 0) {
				rescale[1]   = true
				p_rescale[1] = true
			    } else if (ctor (command, ip, x2) <= 0) {
				call printf (bell)
			    } else {
				x1 = plt_iformatr (x1, fmt)
				x2 = plt_iformatr (x2, fmt)
				call imp_swind (x1, x2, INDEF, INDEF)
				rescale[1]   = false
				p_rescale[1] = false
			    }

			    if (lineplot)
				goto replotline_
			    else
				goto replotcol_
			
			case 'y':
			    # Fix window in Y and replot vector.  If no args
			    # are given, unfix the window.

			    ip = ip + 1
			    if (ctor (command, ip, y1) <= 0) {
				rescale[2]   = true
				p_rescale[2] = true
			    } else if (ctor (command, ip, y2) <= 0) {
				call printf (bell)
			    } else {
				y1 = plt_iformatr (y1, fmt)
				y2 = plt_iformatr (y2, fmt)
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

		    case '/':
			# Scroll or rewrite the status line.

			sline = sline + 1
			call printf (Memc[sl_getstr(sl,sline)])

		    default:
			call printf (bell)
		    }
		}
	    } then
		call erract (EA_WARN)

	    call mfree (xnew, TY_REAL)
	    call mfree (ynew, TY_REAL)
	    call mfree (xold, TY_REAL)
	    call mfree (yold, TY_REAL)
	    if (sl != NULL)
		call sl_free (sl)

	    if (mw != NULL)
		call mw_close (mw)
	    if (im != NULL)
		call imunmap (im)

	    if (key == 'q')
		break
	}

	call gclose (gp)
	call imtclose (list)
end


# IMP_GETVECTOR -- Get a data vector, i.e., line or column or average of
# lines and columns.

procedure imp_getvector (im, mw, ct, wcstype, x, y, xlabel, format, linecol,
	navg, lineplot)

pointer	im			# image descriptor
pointer	mw			# mwcs descriptor
pointer	ct			# coordinate descriptor
char	wcstype[ARB]		# WCS type
pointer	x, y			# output vector
char	xlabel[SZ_FNAME]	# WCS label
char	format[SZ_FNAME]	# WCS format
int	linecol			# line or column number
int	navg			# number of lines or columns to be averaged
bool	lineplot		# true if line is to be extracted

real	norm
pointer	sp, axvals, buf, off
int	x1, x2, y1, y2
int	nx, ny, width, i, ndim
real	asumr()
pointer	imgl2r(), imgs2r(), imgl1r(), imgs1r()
errchk	imgl2r, imgs2r, imgl1r, imgs1r, plt_wcs

begin
	call smark (sp)
	call salloc (axvals, IM_NDIM(im), TY_REAL)

	call strcpy (wcstype, xlabel, SZ_FNAME)

	ndim = IM_NDIM(im)
	nx = IM_LEN(im,1)
	if (ndim > 1)
	    ny = IM_LEN(im,2)
	else
	    ny = 1
	call amovkr (1., Memr[axvals], ndim)

	if (lineplot) {
	    # Extract a line vector.

	    x1 = 1
	    x2 = nx
	    y1 = max(1, min (ny, linecol))
	    y2 = max(1, min (ny, linecol + navg - 1))

	    Memr[axvals+1] = (y1 + y2) / 2.
	    call plt_wcs (im, mw, ct, 1, Memr[axvals], real(x1), real(x2),
		Memr[x], nx, xlabel, format, SZ_FNAME)

	    if (ndim == 1)
		call amovr (Memr[imgl1r(im)], Memr[y], nx)

	    else {
		# Compute sum.
		call aclrr (Memr[y], nx)
		do i = y1, y2
		    call aaddr (Memr[imgl2r(im,i)], Memr[y], Memr[y], nx)

		# Normalize.
		width = y2 - y1 + 1
		if (width > 1)
		    call amulkr (Memr[y], 1. / width, Memr[y], nx)
	    }

	} else {
	    # Extract a column vector.

	    x1 = max(1, min(nx, linecol))
	    x2 = max(1, min(nx, linecol + navg - 1))
	    y1 = 1
	    y2 = ny

	    Memr[axvals] = (x1 + x2) / 2.
	    call plt_wcs (im, mw, ct, 2, Memr[axvals], real(y1), real(y2),
		Memr[x], ny, xlabel, format, SZ_LINE)

	    width = x2 - x1 + 1
	    norm  = 1.0 / real(width)

	    if (width > 1) {
		call aclrr (Memr[y], ny)
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
		    Memr[y+i-1] = asumr (Memr[off], width) * norm
		}
	    } else {
		buf = imgs2r (im, x1, x2, y1, y2)
		call amovr (Memr[buf], Memr[y], ny)
	    }
	}

	call sfree (sp)
end


# IMP_PLOTVECTOR -- Plot a line or column vector.

procedure imp_plotvector (gp, im, xv, yv, nx, ny, y, navg, lineplot, rescale,
	image, xlabel)

pointer	gp			# graphics descriptor
pointer	im			# image descriptor
real	xv[ARB]			# coordinate vector
real	yv[ARB]			# data  vector
int	nx			# number of pixels in vector
int	ny			# number of pixels on plot-Y axis
real	y			# position on plot Y-axis
int	navg			# number of lines or columns averaged
bool	lineplot		# are we plotting a line or a column
bool	rescale[2]		# rescale plot
char	image[ARB]		# image name
char	xlabel[ARB]		# X label

real	junkr
int	i, i1, i2, npix, maxch
pointer	sp, ip, plot_title, op
bool	fp_equalr()

real	x1, x2, y1, y2
common	/implcom/ x1, x2, y1, y2

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

		i1 = max (1, min (npix, nint (y)))
		i2 = max (1, min (npix, nint (y) + navg - 1))
		call pargi (i1)
		call pargi (i2)
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
	if (rescale[1])
	    call gascale (gp, xv, nx, 1)
	else
	    call gswind (gp, x1, x2, INDEF, INDEF)

	call ggwind (gp, x1, x2, junkr, junkr)
	junkr = min (x1, x2)
	for (i1=1; i1<nx && xv[i1] < junkr; i1=i1+1)
	    ;
	junkr = max (x1, x2)
	for (i2=nx; i2>1 && xv[i2] > junkr; i2=i2-1)
	    ;
	if (i2 < i1) {
	    i = i1
	    i1 = i2
	    i2 = i
	}
	npix = max (1, i2 - i1 + 1)

	if (rescale[2]) {
	    if (npix < 2)
		call gascale (gp, yv[i1], nx, 2)
	    else
		call gascale (gp, yv[i1], npix, 2)
	} else
	    call gswind (gp, INDEF, INDEF, y1, y2)

	call ggwind (gp, x1, x2, y1, y2)

	# If the image is two dimensional plot the position within the image
	# of the plotted vector on the plot-Y axis (which may refer to either
	# X or Y on the image).

	if (IM_LEN(im,2) > 1) {
	    # Draw all but right axes.
	    if (fp_equalr (y1, y2)) {
		y1 = 0.99 * y1
		y2 = 1.01 * y2
		call gswind (gp, INDEF, INDEF, y1, y2)
	    }
	    call gseti (gp, G_YDRAWAXES, 1)
	    call glabax (gp, Memc[plot_title], xlabel, "")

	    # Draw right axis (pixel scale)
	    call ggwind (gp, x1, x2, y1, y2)
	    call gswind (gp, 1., real (nx), 1., real (ny))
	    call gseti (gp, G_XDRAWAXES, 0)
	    call gseti (gp, G_YDRAWAXES, 2)
	    call glabax (gp, "", "", "")
	    call gswind (gp, x1, x2, y1, y2)

	    # Mark position on Y axis.
	    if (abs(y) > .001)
		call imp_markpos (gp, nint(y), ny)
	} else {
	    call glabax (gp, Memc[plot_title], xlabel, "")
	    call ggwind (gp, x1, x2, y1, y2)
	}

	# Draw data vector.
	call gpline (gp, xv, yv, nx)

	call sfree (sp)
end


# IMP_SWIND -- Set all or part of the plotting window if autoscaling is not
# desired.

procedure imp_swind (n_x1, n_x2, n_y1, n_y2)

real	n_x1, n_x2		# range of world coords in X
real	n_y1, n_y2		# range of world coords in Y

real	x1, x2, y1, y2
common	/implcom/ x1, x2, y1, y2

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


# IMP_REDRAW -- Erase the old vector and draw a new one in its place.

procedure imp_redraw (gp, xold, yold, xnew, ynew, npix)

pointer	gp			# graphics descriptor
real	xold[ARB], yold[ARB]	# old data vector
real	xnew[ARB], ynew[ARB]	# new data vector
int	npix			# length of the data vectors

int	i, n

begin
	# Erase the old vector and redraw the new in its place, in segments
	# of length SEGLEN.  These segments must overlap by one pixel to
	# produce a continuous output polyline.

	do i = 1, npix, SEGLEN {
	    n  = min (SEGLEN + 1, npix - i + 1)

	    # Erase next segment of old vector.
	    call gseti (gp, G_PLTYPE, 0)
	    call gpline (gp, xold[i], yold[i], n)

	    # Plot same segment of new vector.
	    call gseti (gp, G_PLTYPE, 1)
	    call gseti (gp, G_PLCOLOR, 1)
	    call gpline (gp, xnew[i], ynew[i], n)
	}
end


# IMP_MARKPOS -- Mark the line or column number on the right axis of the plot.

procedure imp_markpos (gp, line, nlines)

pointer	gp			# graphics descriptor
int	line			# line or column
int	nlines			# number of lines or columns
real	y, x1, x2, y1, y2

begin
	if (nlines < 2)
	    return

	call ggwind (gp, x1, x2, y1, y2)
	y = (y2 - y1) / (nlines - 1) * (line - 1) + y1
	call gmark (gp, x2, y, GM_PLUS, 3., 4.)
end


# IMP_TRAN -- Transform cursor coordinate to line and column in image.

procedure imp_tran (gp, x, y, px, py, xvec, nx, ny)

pointer	gp			# graphics descriptor
real	x, y			# cursor coordinate
real	px, py			# image coordinate
real	xvec[nx]		# x vector
int	nx, ny			# number of columns and lines

int	i
real	x1, x2, y1, y2, diff, diffmin
bool	fp_equalr()

begin
	call ggwind (gp, x1, x2, y1, y2)
	if (fp_equalr (y1, y2))
	    py = nint (ny / 2.)
	else
	    py = nint ((ny - 1) / (y2 - y1) * (y - y1) + 1)

	px = 1
	diffmin = abs (x - xvec[1])
	do i = 2, nx {
	    diff = abs (x - xvec[i])
	    if (diff < diffmin) {
		px = i
		diffmin = diff
	    }
	}
end
