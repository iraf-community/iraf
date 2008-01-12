include <gset.h>
include "igi.h"

# IG_ERRORBAR -- Draw error bars at the coordinates in the input column(s)
#
#  3 Feb 1989, Z. G. Levay, CSC/STScI:  Check for INDEF
#
#  8/20/91 Removed ^Ls. ZGL
#  1/27/93 Fix INDEF tests.

procedure ig_errorbar (igs)

pointer	igs

int	errdir
pointer	igps

int	get_int()

errchk	get_int

begin
	call lcmdcat (igs, YES)

	iferr (errdir = get_int (igs))
	    return

	if (IS_INDEFI (errdir)) {
	    call eprintf ("Specify error bar direction ")
	    return
	} else if (errdir == 0 || errdir < -2 || errdir > 4) {
	    call eprintf ("Invalid error bar direction:  %d ")
		call pargi (errdir)
	    return
	}

	call cmdcat (igs, YES)

	igps = PLOT_PARMS(igs)

	if (MG_EDATAP(igps) == NULL) {
	    call eprintf ("No Error data ")
	    return
	}

	if (MG_YDATAP(igps) == NULL) {
	    call eprintf ("No Y data ")
	    return
	}

	call ii_errorbar (igs, errdir)
end


procedure ii_errorbar (igs, errdir)

pointer	igs
int	errdir

pointer	igps
int	npts
real	size
int	style
bool	xlog, ylog

begin
	igps = PLOT_PARMS(igs)

	if (MG_EDATAP(igps) == NULL || MG_YDATAP(igps) == NULL)
	    return

	call setltype (igs, MG_LTYPEN(igps))
	call gseti (GIO_GP(igs), G_CLIP, YES)

	size = MG_CHARSIZE(igps) * MG_EXPAND(igps)

	style = MG_EBTYPE(igps)
	if (IS_INDEFI (style) || style < BAR_TICK || style > UPPER_LOWER)
	    style = BAR_TICK

	xlog = (MG_XLOG(igps) == YES)
	ylog = (MG_YLOG(igps) == YES)

	# Y and error data exists
	if (MG_XDATAP(igps) == NULL) {
	    # Y data only;  use pixel numbers for X
	    npts = min (MG_YNPTS(igps), MG_ENPTS(igps))
	    call mgverr (GIO_GP(igs), Memr[MG_YDATAP(igps)], 
		Memr[MG_EDATAP(igps)], npts, errdir, size, style,
		xlog, ylog)
	} else {
	    # Both X and Y data
	    npts = min (MG_XNPTS(igps), MG_YNPTS(igps))
	    npts = min (npts, MG_ENPTS(igps))

	    call igpserr (GIO_GP(igs), 
		Memr[MG_XDATAP(igps)], Memr[MG_YDATAP(igps)], 
		Memr[MG_EDATAP(igps)], npts, errdir, size, style,
		xlog, ylog)
	}

	MG_NPTS(igps) = npts

	call gamove (GIO_GP(igs), MG_XPOS(igps), MG_YPOS(igps))
	call gflush (GIO_GP(igs))
end


procedure mgverr (gp, ydata, edata, npts, errdir, size, style,
	xlog, ylog)

#  MGVERR -- Draw error bars for Y value versus pixel number

pointer	gp
real	ydata[ARB]
real	edata[ARB]
int	npts
int	errdir
real	size
int	style
bool	xlog, ylog

int	ipix
real	x1, y1
real	x2, y2
real	e

begin
	if (errdir == 1) {
	    # Horizontal error bars (+X)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Horizontal line
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (xlog)
			x2 = log10 (10**x1 + e)
		    else
			x2 = x1 + e
		    y2 = y1

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Vertical tick
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (IS_INDEF(e))
			next
		    if (xlog)
			x1 = log10 (10**x1 + e)
		    else
			x1 = x1 + e

		    call gmark (gp, x1, y1, GM_VLINE, 0.0, size)
		}

	    if (style == UPPER_LOWER)
		# Right arrow
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (xlog)
			x1 = log10 (10**x1 + e)
		    else
			x1 = x1 + e

		    call arrow (gp, x1, y1, size, RIGHT)
		}

	} else if (errdir == 2) {
	    # Vertical error bars (+Y)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Vertical line
		do ipix = 1, npts {
		    x1 = real (ipix)
		    x2 = x1
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y2 = log10 (10**y1 + e)
		    else
			y2 = y1 + e

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Horizonatal tick
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y1 = log10 (10**y1 + e)
		    else
			y1 = y1 + e
		    call gmark (gp, x1, y1, GM_HLINE, size, 0.0)
		}

	    if (style == UPPER_LOWER)
		# Up arrow
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y1 = log10 (10**y1 + e)
		    else
			y1 = y1 + e

		    call arrow (gp, x1, y1, size, UP)
		}

	} else if (errdir == -1 || errdir == 3) {
	    # Horizontal error bars (-X)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Horizontal line
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (xlog)
			x2 = log10 (10**x1 - e)
		    else
			x2 = x1 - e
		    y2 = y1

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Vertical tick
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (xlog)
			x1 = log10 (10**x1 - e)
		    else
			x1 = x1 - e

		    call gmark (gp, x1, y1, GM_VLINE, 0.0, size)
		}

	    if (style == UPPER_LOWER)
		# Left arrow
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (xlog)
			x1 = log10 (10**x1 - e)
		    else
			x1 = x1 - e

		    call arrow (gp, x1, y1, size, LEFT)
		}

	} else if (errdir == -2 || errdir == 4) {
	    # Vertical error bars (-Y)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Vertical line
		do ipix = 1, npts {
		    x1 = real (ipix)
		    x2 = x1
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y2 = log10 (10**y1 - e)
		    else
			y2 = y1 - e

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Horizonatal tick
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y1 = log10 (10**y1 - e)
		    else
			y1 = y1 - e
		    call gmark (gp, x1, y1, GM_HLINE, size, 0.0)
		}

	    if (style == UPPER_LOWER)
		# Down arrow
		do ipix = 1, npts {
		    x1 = real (ipix)
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(e) || IS_INDEF(y1))
			next
		    if (ylog)
			y1 = log10 (10**y1 - e)
		    else
			y1 = y1 - e

		    call arrow (gp, x1, y1, size, DOWN)
		}
	}
end


procedure igpserr (gp, xdata, ydata, edata, npts, errdir, size, style,
	xlog, ylog)

#  IGPSERR -- Draw error bars for X value versus Y value

pointer	gp
real	xdata[ARB], ydata[ARB]
real	edata[ARB]
int	npts
int	errdir
real	size
int	style
bool	xlog, ylog

int	ipix
real	x1, y1
real	x2, y2
real	e

begin
	if (errdir == 1) {
	    # Horizontal error bars (+X)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Horizontal line
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (xlog)
			x2 = log10 (10**x1 + e)
		    else
			x2 = x1 + e
		    y2 = y1

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Vertical tick
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (xlog)
			x1 = log10 (10**x1 + e)
		    else
			x1 = x1 + e

		    call gmark (gp, x1, y1, GM_VLINE, 0.0, size)
		}

	    if (style == UPPER_LOWER)
		# Right arrow
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (xlog)
			x1 = log10 (10**x1 + e)
		    else
			x1 = x1 + e

		    call arrow (gp, x1, y1, size, RIGHT)
		}

	} else if (errdir == 2) {
	    # Vertical error bars (+Y)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Vertical line
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    x2 = x1
		    if (ylog)
			y2 = log10 (10**y1 + e)
		    else
			y2 = y1 + e

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Horizonatal tick
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (ylog)
			y1 = log10 (10**y1 + e)
		    else
			y1 = y1 + e
		    call gmark (gp, x1, y1, GM_HLINE, size, 0.0)
		}

	    if (style == UPPER_LOWER)
		# Up arrow
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (ylog)
			y1 = log10 (10**y1 + e)
		    else
			y1 = y1 + e

		    call arrow (gp, x1, y1, size, UP)
		}

	} else if (errdir == -1 || errdir == 3) {
	    # Horizontal error bars (-X)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Horizontal line
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    y2 = y1
		    if (xlog)
			x2 = log10 (10**x1 - e)
		    else
			x2 = x1 - e

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Vertical tick
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (xlog)
			x1 = log10 (10**x1 - e)
		    else
			x1 = x1 - e

		    call gmark (gp, x1, y1, GM_VLINE, 0.0, size)
		}

	    if (style == UPPER_LOWER)
		# Left arrow
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (xlog)
			x1 = log10 (10**x1 - e)
		    else
			x1 = x1 - e

		    call arrow (gp, x1, y1, size, LEFT)
		}

	} else if (errdir == -2 || errdir == 4) {
	    # Vertical error bars (-Y)
	    if (style == BAR_TICK || style == BAR_ONLY)
		# Vertical line
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    x2 = x1
		    if (ylog)
			y2 = log10 (10**y1 - e)
		    else
			y2 = y1 - e

		    call gline (gp, x1, y1, x2, y2)
		}

	    if (style == BAR_TICK || style == TICK_ONLY)
		# Horizonatal tick
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (ylog)
			y1 = log10 (10**y1 - e)
		    else
			y1 = y1 - e
		    call gmark (gp, x1, y1, GM_HLINE, size, 0.0)
		}

	    if (style == UPPER_LOWER)
		# Down arrow
		do ipix = 1, npts {
		    x1 = xdata[ipix]
		    y1 = ydata[ipix]
		    e  = edata[ipix]
		    if (IS_INDEF(x1) || IS_INDEF(y1) || IS_INDEF(e))
			next
		    if (ylog)
			y1 = log10 (10**y1 - e)
		    else
			y1 = y1 - e

		    call arrow (gp, x1, y1, size, DOWN)
		}
	}
end
