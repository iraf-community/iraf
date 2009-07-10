include <gset.h>
include "sgraph.h"

procedure ggplot (device, mode, input, x, y, size, npix, ncurves)

# GGPLOT -- Does the real work of making the graph, after the graphics
# devics has been opened.  Fetch remaining parameters, read in the data,
# and make the plot.
#
#  7/20/90, Added sysid parameter to optionally write system id in plot
#  title, ZGL.
#
#  1/31/91 Changed the error bar plotting to interpret the error value
#  as the half-width of the error bars.  ZGL
#
#  9/9/91  Changed clgstr() to clgwrd() to minimum-match
#  curve styles from the dictionary.  Z.G. Levay
#
#  9/10/91  Changed line patterns to minimum match from dictionary and
#  use symbolic constants.  ZGL
#
#  9/13/91  Modified marker drawing to adjust size for non-square
#  devices.  ZGL
#  7/16/93  Change window and viewport parameter names.

char	device[SZ_FNAME]	# Graphics device
int	mode			# Mode of graphics stream
char	input[SZ_LINE]		# List of operands to be plotted
pointer	x[MAX_CURVES]		# X values
pointer y[MAX_CURVES]		# Y values
pointer size[MAX_CURVES]	# Size of markers to plot
int	npix[MAX_CURVES]	# Number of points per curve
int	ncurves			# Number of curves to overplot

pointer	gd
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE]
char	plotitle[2*SZ_LINE]
char	marker[SZ_FNAME]
pointer	crvstyle
int	icrvst
bool	pointmode, lintran, xautoscale, yautoscale
bool	drawbox, transpose, rdmarks
int	xtran, ytran, axis, ticklabels, i, marker_type, j, drawgrid
real	p1, p2, q1, q2, wx1, wx2, wy1, wy2, szmarker, vx1, vx2, vy1, vy2
real	vb, vt, dvy
real	wb, wt, wl, wr
real	xx, yy, sz
pointer	ptemp
pointer	sp, linepat, system_id, errmsg
int	lnpati
int	erraxis
bool	stack		# Plot multiple curves on stacked viewports?
bool	yflip		# Flip Y axis?
int	repld		# Number of points replaced with INDEF
real	margin		# Margin between curve and axis (fraction of window)
real	xmsize, ymsize	# Marker sizes
real	xms, yms	# Marker sizes
int	crvcolor		# Color index of data curve(s)
int	color		# Color index for everything else
bool	cycolor		# Cycle colors for multiple curve?
int	fillpat		# Fill pattern

string	xformat "\n%d X values rejected in curve %d "
string	yformat "\n%d Y values rejected in curve %d "

string	lnpdict	"|solid|dashed|dotted|dotdash|"
string	crvdict	"|straight|pseudohist|fullhist|"

pointer	gopen()
bool	clgetb(), fp_equalr(), streq()
int	clgeti(), gg_rdcurves(), gg_relindef(), clgwrd(), getpat(), gstati()
real	clgetr()

errchk	clgetb, clgeti, clgstr, clgetr, gpmark
errchk	gg_setdashpat, gswind, gseti, gg_rdcurves, gascale, grscale
errchk	gopen

begin
	call smark (sp)
	call salloc (linepat,   SZ_LINE, TY_CHAR)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (errmsg,    SZ_LINE, TY_CHAR)
	call salloc (crvstyle,  SZ_LINE, TY_CHAR)

	# If computing projection along an axis (collapsing a multidimensional
	# section to a vector), fetch axis number.
	axis = clgeti ("axis")

	# If pointmode is enabled, get the marker character to be drawn at
	# each point.  The size of the character is given by szmarker. If
	# zero and the input operand is a list, marker sizes are taken
	# individually from the third column of each list element.  If
	# negative, all markers are of size |szmarker| in NDC.  If
	# positive and the input operand is a list, the size of a marker
	# is the third column of each list element times szmarker. 

	pointmode = clgetb ("pointmode")
	erraxis   = clgeti ("erraxis")
	szmarker  = 0.0
	rdmarks   = false

	if (pointmode) {
	    # Draw markers only
	    call clgstr ("marker", marker, SZ_FNAME)
	    call init_marker (marker, marker_type)
	    if (marker_type != GM_POINT) {
		szmarker = clgetr ("szmarker")
		rdmarks  = (szmarker == 0.)
	    }

	} else if (erraxis == 1 || erraxis == 2) {
	    # Variable size error bars
	    rdmarks = true

	} else {
	    # Connected points without markers;  

#  10 Sept. 91  Changed clgstr() to clgwrd() to minimum-match on line
#  patterns from the dictionary.  Z.G. Levay

	    # Initialize the dashline index
	    lnpati = clgwrd ("pattern", Memc[linepat], SZ_LINE, lnpdict)
	    call gg_initdashpat (lnpati)

#  9 Sept. 91  Changed clgstr() to clgwrd() to minimum-match
#  curve styles from the dictionary.  Z.G. Levay

	    # Initialize the curve style (connected, histogram, etc.)
	    icrvst = clgwrd ("crvstyle", Memc[crvstyle], SZ_LINE, crvdict)
	}

	# Read all the curves specified by the operands in input into memory.
	ncurves = gg_rdcurves (input, x, y, size, npix, 
		               axis, rdmarks, erraxis)

	gd = gopen (device, mode, STDGRAPH)

#  I don't know why this was here.  It caused `append'ed curves to be
#  plotted with wide lines.  The glabax call resets G_PLWIDTH to 1.
#  One should be able to specify a dash pattern for appended curves
#  explicitly.
#  13 November 1989, Z. G. Levay, STScI
#	call gsetr (gd, G_PLWIDTH, 2.0)

	xautoscale = false
	yautoscale = false

	# Set window and viewport.  If user window has not been set, enable
	# autoscaling.  If device viewport has not been set, let glabax
	# handle the viewport internally.

	if (mode != APPEND) {
	    call gclear (gd)
	    wx1 = clgetr ("wl")
	    wx2 = clgetr ("wr")
	    wy1 = clgetr ("wb")
	    wy2 = clgetr ("wt")
 
	    if (fp_equalr (wx1, wx2)) 
		xautoscale = true

	    if (fp_equalr (wy1, wy2))
		yautoscale = true

	    vx1 = clgetr ("left")
	    vx2 = clgetr ("right")
	    vy1 = clgetr ("bottom")
	    vy2 = clgetr ("top")

	    if (!(fp_equalr (vx1, vx2)) && !(fp_equalr (vy1, vy2)))
	        call gsview (gd, vx1, vx2, vy1, vy2)

	    if (!clgetb ("fill"))
	        call gseti (gd, G_ASPECT, 1)

	    if (clgetb ("round"))
	        call gseti (gd, G_ROUND, YES)
	}

	# Get the color index and set all possible color parameters
	color    = clgeti ("color")
	crvcolor = clgeti ("crvcolor")
	cycolor  = clgetb ("cycolor")

	if (IS_INDEFI(color))
	    #  Default
	    color = DEF_COLOR

	if (IS_INDEFI(crvcolor))
	    #  Default
	    crvcolor = DEF_COLOR

	# Get the bar ("histogram") fill pattern
	fillpat = getpat ("barpat")

	# Draw box around plot?
	drawbox = false
	if (mode != APPEND)
	    if (clgetb ("box"))
		drawbox = true

	if (drawbox) {
	    # Get number of major and minor tick marks.
	    call gseti (gd, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gd, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gd, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gd, G_YNMINOR, clgeti ("minry"))

	    # Fetch labels and plot title string. 

	    call clgstr ("xlabel", xlabel, SZ_LINE)
	    call clgstr ("ylabel", ylabel, SZ_LINE)

	    call clgstr ("title", title, SZ_LINE)
	    if (streq (title, "imtitle"))
		call gg_get_imtitle (input, 
		    title, xlabel, ylabel, SZ_LINE)

	    if (clgetb ("sysid")) {
		call sysid (Memc[system_id], SZ_LINE)
		call sprintf (plotitle, 2*SZ_LINE, "%s\n%s")
		    call pargstr (Memc[system_id])
		    call pargstr (title)
	    } else
		call strcpy (title, plotitle, SZ_LINE)

	    # Label tick marks on axes?
	    ticklabels = NO
	    if (clgetb ("ticklabels"))
		ticklabels = YES

	    # Draw grid ?
	    drawgrid = NO
	    if (clgetb ("grid"))
		drawgrid = YES

	    call gseti (gd, G_DRAWGRID, drawgrid)
	}

	# Perform linear transformation on the X axis?
	lintran = clgetb ("lintran")
	if (lintran) {
	    p1 = clgetr ("p1")
	    p2 = clgetr ("p2")
	    q1 = clgetr ("q1")
	    q2 = clgetr ("q2")
	}

	# Transpose X,Y axes?
	transpose = clgetb ("transpose")

	# Log scale?  Call gswind to set log scaling regardless of whether
	# the user window is known; if the user window was not input,
	# autoscaling will reset it later.

	if (mode == APPEND) {
	    # Append to existing plot;  use that WCS
	    xtran = gstati(gd, G_XTRAN)
	    ytran = gstati(gd, G_YTRAN)
	    call ggwind (gd, wx1, wx2, wy1, wy2)

	} else {
	    # Set the WCS
	    xtran = GW_LINEAR
	    if (clgetb ("logx"))
		xtran = GW_LOG

	    ytran = GW_LINEAR
	    if (clgetb ("logy"))
		ytran = GW_LOG

	    call gseti (gd, G_XTRAN, xtran)
	    call gseti (gd, G_YTRAN, ytran)
	    call gswind (gd, wx1, wx2, wy1, wy2)
	}

	# Carry out linear transformation on X coords, if desired.
	if (lintran)
	    do i = 1, ncurves
		call gg_lintran (Memr[x[i]], npix[i], p1,p2, q1,q2)

	if (transpose)

	# Swap axes, if enabled.  Note that the linear transformation of
	# the x-axis should be performed before axes are swapped.  This is
	# because the purpose of the lintran option is to provide a means
	# of assigning a coordinate system to a pixel array.

	    do i = 1, ncurves {
		ptemp = x[i]
		x[i] = y[i]
		y[i] = ptemp
	    }

	if (clgetb ("rejectlog")) {
	    # Replace invalid (<= 0) log values by INDEF
	    if (xtran == GW_LOG)
		do i = 1, ncurves {
		    repld = gg_relindef (Memr[x[i]], npix[i])
		    if (repld > 0) {
			call sprintf (Memc[errmsg], SZ_LINE, xformat)
			    call pargi (repld)
			    call pargi (i)
			call eprintf (Memc[errmsg])
		    }
		}

	    if (ytran == GW_LOG)
		do i = 1, ncurves {
		    repld = gg_relindef (Memr[y[i]], npix[i])
		    if (repld > 0) {
			call sprintf (Memc[errmsg], SZ_LINE, yformat)
			    call pargi (repld)
			    call pargi (i)
			call eprintf (Memc[errmsg])
		    }
		}
	}

	# Autoscale if enabled.
	if (xautoscale) {
	    call gascale (gd, Memr[x[1]], npix[1], 1)
	    if (ncurves > 1) {
		do i = 2, ncurves
		    call grscale (gd, Memr[x[i]], npix[i], 1)
	    }
	    if (clgetb ("xflip")) {
		# Flip the X axis
		call ggwind (gd, wx1, wx2, wy1, wy2)
		call gswind (gd, wx2, wx1, wy1, wy2)
	    }
	} 
	call ggwind (gd, wl, wr, wb, wt)

	yflip = clgetb ("yflip")
	stack = clgetb ("stack") && (mode != APPEND)

	if (yautoscale) {
	    if (!stack) {
		# Overplot multiple curves on the same viewport
		call gascale (gd, Memr[y[1]], npix[1], 2)
		if (ncurves > 1)
		    do i = 2, ncurves
			call grscale (gd, Memr[y[i]], npix[i], 2)

		if (yflip) {
		    # Flip the Y axis
		    call ggwind (gd, wx1, wx2, wy1, wy2)
		    call gswind (gd, wx1, wx2, wy2, wy1)
		}
	    }
	}

	margin = clgetr ("margin")
	if (IS_INDEFR(margin))
	    margin = DEF_MARG

	if (drawbox) {
	    # Draw box around plot
	    call gseti (gd, G_LABELTICKS, ticklabels)

	    # Set the color
	    call gseti (gd, G_PLCOLOR, color)
	    call gseti (gd, G_PMCOLOR, color)
	    call gseti (gd, G_FACOLOR, color)
	    call gseti (gd, G_TXCOLOR, color)

	    if (stack) {
		# Stack viewports for multiple curves
		#call gseti  (gd, G_WCS, 15)
		call gseti  (gd, G_XDRAWTICKS, YES)
		call gseti  (gd, G_YDRAWTICKS, NO)
		call gseti  (gd, G_XTRAN, xtran)
		call gseti  (gd, G_YTRAN, ytran)
		call gswind (gd, wl, wr, INDEF, INDEF)
		call axmarg (gd, margin, plotitle, xlabel, EOS)
#		Draw X axes between viewports?
#		commented ==> draw them
#		call gseti  (gd, G_XDRAWAXES, 0)
		call gseti  (gd, G_XLABELTICKS, NO)
		call gseti  (gd, G_YDRAWTICKS, YES)
		call ggview (gd, vx1, vx2, vy1, vy2)
		dvy = (vy2 - vy1) / ncurves
		vb = vy1
	    } else {
		# Overplot multiple curves on the same viewport
		call axmarg (gd, margin, plotitle, xlabel, ylabel)
	    }
	}

	# Draw the curves.
	do i = 1, ncurves {
	    # For each curve
	    if (stack) {
		# Stack viewports for multiple curves
		call gseti (gd, G_WCS, i)
		call gseti (gd, G_XTRAN, xtran)
		call gseti (gd, G_YTRAN, ytran)
		vt = vb + dvy
		call gsview (gd, vx1, vx2, vb, vt)
		vb = vt
		call gswind  (gd, wl, wr, wy1, wy2)
		if (yautoscale)
		    call gascale (gd, Memr[y[i]], npix[i], 2)
		if (yflip) {
		    call ggwind (gd, wx1, wx2, wy1, wy2)
		    call gswind (gd, wx1, wx2, wy2, wy1)
		}

		# Set the axis color
		call gseti (gd, G_PLCOLOR, color)
		call gseti (gd, G_PMCOLOR, color)
		call gseti (gd, G_FACOLOR, color)
		call gseti (gd, G_TXCOLOR, color)

		# Draw the axis
		call axmarg (gd, margin, EOS, EOS, ylabel)
	    }

	    call gseti (gd, G_PLCOLOR, crvcolor)
	    call gseti (gd, G_PMCOLOR, crvcolor)
	    call gseti (gd, G_FACOLOR, crvcolor)
	    call gseti (gd, G_TXCOLOR, crvcolor)

	    if (cycolor && !stack)
		# Multiple curves on the same viewport
		# Cycle the color
		crvcolor = mod (crvcolor, 8) + 1

	    if (pointmode) {
		# Markers at each point with no connection

		# Compute the size adjustment to the device aspect ratio
		call fmsize (gd, 1.0, xmsize, ymsize)

		if (!rdmarks)
		    # Use the same size marker for every point
#		    call amovkr (szmarker, Memr[size[i]], npix[i])
#
#  Removed the constant size outside the loop  22 April 1992 ZGL
#
		    sz = szmarker

#		    if (szmarker < 0)
#			# World coordinate marker sizes
#		        call aabsr (Memr[size[i]], Memr[size[i]], npix[i])

		    do j = 1, npix[i] {
			# For each point in the curve
			xx = Memr[x[i]+j-1]
			yy = Memr[y[i]+j-1]
			if (rdmarks)
			    sz = Memr[size[i]+j-1]

			if (!IS_INDEF(sz) && !IS_INDEF(xx) && !IS_INDEF(yy)) {
			    if (marker_type == GM_HEBAR)
				call gmark (gd, xx, yy, marker_type,
				    2.0*sz, 1.0)

			    else if (marker_type == GM_VEBAR)
				call gmark (gd, xx, yy, marker_type,
				    1.0, 2.0*sz)

			    else {
				# Correct for device aspect
				xms = sz * xmsize
				yms = sz * ymsize

				# Draw the marker
				call gmark (gd, xx, yy, marker_type, xms, yms)
			    }
			}
		    }

	    } else if (erraxis == 1) {
		# Variable size horizontal error bars
		do j = 1, npix[i] {
		    # For each point in the curve
#		    sz = 2.0 * Memr[size[i]+j-1]
		    sz = Memr[size[i]+j-1]

		    # Ignore INDEF size
		    if (!IS_INDEF(sz)) {
			xx = Memr[x[i]+j-1]
			yy = Memr[y[i]+j-1]
			call gmark (gd, xx, yy, GM_HEBAR, -sz, 1.0)
		    }
	        }

	    } else if (erraxis == 2) {
		# Variable size vertical error bars
		do j = 1, npix[i] {
		    # For each point in the curve
#		    sz = 2.0 * Memr[size[i]+j-1]
		    sz = Memr[size[i]+j-1]

		    # Ignore INDEF size
		    if (!IS_INDEF(sz)) {
			xx = Memr[x[i]+j-1]
			yy = Memr[y[i]+j-1]
			call gmark (gd, xx, yy, GM_VEBAR, 1.0, -sz)
		    }
	        }

	    } else {
		# Connect the dots
		if (!stack && !cycolor)
		    # Multiple curves on the same viewport
		    # Cycle the dash pattern
		    call gg_setdashpat (gd)

		switch (icrvst) {
		# Select curve style

		case CRV_STRT:
		    # Straight connections
		    call gpline (gd, Memr[x[i]], Memr[y[i]], npix[i])

		case CRV_PSEUH:
		    # "Pseudo-histogram" or stepped curve
		    call hgline (gd, Memr[x[i]], Memr[y[i]], npix[i])

		case CRV_FULLH:
		    # "Full histogram"
		    call fhglin (gd, Memr[x[i]], Memr[y[i]], npix[i], fillpat)

		default:
		    call gpline (gd, Memr[x[i]], Memr[y[i]], npix[i])
		}
	    }
	}

	call sfree (sp)
	call gclose (gd)
end
