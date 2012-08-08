include	<xwhen.h>
include	<config.h>
include	<mach.h>
include	<error.h>
include	<ctype.h>
include	<fset.h>	# FIO
include	<gset.h>	# GIO
include <tbset.h>	# TBtables

define	HELPFILE	"tables$pkg/ttools/gtedit/gtedit.key" # (BPS 05.31.94)
define	GT_QUIT		0
define	GT_EXIT		1

procedure gteplot (device, input, tp, tpr, deleted, xcolumn, ycolumn, x, y,
	  size, null, npix, table_name, status)

char	device[SZ_FNAME]	# Graphics device
char	input[SZ_FNAME]		# Input table name
pointer	tp			# Table descriptor
pointer	tpr			# Reject Table descriptor
pointer	deleted			# Pointer for array of delete flags
char	xcolumn[SZ_COLNAME]	# X column in table
char	ycolumn[SZ_COLNAME]	# Y column in table
pointer	x
pointer	y
pointer size			# Size of markers to plot
pointer null			# 
int	npix			# Number of points per curve
char	table_name[SZ_FNAME]	# Table name
int	status			# return status

pointer	gd
int	mode, npix_save
char	col_save[SZ_COLNAME]
char	xlabel[SZ_LINE], ylabel[SZ_LINE]
char	plotitle[2*SZ_LINE]
char	marker[SZ_FNAME]
char	cmd[SZ_LINE]
char	bad_column[SZ_COLNAME]
bool	xautoscale, yautoscale, mark_del
bool	drawbox, rdmarks
bool 	silent, readonly, inplace, auto_replot
int	xtran, ytran, ticklabels, marker_type, j, drawgrid
int	wcs, key, ip
int	undelete
real	px, py
real	wx1, wx2, wy1, wy2, szmarker, vx1, vx2, vy1, vy2
real	wb, wt, wl, wr
real	tol, xx, yy, sz
real	x1, y1, x2, y2
pointer	sp, system_id, errmsg

string	bell 	"\007"
define	replot_		91
define	next_		92

pointer	gopen()
bool	clgetb()
int	strncmp()
int	clgeti(), gstati()
int	clgcur()
real	clgetr()
pointer	tbtopn()
errchk	clgetb, clgeti, clgstr, clgetr, glabax, gpmark
errchk	gswind, gseti, gascale, grscale

begin
	call smark (sp)
	call salloc (system_id, SZ_LINE, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)

	mode = NEW_FILE
	mark_del = false
	undelete = NO

	# Get the marker character to be drawn at
	# each point.  The size of the character is given by szmarker. If
	# zero and the input operadd is a list, marker sizes are taken
	# individually from the third column of each list element.  If
	# negative, all markers are of size |szmarker| in NDC.  If
	# positive and the input operand is a list, the size of a marker
	# is the third column of each list element times szmarker. 

	szmarker  = 0.0
	rdmarks   = false

	# Draw markers only
	call clgstr ("marker", marker, SZ_FNAME)
	call init_mark2 (marker, marker_type)
	if (marker_type != GM_POINT) {
    	    szmarker = clgetr ("szmarker")
	    rdmarks = (szmarker <= 0)
	}

	gd = gopen (device, mode, STDGRAPH)

	call gsetr (gd, G_PLWIDTH, 2.0)

	tol = 10.0 * EPSILONR
	xautoscale = false
	yautoscale = false

	# Set window and viewport.  If user window has not been set, enable
	# autoscaling.  If device viewport has not been set, let glabax
	# handle the viewport internally.

	call gclear (gd)
	wx1 = clgetr ("wx1")
	wx2 = clgetr ("wx2")
	wy1 = clgetr ("wy1")
	wy2 = clgetr ("wy2")

	if (abs (wx2 - wx1) < tol) 
	    xautoscale = true
	if (abs (wy2 - wy1) < tol)
	    yautoscale = true

	vx1 = clgetr ("vx1")
	vx2 = clgetr ("vx2")
	vy1 = clgetr ("vy1")
	vy2 = clgetr ("vy2")

	if ((abs (vx2 - vx1) > tol) && (abs (vy2 - vy1) > tol))
	    call gsview (gd, vx1, vx2, vy1, vy2)

	if (!clgetb ("fill"))
	    call gseti (gd, G_ASPECT, 1)

	if (clgetb ("round"))
	    call gseti (gd, G_ROUND, YES)

replot_
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

	    call gt_hinfo (tp, xlabel, ylabel, xcolumn, ycolumn, SZ_LINE)

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

	# Log scale?  Call gswind to set log scaling regardless of whether
	# the user window is known; if the user window was not input,
	# autoscaling will reset it later.

	if (mode == APPEND) {
	    xtran = gstati (gd, G_XTRAN)
	    ytran = gstati (gd, G_YTRAN)
	    call ggwind (gd, wx1, wx2, wy1, wy2)
	} else {
	    xtran = GW_LINEAR
	    if (clgetb ("logx"))
		xtran = GW_LOG
	    ytran = GW_LINEAR
	    if (clgetb ("logy"))
		ytran = GW_LOG
	    call gswind (gd, wx1, wx2, wy1, wy2)
	    call gseti (gd, G_XTRAN, xtran)
	    call gseti (gd, G_YTRAN, ytran)
	}

	# Autoscale if enabled.
	if (xautoscale) {
	    call gascale (gd, Memr[x], npix, 1)
	} 
	call ggwind (gd, wl, wr, wb, wt)

	if (yautoscale) {
	    # Overplot multiple curves on the same viewport
            call gascale (gd, Memr[y], npix, 2)
	}

	if (drawbox) {
	    # Draw box around plot
	    call gseti (gd, G_LABELTICKS, ticklabels)
	    # Overplot multiple curves on the same viewport
	    call strcpy ("", plotitle, SZ_FNAME)
	    call glabax (gd, plotitle, xlabel, ylabel)
	}

	# Markers at each point with no connection
	if (rdmarks) {
	    # Variable marker sizes
 	    if (szmarker < 0)
	        # World coordinate marker sizes
	        call amulkr (Memr[size], -szmarker, Memr[size],
	            npix)
		do j = 1, npix {	# For each point in the curve
	    	    xx = Memr[x+j-1]
		    yy = Memr[y+j-1]
		    sz = Memr[size+j-1]
		    call gmark (gd, xx, yy, marker_type, sz, sz)
		}
	} else {
	   call gpmark (gd, Memr[x], Memr[y],npix, marker_type,
	      szmarker, szmarker)
	}

	# We have plotted things so now is the time to let the user
	# do his thing.

	# Over plot crosses for those points which have been deleted
	if (mark_del) {
	    call gt_pdel (gd, Memr[x], Memr[y], Memr[deleted], npix)
	    mark_del = false
	}

next_
	while (clgcur ("commands", px, py, wcs, key, cmd, SZ_LINE) 
		!= EOF) {

	    switch (key) {

	    # Quit and do not make changes
	    case 'q':
		status = GT_QUIT
		break

	    # Exit and do the changes
	    case 'e':
		status = GT_EXIT
		break

	    # Help page
	    case '?':
		if (gd == NULL)
		    call pagefile (HELPFILE, "")
		else 
		    call gpagefile (gd, HELPFILE, "")

	    # Simply replot (may have new columns)
	    case 'p':
		call gclear (gd)
		mark_del = true
		goto replot_

	    # Mark the corners of a box and delete the points within
	    case 'c':
		x1 = px; y1 = py
		call gmark (gd, x1, y1, GM_DIAMOND, 1., 1.)
		call printf ("again:")
		if (clgcur ("commands", px, py, wcs, key, cmd, SZ_LINE) == EOF)
		    goto next_
		
		call gt_dbox (gd, npix, Memi[deleted], undelete, Memr[x], 
			      Memr[y], x1, y1, px, py)

	    # Mark the end points of a line segment and delete points on
	    # one side of this segment (indicated by user) for points with
	    # X values between x1 and x2
	    case 's':
		x1 = px; y1 = py
		call gmark (gd, x1, y1, GM_DIAMOND, 1., 1.)
		call printf ("again:")
		if (clgcur ("commands", px, py, wcs, key, cmd, SZ_LINE) == EOF)
		    goto next_
		x2 = px
		y2 = py
		call gmark (gd, x1, y1, GM_DIAMOND, 1., 1.)
		call gline (gd, x1, y1, x2, y2)
		call printf ("Move cursor to one side of line and hit any key")
		if (clgcur ("commands", px, py, wcs, key, cmd, SZ_LINE) == EOF)
		    goto next_
		
		call gt_dseg (gd, npix, Memi[deleted], undelete, Memr[x], 
			      Memr[y], x1, y1, x2, y2, px, py)

	    # Update graph (delete points and replot)
	    case 'f':
		call gt_update (tp, tpr, Memr[x], Memr[y], Memi[deleted], npix)
		call gclear (gd)
		goto replot_

	    # Print out the complete record for this point
	    case 'a':
		call gt_wrdata (gd, tp, px, py, Memr[x], Memr[y], npix)

	    # Print out the column names
	    case 'h':
		call gt_wrhead (gd, tp)

	    # Delete a point
	    case 'd':
		call gt_delpt (gd,px, py, Memr[x], Memr[y], npix, 
			       Memi[deleted], undelete)

	    # Undelete a point
	    case 'u':
		if (undelete == YES) {
		    undelete = NO
		    call printf ("Now deleting points\n")
		} else {
		    undelete = YES
		    call printf ("Now undeleting points\n")
		}

	    # Revert to normal table editor
	    case 'v':
		silent = false
		readonly = false
		inplace = false

		# First update the table
		call gt_update (tp, tpr, Memr[x], Memr[y], Memi[deleted], npix)
		call tbtclo (tp)
		call gdeactivate (gd, 0)
		call edit (table_name, " ", silent, readonly, inplace)

		# Now read in the data (which may have been edited
		npix_save = npix
		tp = tbtopn (table_name, READ_WRITE, NULL)
		call gt_rdxycol (tp, xcolumn, ycolumn, x, y, size, null, npix,
		    bad_column)
		if (npix < 0) {
		    npix = npix_save
		    call printf ("Cannot find column %s")
			call pargstr (bad_column)
		}
		call greactivate (gd, AW_PAUSE)
		call gclear (gd)
		goto replot_

	    # Undelete status
	    case 'z':
		if (undelete == NO) {
		    call printf ("Currently deleting points\n")
		} else {
		    call printf ("Currently undeleting points\n")
		}

	    # Delete points > Y
	    case 't':
		call gt_dygt (gd, py, Memr[x], Memr[y], npix, 
		Memi[deleted], undelete)

	    # Delete points < Y
	    case 'b':
		call gt_dylt (gd, py, Memr[x], Memr[y], npix, 
		Memi[deleted], undelete)

	    # Delete points > X
	    case 'r':
		call gt_dxgt (gd, px, Memr[x], Memr[y], npix, 
		Memi[deleted], undelete)

	    # Delete points < X
	    case 'l':
		call gt_dxlt (gd, px, Memr[x], Memr[y], npix, 
		Memi[deleted], undelete)

	    # Colon commands:
	    case ':':
		# Command mode
		for (ip=1; IS_WHITE (cmd[ip]); ip = ip + 1)
		    ;

	   	switch (cmd[ip]) {
		case 'x':
		    # Read in a new X column
		    ip = ip + 1
		    auto_replot = true
		    if (strncmp (cmd[ip], "-", 1) == 0) {
			ip = ip + 1
			auto_replot = false
		    }
		    call amovc (xcolumn, col_save, SZ_COLNAME)
		    call ctowrd (cmd, ip, xcolumn, SZ_FNAME)
		    npix_save = npix
		    call gt_rdxycol (tp, xcolumn, ycolumn, x, y, size, null,
			npix, bad_column)
		    if (npix < 0) {
			npix = npix_save
		        call gdeactivate (gd, 0)
		        call printf ("Cannot find column %s")
			    call pargstr (bad_column)
		        call greactivate (gd, 0)
		        call amovc (col_save, xcolumn, SZ_COLNAME)
		    }
		    if (auto_replot) {
		        call gclear (gd)
		        mark_del = true
		        goto replot_
		    }

		case 'y':
		    # Read in a new Y column
		    ip = ip + 1
		    auto_replot = true
		    if (strncmp (cmd[ip], "-", 1) == 0) {
			ip = ip + 1
			auto_replot = false
		    }
		    call amovc (ycolumn, col_save, SZ_COLNAME)
		    call ctowrd (cmd, ip, ycolumn, SZ_FNAME)
		    npix_save = npix
		    call gt_rdxycol (tp, xcolumn, ycolumn, x, y,
			size, null, npix, bad_column)
		    if (npix < 0) {
			call gdeactivate (gd, 0)
			npix = npix_save
		        call printf ("Cannot find column %s")
			    call pargstr (bad_column)
		        call greactivate (gd, 0)
		        call amovc (col_save, ycolumn, SZ_COLNAME)
		    }
		    if (auto_replot) {
		        call gclear (gd)
		        mark_del = true
		        goto replot_
		    }

		default:
		    call printf (bell)

		}

	    default:
		call printf (bell)

	    }
	}
	call sfree (sp)
	call gclose (gd)
end

# TGR_ONINT2 -- Interrupt handler for the task graph.  Branches back to ZSVJMP
# in the main routine to permit shutdown without an error message.

procedure tgr_onint2 (vex, next_handler)

int	vex			# Virtual exception
int	next_handler		# not used

int	tgrjmp[LEN_JUMPBUF]
common	/tgrcom/ tgrjmp

begin
	call xer_reset()
	call zdojmp (tgrjmp, vex)
end


# INIT_MARK2 -- Returns integers code for marker type string.

procedure init_mark2 (marker, imark)

char	marker[SZ_FNAME]	# Marker type as a string
int	imark			# Integer code for marker - returned

bool	streq()

begin
	if (streq (marker, "point"))
	    imark = GM_POINT
	else if (streq (marker,   "box"))
	    imark = GM_BOX
	else if (streq (marker,  "plus"))
	    imark = GM_PLUS
	else if (streq (marker, "cross"))
	    imark = GM_CROSS
	else if (streq (marker, "circle"))
	    imark = GM_CIRCLE
	else if (streq (marker, "hebar"))
	    imark = GM_HEBAR
	else if (streq (marker, "vebar"))
	    imark = GM_VEBAR
	else if (streq (marker, "hline"))
	    imark = GM_HLINE
	else if (streq (marker, "vline"))
	    imark = GM_VLINE
	else if (streq (marker, "diamond"))
	    imark = GM_DIAMOND
	else {
	    call eprintf ("Unrecognized marker type, using 'box'\n")
	    imark = GM_BOX
	}
end
