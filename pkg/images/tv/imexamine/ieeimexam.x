# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<gset.h>
include	<config.h>
include	<mach.h>
include	<imhdr.h>
include	<xwhen.h>
include	<fset.h>
include	"imexam.h"


# IE_EIMEXAM -- Contour map
# This is an interface to the NCAR CONREC routine.

procedure ie_eimexam (gp, mode, ie, x, y)
 
pointer	gp			# GIO pointer
int	mode			# Mode
pointer	ie			# IE pointer
real	x, y			# Center

bool	banner
int	nset, ncontours, dashpat, nhi
int	x1, x2, y1, y2, nx, ny, npts, wkid
real	vx1, vx2, vy1, vy2, xs, xe, ys, ye
real	interval, floor, ceiling, zero, finc, zmin, zmax
pointer	sp, title, hostid, user, xlabel, ylabel, im, data, data1

pointer	pp, clopset(), ie_gdata(), ie_gimage()
bool	clgpsetb(), fp_equalr()
int	clgpseti(), btoi()
real	clgpsetr()

int	isizel, isizem, isizep, nrep, ncrt, ilab, nulbll, ioffd
int	ioffm, isolid, nla, nlm
real	xlt, ybt, side, ext, hold[5]
common  /conre4/ isizel, isizem , isizep, nrep, ncrt, ilab, nulbll, 
            ioffd, ext, ioffm, isolid, nla, nlm, xlt, ybt, side
int	first
common  /conflg/ first
common  /noaolb/ hold

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	pp = IE_PP(ie)
	if (pp != NULL)
	    call clcpset (pp)
	pp = clopset ("eimexam")
	IE_PP(ie) = pp

	if (!IS_INDEF(x))
	    IE_X1(ie) = x
	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	nx = clgpseti (pp, "ncolumns")
	ny = clgpseti (pp, "nlines")
	x1 = IE_X1(ie) - (nx - 1) / 2 + 0.5
	x2 = IE_X1(ie) + nx / 2 + 0.5
	y1 = IE_Y1(ie) - (ny - 1) / 2 + 0.5
	y2 = IE_Y1(ie) + ny / 2 + 0.5
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}

	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny
	xs = x1
	xe = x2
	ys = y1
	ye = y2

	call smark (sp)
	banner = false
	if (mode == NEW_FILE) {
	    call gclear (gp)

	    # Set the WCS
	    call gswind (gp, xs, xe, ys, ye)

	    if (!clgpsetb (pp, "fill"))
	        call gsetr (gp, G_ASPECT, real (ny-1) / real (nx-1))

	    call gseti (gp, G_ROUND, btoi (clgpsetb (pp, "round")))

	    if (clgpsetb (pp, "box")) {
	        # Get number of major and minor tick marks.
	        call gseti (gp, G_XNMAJOR, clgpseti (pp, "majrx"))
	        call gseti (gp, G_XNMINOR, clgpseti (pp, "minrx"))
	        call gseti (gp, G_YNMAJOR, clgpseti (pp, "majry"))
	        call gseti (gp, G_YNMINOR, clgpseti (pp, "minry"))

	        # Label tick marks on axes?
	        call gseti (gp, G_LABELTICKS,
		    btoi (clgpsetb (pp, "ticklabels")))

		# Labels
		call salloc (title, IE_SZTITLE, TY_CHAR)
		call salloc (hostid, SZ_LINE, TY_CHAR)
		call salloc (user, SZ_LINE, TY_CHAR)
		call salloc (xlabel, SZ_LINE, TY_CHAR)
		call salloc (ylabel, SZ_LINE, TY_CHAR)

		banner = clgpsetb (pp, "banner")
		if (banner) {
		    call sysid (Memc[hostid], SZ_LINE)
		    # We must postpone the parameter line until after conrec.
		    call sprintf (Memc[title], IE_SZTITLE, "%s\n\n%s")
			call pargstr (Memc[hostid])
			call pargstr (IM_TITLE(im))
		} else
		    Memc[title] = EOS

		call clgpset (pp, "title", Memc[user], SZ_LINE)
		if (Memc[user] != EOS) {
		    call strcat ("\n", Memc[title], IE_SZTITLE)
		    call strcat (Memc[user], Memc[title], IE_SZTITLE)
		}
		call clgpset (pp, "xlabel", Memc[xlabel], SZ_LINE)
		call clgpset (pp, "ylabel", Memc[ylabel], SZ_LINE)

	        call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	    }
	}

	# First of all, intialize conrec's block data before altering any
	# parameters in common.
	first = 1
	call conbd

	# Set contour parameters
	zero	= clgpsetr (pp, "zero")
	floor	= clgpsetr (pp, "floor")
	ceiling	= clgpsetr (pp, "ceiling")
	nhi	= clgpseti (pp, "nhi")
	dashpat	= clgpseti (pp, "dashpat")

	# Resolve INDEF limits.
	if (IS_INDEF (floor) || IS_INDEF (ceiling)) {
	    call alimr (Memr[data], npts, zmin, zmax)
	    if (IS_INDEF (floor))
	        floor = zmin
	    if (IS_INDEF (ceiling))
	        ceiling = zmax
	}

	# Apply the zero point shift.
	if (abs (zero) > EPSILON) {
	    call salloc (data1, npts, TY_REAL)
	    call asubkr (Memr[data], zero, Memr[data1], npts)
	    floor = floor - zero
	    ceiling = ceiling - zero
	} else
	    data1 = data

	# Avoid conrec's automatic scaling.
	if (floor == 0.)
	    floor = EPSILON
	if (ceiling == 0.)
	    ceiling = EPSILON

	# The user can suppress the contour labelling by setting the common
	# parameter "ilab" to zero.
	if (btoi (clgpsetb (pp, "label")) == NO)
	    ilab = 0
	else
	    ilab = 1

	# User can specify either the number of contours or the contour
	# interval, or let conrec pick a nice number.  Get params and
	# encode the FINC param expected by conrec.

	ncontours = clgpseti (pp, "ncontours")
	if (ncontours <= 0) {
	    interval = clgpsetr (pp, "interval")
	    if (interval <= 0)
		finc = 0
	    else
		finc = interval
	} else
	    finc = - abs (ncontours)

	# Open device and make contour plot.
	call gopks (STDERR)
	wkid = 1
	call gopwk (wkid, 6, gp)
	call gacwk (wkid)

	# Make the contour plot.
	nset = 1	# No conrec viewport
	ioffm = 1	# No conrec box
	call gswind (gp, 1., real (nx), 1., real (ny))
	call ggview (gp, vx1, vx2, vy1, vy2)
	call set (vx1, vx2, vy1, vy2, 1.0, real (nx), 1.0, real (ny), 1)
	call conrec (Memr[data1], nx, nx, ny, floor,
	    ceiling, finc, nset, nhi, -dashpat)

	call gdawk (wkid)
	call gclks ()

	call gswind (gp, xs, xe, ys, ye)
	if (banner) {
	    if (fp_equalr (hold(5), 1.0)) {
	        call sprintf (Memc[title], IE_SZTITLE, 
    "%s\n%s: Contoured from %g to %g, interval = %g\n%s")
		    call pargstr (Memc[hostid])
		    call pargstr (IE_IMNAME(ie))
	            call pargr (hold(1))
	            call pargr (hold(2))
	            call pargr (hold(3))
		    call pargstr (IM_TITLE(im))
	    } else {
	        call sprintf (Memc[title], IE_SZTITLE, 
    "%s\n%s:contoured from %g to %g, interval = %g, labels scaled by %g\n%s")
		    call pargstr (Memc[xlabel])
		    call pargstr (IE_IMNAME(ie))
	            call pargr (hold(1))
	            call pargr (hold(2))
	            call pargr (hold(3))
		    call pargr (hold(5))
		    call pargstr (IM_TITLE(im))
	    }

	    if (Memc[user] != EOS) {
		call strcat ("\n", Memc[user], IE_SZTITLE)
		call strcat (Memc[user], Memc[title], IE_SZTITLE)
	    }

	    call gseti (gp, G_DRAWAXES, NO)
	    call glabax (gp, Memc[title], "", "")

	} else
	    call gtext (gp, xs, ys, "", "")

	call sfree (sp)
end
