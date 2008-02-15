# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"imexam.h"

 
# IE_LIMEXAM -- Make a line plot
# If the line is INDEF then use the last line.

procedure ie_limexam (gp, mode, ie, y)

pointer	gp		# GIO pointer
int	mode		# Mode
pointer	ie		# Structure pointer
real	y		# Line

real	yavg, junk
int	i, x1, x2, y1, y2, nx, ny, npts
pointer	sp, title, im, data, ptr, xp, yp

int	clgpseti()
pointer	clopset(), ie_gimage(), ie_gdata()

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	if (IE_PP(ie) != NULL)
	    call clcpset (IE_PP(ie))
	IE_PP(ie) = clopset ("limexam")

	if (!IS_INDEF(y))
	    IE_Y1(ie) = y

	ny = clgpseti (IE_PP(ie), "naverage")
	x1 = INDEFI
	x2 = INDEFI
	y1 = IE_Y1(ie) - (ny - 1) / 2 + 0.5
	y2 = IE_Y1(ie) + ny / 2 + 0.5
	yavg = (y1 + y2) / 2.
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	call smark (sp)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (xp, nx, TY_REAL)

	do i = 1, nx
	    call ie_mwctran (ie, real(i), yavg, Memr[xp+i-1], junk)

	if (ny > 1) {
	    ptr = data
	    call salloc (yp, nx, TY_REAL)
	    call amovr (Memr[ptr], Memr[yp], nx)
	    do i = 2, ny {
	        ptr = ptr + nx
	        call aaddr (Memr[ptr], Memr[yp], Memr[yp], nx)
	    }
	    call adivkr (Memr[yp], real (ny), Memr[yp], nx)
	} else
	    yp = data

	call sprintf (Memc[title], IE_SZTITLE, "%s: Lines %d - %d\n%s")
	    call pargstr (IE_IMNAME(ie))
	    call pargi (y1)
	    call pargi (y2)
	    call pargstr (IM_TITLE(im))

	call ie_graph (gp, mode, IE_PP(ie), Memc[title], Memr[xp],
	    Memr[yp], nx, IE_XLABEL(ie), IE_XFORMAT(ie))

	call sfree (sp)
end
