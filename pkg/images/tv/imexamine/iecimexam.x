# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<error.h>
include	"imexam.h"
 
# IE_CIMEXAM -- Column plot
# If the input column is INDEF use the last column.

procedure ie_cimexam (gp, mode, ie, x)

pointer	gp		# GIO pointer
int	mode		# Mode
pointer	ie		# Structure pointer
real	x		# Column

real	xavg, junk
int	i, x1, x2, y1, y2, nx, ny, npts
pointer	sp, title, im, data, ptr, xp, yp

real	asumr()
int	clgpseti()
pointer	clopset(), ie_gimage(), ie_gdata()
errchk	clcpset, clopset

begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	if (IE_PP(ie) != NULL)
	    call clcpset (IE_PP(ie))
	IE_PP(ie) = clopset ("cimexam")

	if (!IS_INDEF(x))
	    IE_X1(ie) = x

	nx = clgpseti (IE_PP(ie), "naverage")
	x1 = IE_X1(ie) - (nx - 1) / 2 + 0.5
	x2 = IE_X1(ie) + nx / 2 + 0.5
	xavg = (x1 + x2) / 2.
	y1 = INDEFI
	y2 = INDEFI
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}
	nx = x2 - x1 + 1
	ny = y2 - y1 + 1
	npts = nx * ny

	call smark (sp)
	call salloc (title, IE_SZTITLE, TY_CHAR)
	call salloc (xp, ny, TY_REAL)

	do i = 1, ny
	    call ie_mwctran (ie, xavg,  real(i), junk, Memr[xp+i-1])

	if (nx > 1) {
	    ptr = data
	    call salloc (yp, ny, TY_REAL)
	    do i = 1, ny {
		Memr[yp+i-1] = asumr (Memr[ptr], nx)
	        ptr = ptr + nx
	    }
	    call adivkr (Memr[yp], real (nx), Memr[yp], ny)
	} else
	    yp = data

	call sprintf (Memc[title], IE_SZTITLE, "%s: Columns %d - %d\n%s")
	    call pargstr (IE_IMNAME(ie))
	    call pargi (x1)
	    call pargi (x2)
	    call pargstr (IM_TITLE(im))

	call ie_graph (gp, mode, IE_PP(ie), Memc[title], Memr[xp],
	    Memr[yp], ny, IE_YLABEL(ie), IE_YFORMAT(ie))

	call sfree (sp)
end
