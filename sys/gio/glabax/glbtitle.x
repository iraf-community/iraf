# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<gset.h>
include	<gio.h>
include	"glabax.h"

# GLB_PLOT_TITLE -- Draw plot title block.  The block may contain several lines.
# Lines are plotted with center, left, or right justification, immediately
# above the top viewport boundary (not immediately above the drawn axis,
# which need not be at the viewport boundary).

procedure glb_plot_title (gp, title, ntitlelines)

pointer	gp			# graphics descriptor
char	title[ARB]		# title block
int	ntitlelines		# number of lines in title block

int	lineno, ip, wcs
real	char_height, x, y, dy
pointer sp, op, lbuf, format, w
real	ggetr()

begin
	if (title[1] == EOS || ntitlelines < 1)
	    return

	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)
	call salloc (format, SZ_FORMAT, TY_CHAR)

	char_height = ggetr (gp, "ch")
	if (char_height < EPSILON)
	    char_height = DEF_CHARHEIGHT * GP_TITLESIZE(gp)

	wcs = GP_WCS(gp)
	w = GP_WCSPTR (gp, wcs)
	y = min (1.0 - char_height,
	    WCS_SY2(w) + (ntitlelines - 1 + 0.5) * char_height)

	call sprintf (Memc[format], SZ_FORMAT, "hj=%c,vj=b")
	    switch (GP_TITLEJUST(gp)) {
	    case GT_LEFT:
		call pargi ('l')
		x = WCS_SX1(w)
	    case GT_RIGHT:
		call pargi ('r')
		x = WCS_SX2(w)
	    default:
		call pargi ('c')
		x = (WCS_SX1(w) + WCS_SX2(w)) / 2.0
	    }

	call gsetr (gp, G_TXSIZE, GP_TITLESIZE(gp))
	call gseti (gp, G_WCS, 0)
	lineno = 1
	op = lbuf

	for (ip=1;  title[ip] != EOS;  ip=ip+1)
	    if (title[ip] == '\n' || (title[ip+1] == EOS && op > lbuf)) {
		if (title[ip] != '\n') {
		    Memc[op] = title[ip]
		    op = op + 1
		}
		Memc[op] = EOS
		dy = (lineno - 1) * char_height
		call gtext (gp, x, y - dy, Memc[lbuf], Memc[format])
		lineno = lineno + 1
		op = lbuf
	    } else {
		Memc[op] = title[ip]
		op = op + 1
	    }
	
	call sfree (sp)
end
