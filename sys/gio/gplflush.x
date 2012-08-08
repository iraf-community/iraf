# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gki.h>
include	<gio.h>

# GPL_FLUSH -- Flush the buffered "polyline", i.e., array of transformed and
# clipped points.  For a polyline or fill area polygon there must be at least
# two points (4 cells) or it will be discarded.  A single point polymarker is
# permitted.

procedure gpl_flush()

int	fd
pointer	ap
include	"gpl.com"

begin
	if (op > 2 && gp_out != NULL) {
	    fd = GP_FD(gp_out)

	    switch (pl_type) {
	    case POLYMARKER:
		ap = GP_PMAP(gp_out)
		if (PM_STATE(ap) != FIXED) {
		    call gki_pmset (fd, ap)
		    PM_STATE(ap) = FIXED
		}
		call gki_polymarker (fd, pl, op / 2)

	    case FILLAREA:
		ap = GP_FAAP(gp_out)
		if (FA_STATE(ap) != FIXED) {
		    call gki_faset (fd, ap)
		    FA_STATE(ap) = FIXED
		}
		if (op > 4)
		    call gki_fillarea (fd, pl, op / 2)

	    default:					# (case POLYLINE)
		ap = GP_PLAP(gp_out)
		if (PL_STATE(ap) != FIXED) {
		    call gki_plset (fd, ap)
		    PL_STATE(ap) = FIXED
		}
		if (op > 4)
		    call gki_polyline (fd, pl, op / 2)
	    }

	    op = 1
	}
end
