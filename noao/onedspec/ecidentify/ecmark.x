include	<gset.h>
include	<pkg/center1d.h>
include	"ecidentify.h"

procedure ec_mark (ec, feature)

pointer	ec				# ID pointer
int	feature

int	pix
real	x, y
real	mx, my, x1, x2, y1, y2, tick, gap
pointer	sp, format, label
double	smw_c1trand()

define	TICK	.03	# Tick size in NDC
define	GAP	.02	# Gap size in NDC

begin
	call ggwind (EC_GP(ec), x1, x2, y1, y2)

	x = FIT(ec,feature)

	if ((x < min (x1, x2)) || (x > max (x1, x2)))
	    return

	pix = smw_c1trand (EC_PL(ec), PIX(ec,feature))
	pix = max (1, min (pix, EC_NPTS(ec) - 1))

	call smark (sp)
	call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)
	switch (EC_FTYPE(ec)) {
	case EMISSION:
	    y = max (IMDATA(ec,pix), IMDATA(ec,pix+1))
	    tick = TICK
	    gap = GAP
	    call strcpy ("u=180;h=c;v=b;s=0.5", Memc[format], SZ_LINE)
	case ABSORPTION:
	    y = min (IMDATA(ec,pix), IMDATA(ec,pix+1))
	    tick = -TICK
	    gap = -GAP
	    call strcpy ("u=0;h=c;v=t;s=0.5", Memc[format], SZ_LINE)
	}

	call gctran (EC_GP(ec), x, y, mx, my, 1, 0)
	call gctran (EC_GP(ec), mx, my + gap, x1, y1, 0, 1)
	call gctran (EC_GP(ec), mx, my + gap + tick, x1, y2, 0, 1)
	call gline (EC_GP(ec), x1, y1, x1, y2)

	call gctran (EC_GP(ec), mx, my + tick + 2 * gap, x1, y2, 0, 1)
	switch (EC_LABELS(ec)) {
	case 2:
	    call sprintf (Memc[label], SZ_LINE, "%d")
		call pargi (feature)
	    call gtext (EC_GP(ec), x1, y2, Memc[label], Memc[format])
	case 3:
	    call sprintf (Memc[label], SZ_LINE, "%0.2f")
		call pargd (PIX(ec,feature))
	    call gtext (EC_GP(ec), x1, y2, Memc[label], Memc[format])
	case 4:
	    if (!IS_INDEFD (USER(ec,feature))) {
	        call sprintf (Memc[label], SZ_LINE, "%0.4f")
		    call pargd (USER(ec,feature))
		call gtext (EC_GP(ec), x1, y2, Memc[label], Memc[format])
	    }
	}

	call sfree (sp)
	call gflush (EC_GP(ec))
end
