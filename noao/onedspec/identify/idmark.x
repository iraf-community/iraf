include	<gset.h>
include	<pkg/center1d.h>
include	"identify.h"

procedure id_mark (id, feature)

pointer	id				# ID pointer
int	feature

int	pix
real	x, y
real	mx, my, x1, x2, y1, y2, tick, gap
pointer	sp, format, label

define	TICK	.03	# Tick size in NDC
define	GAP	.02	# Gap size in NDC

begin
	call ggwind (ID_GP(id), x1, x2, y1, y2)

	x = FIT(id,feature)

	if ((x < min (x1, x2)) || (x > max (x1, x2)))
	    return

	call smark (sp)
	call salloc (format, SZ_LINE, TY_CHAR)
	call salloc (label, SZ_LINE, TY_CHAR)
	switch (ID_FTYPE(id)) {
	case EMISSION:
	    pix = min (int (PIX(id,feature)), ID_NPTS(id) - 1)
	    y = max (IMDATA(id,pix), IMDATA(id,pix+1))
	    tick = TICK
	    gap = GAP
	    call strcpy ("u=180;h=c;v=b;s=0.5", Memc[format], SZ_LINE)
	case ABSORPTION:
	    pix = min (int (PIX(id,feature)), ID_NPTS(id) - 1)
	    y = min (IMDATA(id,pix), IMDATA(id,pix+1))
	    tick = -TICK
	    gap = -GAP
	    call strcpy ("u=0;h=c;v=t;s=0.5", Memc[format], SZ_LINE)
	}

	call gctran (ID_GP(id), x, y, mx, my, 1, 0)
	call gctran (ID_GP(id), mx, my + gap, x1, y1, 0, 1)
	call gctran (ID_GP(id), mx, my + gap + tick, x1, y2, 0, 1)
	call gline (ID_GP(id), x1, y1, x1, y2)

	call gctran (ID_GP(id), mx, my + tick + 2 * gap, x1, y2, 0, 1)
	switch (ID_LABELS(id)) {
	case 2:
	    call sprintf (Memc[label], SZ_LINE, "%d")
		call pargi (feature)
	    call gtext (ID_GP(id), x1, y2, Memc[label], Memc[format])
	case 3:
	    call sprintf (Memc[label], SZ_LINE, "%0.2f")
		call pargd (PIX(id,feature))
	    call gtext (ID_GP(id), x1, y2, Memc[label], Memc[format])
	case 4:
	    if (!IS_INDEFD (USER(id,feature))) {
	        call sprintf (Memc[label], SZ_LINE, "%0.4f")
		    call pargd (USER(id,feature))
		call gtext (ID_GP(id), x1, y2, Memc[label], Memc[format])
	    }
	}

	call sfree (sp)
	call gflush (ID_GP(id))
end
