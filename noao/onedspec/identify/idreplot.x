include	<pkg/gtools.h>
include	"identify.h"

# ID_REPLOT -- Plot residual graph of reidentfy lines.

procedure id_replot (id, fd)

pointer	id				# ID pointer
int	fd				# Plot file descriptor

int	i, j
pointer	sp, str, x, y
pointer	gp, gt, gopen(), gt_init()

begin
	# Check if there is anything to plot.
	if (fd == NULL || ID_NFEATURES(id) == 0)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, ID_NFEATURES(id), TY_REAL)
	call salloc (y, ID_NFEATURES(id), TY_REAL)

	# Set plot points.
	j = 0
	do i = 1, ID_NFEATURES(id) {
	    if (IS_INDEFD (USER(id,i)))
		break

	    Memr[x+j] = USER(id,i)
	    Memr[y+j] = FIT(id,i) - USER(id,i)
	    j = j + 1
	}

	if (j == 0) {
	    call sfree (sp)
	    return
	}

	# Open plot stream and set graph parameters.
	gp = gopen ("stdvdm", NEW_FILE, fd)
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "mark")
	call sprintf (Memc[str], SZ_LINE, "Reidentify: %s")
	    call pargstr (Memc[ID_IMAGE(id)])
	call gt_sets (gt, GTTITLE, Memc[str])
	call gt_sets (gt, GTXLABEL, "user coordinates")
	call gt_sets (gt, GTYLABEL, "residuals (fit - user)")

	# Make the plot.
	call gclear (gp)
	call gascale (gp, Memr[x], j, 1)
	call gascale (gp, Memr[y], j, 2)
	call gt_swind (gp, gt)
	call gt_labax (gp, gt)
	call gt_plot (gp, gt, Memr[x], Memr[y], j)

	call gt_free (gt)
	call gclose (gp)
	call sfree (sp)
end
