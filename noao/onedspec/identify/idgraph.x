include	<gset.h>
include	<pkg/gtools.h>
include	"identify.h"

# ID_GRAPH -- Graph image vector in which features are to be identified.

procedure id_graph (id, gtype)

pointer	id		# ID pointer
int	gtype		# Graph type

begin
	switch (gtype) {
	case 1:
	    call id_graph1 (id)
	case 2:
	    call id_graph2 (id)
	default:
	    call id_graph1 (id)
	}
end


procedure id_graph1 (id)

pointer	id				# ID pointer

int	i
real	xmin, xmax, ymin, ymax, dy
pointer	sp, x, y

begin
	call smark (sp)
	call salloc (x, ID_NPTS(id), TY_REAL)
	call salloc (y, ID_NPTS(id), TY_REAL)
	call achtdr (FITDATA(id,1), Memr[x], ID_NPTS(id))
	call achtdr (IMDATA(id,1), Memr[y], ID_NPTS(id))

	call gclear (ID_GP(id))
	xmin = min (Memr[x], Memr[x+ID_NPTS(id)-1])
	xmax = max (Memr[x], Memr[x+ID_NPTS(id)-1])
	call alimr (Memr[y], ID_NPTS(id), ymin, ymax)
	dy = ymax - ymin
	call gswind (ID_GP(id), xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	call gt_swind (ID_GP(id), ID_GT(id))
	call gt_labax (ID_GP(id), ID_GT(id))
	call gt_plot (ID_GP(id), ID_GT(id), Memr[x], Memr[y], ID_NPTS(id))

	do i = 1, ID_NFEATURES(id)
	    call id_mark (id, i)

	call sfree (sp)
end


# ID_GRAPH2 -- Make review graph for current feature.

procedure id_graph2 (id)

pointer	id				# ID pointer

int	i, j, k
real	xmin, xmax, ymin, ymax, dy
pointer	sp, x, y

begin
	call smark (sp)
	call salloc (x, ID_NPTS(id), TY_REAL)
	call salloc (y, ID_NPTS(id), TY_REAL)
	call achtdr (FITDATA(id,1), Memr[x], ID_NPTS(id))
	call achtdr (IMDATA(id,1), Memr[y], ID_NPTS(id))

	xmin = real (FIT(id,ID_CURRENT(id))) - ID_ZWIDTH(id) / 2.
	xmax = real (FIT(id,ID_CURRENT(id))) + ID_ZWIDTH(id) / 2.

	i = 0
	do k = 1, ID_NPTS(id) {
	    if ((Memr[x+k-1] < xmin) || (Memr[x+k-1] > xmax))
		next
	    if (i == 0)
		i = k
	    j = k
	}
	k = j - i + 1

	call alimr (Memr[y+i-1], k, ymin, ymax)
	dy = ymax - ymin

	call gclear (ID_GP(id))
	call gswind (ID_GP(id), xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	if (ID_GT(id) != NULL) {
	    call gseti (ID_GP(id), G_XTRAN, GT_XTRAN(ID_GT(id)))
	    call gseti (ID_GP(id), G_YTRAN, GT_YTRAN(ID_GT(id)))
	}
	call gt_labax (ID_GP(id), ID_GT(id))
	call gt_plot (ID_GP(id), ID_GT(id), Memr[x], Memr[y], ID_NPTS(id))

	do i = 1, ID_NFEATURES(id)
	    call id_mark (id, i)

	call sfree (sp)
end
