include	<gset.h>
include	<pkg/gtools.h>
include	<smw.h>
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

int	i, n
real	xmin, xmax, ymin, ymax, dy, xminz, xmaxz, id_zshiftr()
pointer	gp, sh, sp, x, y, str

begin
	gp = ID_GP(id)
	sh = ID_SH(id)

	call smark (sp)
	call salloc (x, SN(sh), TY_REAL)
	y = SY(sh)
	n = SN(sh)

	call achtdr (FITDATA(id,1), Memr[x], n)

	call gclear (gp)
	xmin = min (Memr[x], Memr[x+n-1])
	xmax = max (Memr[x], Memr[x+n-1])
	call alimr (Memr[y], n, ymin, ymax)
	dy = ymax - ymin
	call gswind (gp, xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	call gt_swind (gp, ID_GT(id))

	if (ID_TASK(id) == RVIDLINES && ID_REDSHIFT(id) != 0D0) {
	    call salloc (str, SZ_LINE, TY_CHAR)
	    if (ID_ZHELIO(id) == 0D0)
		call sprintf (Memc[str], SZ_LINE,
		    "Vobs = %.5g, Zobs = %.5g\n\n\n")
	    else
		call sprintf (Memc[str], SZ_LINE,
		    "Vhelio = %.5g, Zhelio = %.5g\n\n\n")
		call pargd ((ID_REDSHIFT(id)+ID_ZHELIO(id)) * VLIGHT)
		call pargd (ID_REDSHIFT(id)+ID_ZHELIO(id))
	    call gt_sets (ID_GT(id), GTSUBTITLE, Memc[str])
	    call gseti (gp, G_XDRAWAXES, 1)
	    call gt_labax (gp, ID_GT(id))
	    call gt_sets (ID_GT(id), GTSUBTITLE, "")

	    call ggwind (gp, xmin, xmax, ymin, ymax)
	    xminz = id_zshiftr (id, xmin, 0)
	    xmaxz = id_zshiftr (id, xmax, 0)
	    call gswind (gp, xminz, xmaxz, ymin, ymax)
	    call gseti (gp, G_XDRAWAXES, 2)
	    call gseti (gp, G_YDRAWAXES, 0)
	    call glabax (gp, "", "", "")

	    call gswind (gp, xmin, xmax, ymin, ymax)
	    call gctran (gp, xmin, ymin, xmax, ymax, 1, 0)
	    call gctran (gp, xmax, ymax, xmin, ymin, 0, 1)
	} else
	    call gt_labax (gp, ID_GT(id))

	call gt_plot (gp, ID_GT(id), Memr[x], Memr[y], n)

	do i = 1, ID_NFEATURES(id)
	    call id_mark (id, i)

	call sfree (sp)
end


# ID_GRAPH2 -- Make review graph for current feature.

procedure id_graph2 (id)

pointer	id				# ID pointer

int	i, j, k, n
real	xmin, xmax, ymin, ymax, dy, xminz, xmaxz, id_zshiftr()
pointer	gp, sh, sp, x, y, str

begin
	gp = ID_GP(id)
	sh = ID_SH(id)

	call smark (sp)
	call salloc (x, SN(sh), TY_REAL)
	y = SY(sh)
	n = SN(sh)

	call achtdr (FITDATA(id,1), Memr[x], n)

	xmin = real (FIT(id,ID_CURRENT(id))) - ID_ZWIDTH(id) / 2.
	xmax = real (FIT(id,ID_CURRENT(id))) + ID_ZWIDTH(id) / 2.

	i = 0
	do k = 1, n {
	    if ((Memr[x+k-1] < xmin) || (Memr[x+k-1] > xmax))
		next
	    if (i == 0)
		i = k
	    j = k
	}
	k = j - i + 1

	call alimr (Memr[y+i-1], k, ymin, ymax)
	dy = ymax - ymin

	call gclear (gp)
	call gswind (gp, xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
#	if (ID_GT(id) != NULL) {
#	    call gseti (gp, G_XTRAN, GT_XTRAN(ID_GT(id)))
#	    call gseti (gp, G_YTRAN, GT_YTRAN(ID_GT(id)))
#	}
	if (ID_TASK(id) == RVIDLINES && ID_REDSHIFT(id) != 0D0) {
	    call salloc (str, SZ_LINE, TY_CHAR)
	    if (ID_ZHELIO(id) == 0D0)
		call sprintf (Memc[str], SZ_LINE,
		    "Vobs = %.5g, Zobs = %.5g\n\n\n")
	    else
		call sprintf (Memc[str], SZ_LINE,
		    "Vhelio = %.5g, Zhelio = %.5g\n\n\n")
		call pargd ((ID_REDSHIFT(id)+ID_ZHELIO(id)) * VLIGHT)
		call pargd (ID_REDSHIFT(id)+ID_ZHELIO(id))
	    call gt_sets (ID_GT(id), GTSUBTITLE, Memc[str])
	    call gseti (gp, G_XDRAWAXES, 1)
	    call gt_labax (gp, ID_GT(id))
	    call gt_sets (ID_GT(id), GTSUBTITLE, "")

	    call ggwind (gp, xmin, xmax, ymin, ymax)
	    xminz = id_zshiftr (id, xmin, 0)
	    xmaxz = id_zshiftr (id, xmax, 0)
	    call gswind (gp, xminz, xmaxz, ymin, ymax)
	    call gseti (gp, G_XDRAWAXES, 2)
	    call gseti (gp, G_YDRAWAXES, 0)
	    call glabax (gp, "", "", "")

	    call gswind (gp, xmin, xmax, ymin, ymax)
	    call gctran (gp, xmin, ymin, xmax, ymax, 1, 0)
	    call gctran (gp, xmax, ymax, xmin, ymin, 0, 1)
	} else
	    call gt_labax (gp, ID_GT(id))

	call gt_plot (gp, ID_GT(id), Memr[x], Memr[y], n)

	do i = 1, ID_NFEATURES(id)
	    call id_mark (id, i)

	call sfree (sp)
end
