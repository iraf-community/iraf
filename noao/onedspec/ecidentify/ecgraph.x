include	<gset.h>
include	<pkg/gtools.h>
include	"ecidentify.h"

# EC_GRAPH -- Graph image vector in which features are to be ecentified.

procedure ec_graph (ec, gtype)

pointer	ec		# ID pointer
int	gtype		# Graph type

begin
	switch (gtype) {
	case 1:
	    if (IS_INDEFI (EC_AP(ec)))
		call ec_graph3(ec)
	    else
	        call ec_graph1 (ec)
	case 2:
	    call ec_graph2 (ec)
	default:
	    call ec_graph1 (ec)
	}
end


procedure ec_graph1 (ec)

pointer	ec				# ID pointer

int	i
real	xmin, xmax, ymin, ymax, dy
pointer	sp, str, x, y

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, EC_NPTS(ec), TY_REAL)
	y = EC_IMLINE(ec)

	call sprintf (Memc[str], SZ_LINE,
	    "Aperture %d, Image line %d, Order %d")
	    call pargi (EC_AP(ec))
	    call pargi (EC_LINE(ec))
	    call pargi (EC_ORDER(ec))
	call gt_sets (EC_GT(ec), GTPARAMS, Memc[str])
	call achtdr (FITDATA(ec,1), Memr[x], EC_NPTS(ec))

	call gclear (EC_GP(ec))
	xmin = min (Memr[x], Memr[x+EC_NPTS(ec)-1])
	xmax = max (Memr[x], Memr[x+EC_NPTS(ec)-1])
	call alimr (Memr[y], EC_NPTS(ec), ymin, ymax)
	dy = ymax - ymin
	call gswind (EC_GP(ec), xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	call gt_swind (EC_GP(ec), EC_GT(ec))
	call gt_labax (EC_GP(ec), EC_GT(ec))
	call gt_plot (EC_GP(ec), EC_GT(ec), Memr[x], Memr[y], EC_NPTS(ec))

	do i = 1, EC_NFEATURES(ec)
	    if (APN(ec,i) == EC_AP(ec))
	        call ec_mark (ec, i)

	call sfree (sp)
end


# EC_GRAPH2 -- Make review graph for current feature.

procedure ec_graph2 (ec)

pointer	ec				# ID pointer

int	i, j, k
real	xmin, xmax, ymin, ymax, dy
pointer	sp, str, x, y

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, EC_NPTS(ec), TY_REAL)
	y = EC_IMLINE(ec)

	call sprintf (Memc[str], SZ_LINE, "Order %d")
	    call pargi (EC_AP(ec))
	call gt_sets (EC_GT(ec), GTPARAMS, Memc[str])
	call achtdr (FITDATA(ec,1), Memr[x], EC_NPTS(ec))

	xmin = real (FIT(ec,EC_CURRENT(ec))) - EC_ZWIDTH(ec) / 2.
	xmax = real (FIT(ec,EC_CURRENT(ec))) + EC_ZWIDTH(ec) / 2.

	i = 0
	do k = 1, EC_NPTS(ec) {
	    if ((Memr[x+k-1] < xmin) || (Memr[x+k-1] > xmax))
		next
	    if (i == 0)
		i = k
	    j = k
	}
	k = j - i + 1

	call alimr (Memr[y+i-1], k, ymin, ymax)
	dy = ymax - ymin

	call gclear (EC_GP(ec))
	call gswind (EC_GP(ec), xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	call gt_labax (EC_GP(ec), EC_GT(ec))
	call gt_plot (EC_GP(ec), EC_GT(ec), Memr[x], Memr[y], EC_NPTS(ec))

	do i = 1, EC_NFEATURES(ec)
	    if (APN(ec,i) == EC_AP(ec))
	        call ec_mark (ec, i)

	call sfree (sp)
end


procedure ec_graph3 (ec)

pointer	ec				# ID pointer

int	i, npts
real	xmin, xmax, ymin, ymax, dy
pointer	sp, str, x, y

begin
	npts = EC_NPTS(ec) * EC_NLINES(ec)

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (x, npts, TY_REAL)
	y = EC_IMLINE(ec)

	call sprintf (Memc[str], SZ_LINE, "All orders")
	call gt_sets (EC_GT(ec), GTPARAMS, Memc[str])
	call achtdr (Memd[EC_FITDATA(ec)], Memr[x], npts)

	call gclear (EC_GP(ec))
	xmin = min (Memr[x], Memr[x+npts-1])
	xmax = max (Memr[x], Memr[x+npts-1])
	call alimr (Memr[y], npts, ymin, ymax)
	dy = ymax - ymin
	call gswind (EC_GP(ec), xmin, xmax, ymin - .2 * dy, ymax + .2 * dy)
	call gt_swind (EC_GP(ec), EC_GT(ec))
	call gt_labax (EC_GP(ec), EC_GT(ec))
	do i = 1, EC_NLINES(ec) {
	    call gt_plot (EC_GP(ec), EC_GT(ec), Memr[x], Memr[y], EC_NPTS(ec))
	    x = x + EC_NPTS(ec)
	    y = y + EC_NPTS(ec)
	}

	do i = 1, EC_NFEATURES(ec)
	    call ec_mark (ec, i)

	call sfree (sp)
end
