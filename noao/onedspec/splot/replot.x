include	<gset.h>

# REPLOT -- Replot the current array

procedure replot (gfd, gt, x, y, npts, clear)

pointer	gfd
pointer	gt
real	x[ARB]
real	y[ARB]
int	npts
int	clear

int	wc, gstati()

begin
	if (clear == YES) {
	    wc = gstati (gfd, G_WCS)
	    call gclear (gfd)
	    call gseti (gfd, G_WCS, wc)
	    call gascale (gfd, x, npts, 1)
	    call gascale (gfd, y, npts, 2)
	    call gt_swind (gfd, gt)
	    call gt_labax (gfd, gt)
	}

	call gt_plot (gfd, gt, x, y, npts)
end
