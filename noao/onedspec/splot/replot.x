# REPLOT -- Replot the current array

procedure replot (gfd, gt, pix, npts, x1, x2, clear)

pointer	gfd
pointer	gt
real	pix[ARB]
real	x1, x2
int	npts
int	clear

begin
	if (clear == YES) {
	    call gclear (gfd)
	    call gswind (gfd, x1, x2, INDEF, INDEF)
	    call gascale (gfd, pix, npts, 2)
	    call gt_swind (gfd, gt)
	    call gt_labax (gfd, gt)
	}

	call gt_vplot (gfd, gt, pix, npts, x1, x2)
end
