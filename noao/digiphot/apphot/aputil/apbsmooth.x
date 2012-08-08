# AP_BSMOOTH - Box car smooth a histogram 1, 2 or 3 times.

procedure ap_bsmooth (hgm, shgm, nbins, nker, iter)

real	hgm[ARB]		# the original histogram
real	shgm[ARB]		# the smoothed histogram
int	nbins			# length of the histogram
int	nker			# half width of box kernel
int	iter			# number of iterations

pointer	sp, work1, work2

begin
	# Smooth the histogram
	switch (iter) {
	case 1:
	    call ap_sboxr (hgm, shgm, nbins, nker)
	case 2:
	    call smark (sp)
	    call salloc (work1, nbins, TY_REAL)
	    call ap_sboxr (hgm, Memr[work1], nbins, nker)
	    call ap_sboxr (Memr[work1], shgm, nbins, nker)
	    call sfree (sp)
	default:
	    call smark (sp)
	    call salloc (work1, nbins, TY_REAL)
	    call salloc (work2, nbins, TY_REAL)
	    call ap_sboxr (hgm, Memr[work1], nbins, nker)
	    call ap_sboxr (Memr[work1], Memr[work2], nbins, nker)
	    call ap_sboxr (Memr[work2], shgm, nbins, nker)
	    call sfree (sp)
	}
end
