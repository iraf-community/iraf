# AP_LUCY_SMOOTH - Lucy smooth a histogram with a box shaped function.

procedure ap_lucy_smooth (hgm, shgm, nbins, nker, iter)

real	hgm[ARB]		# the original histogram
real	shgm[ARB]		# smoothed histogram
int	nbins			# length of the histogram
int	nker			# length of box kernel
int	iter			# number of iterations

int	i, j, nsmooth
pointer	sp, work1, work2
real	ap_diverr()

begin
	# Allocate space and clear the work arrays.
	call smark (sp)
	call salloc (work1, nbins, TY_REAL)
	call salloc (work2, nbins, TY_REAL)
	call aclrr (Memr[work1], nbins)
	call aclrr (Memr[work2], nbins)
	call aclrr (shgm, nbins)

	# Compute the smoothing length and initialize output array.
	nsmooth = nbins - nker + 1
	#call ap_aboxr (hgm, shgm[1+nker/2], nsmooth, nker)
	call ap_sboxr (hgm, shgm, nbins, nker)

	# Iterate on the solution.
	do i = 1, iter {
	    #call ap_aboxr (shgm, Memr[work1+nker/2], nsmooth, nker)
	    call ap_sboxr (shgm, Memr[work1], nbins, nker)
	    #do j = 1 + nker / 2, nsmooth + nker / 2 {
	    do j = 1, nbins {
		if (Memr[work1+j-1] == 0.0)
		    Memr[work1+j-1] = ap_diverr ()
		else
		    #Memr[work1+j-1] = hgm[j] / Memr[work1+j-1]
		    Memr[work1+j-1] = shgm[j] / Memr[work1+j-1]
	    }
	    #call ap_aboxr (Memr[work1], Memr[work2+nker/2], nsmooth, nker)
	    call ap_sboxr (Memr[work1], Memr[work2], nbins, nker)
	    call amulr (shgm, Memr[work2], shgm, nbins)
	}

	call sfree (sp)
end
