# AVGSNR -- Compute average value and signal-to-noise is region

procedure avgsnr (gfd, wx, wy, x1, x2, dx, pix)

int	gfd
real	wx, wy, x1, x2, dx
real	pix[ARB]

char	command[SZ_FNAME]
real	wc
real	snx1, snx2, sny1, sny2
real	avg, snr, rms
int	i, i1, i2, nsum
int	key, junk

int	clgcur()

begin
	snx1 = wx
	sny1 = wy

	#call gmark (gfd, snx1, sny1, GM_BOX, 1., 1.)
	#call gflush (gfd)

	call printf ("m again:")
	call flush (STDOUT)

	# Get second position
	junk = clgcur ("cursor", snx2, sny2, wc, key, command, SZ_FNAME)

	#call gmark (gfd, snx2, sny2, GM_BOX, 1., 1.)
	#call gflush (gfd)

	# Reverse cursor positions if necessary
	call fixx (snx1, snx2, sny1, sny2, x1, x2)

	if (snx1 == snx2) {
	    call printf ("Cannot determine SNR - move cursor")
	    return
	}

	# Get pixel indices
	call pixind (x1, x2, dx, snx1, i1)
	call pixind (x1, x2, dx, snx2, i2)

	# Compute avg, rms, snr
	nsum = i2 - i1 + 1
	avg = 0.
	rms = 0.
	snr = 0.

	if (nsum > 0) {
	    do i = i1, i2
		avg = avg + pix[i]
	    avg = avg / nsum
	}

	if (nsum > 1) {
	    call alimr (pix[i1], nsum, sny1, sny2)
	    sny1 = sny2 - sny1
	    if (sny1 > 0.) {
	        do i = i1, i2
		    rms = rms + ((pix[i] - avg) / sny1) ** 2
	        rms = sny1 * sqrt (rms / (nsum-1))
	        snr = avg / rms
	    }
	}

	# Print out
	call printf ("avg: %10.4g  rms: %10.4g   snr: %8.2f")
	    call pargr (avg)
	    call pargr (rms)
	    call pargr (snr)
end
