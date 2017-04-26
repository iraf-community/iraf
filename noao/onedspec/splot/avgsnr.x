# AVGSNR -- Compute average value and signal-to-noise in region

procedure avgsnr (sh, wx1, wy1, y, n, fd1, fd2)

pointer	sh
real	wx1, wy1
real	y[n]
int	n
int	fd1, fd2

char	command[SZ_FNAME]
real	wx2, wy2
real	avg, snr, rms
int	i, i1, i2, nsum
int	wc, key

int	clgcur()

begin
	# Get second position
	call printf ("m again:")
	call flush (STDOUT)
	i = clgcur ("cursor", wx2, wy2, wc, key, command, SZ_FNAME)

	# Fix pixel indices
	call fixx (sh, wx1, wx2, wy1, wy2, i1, i2)
	if (i1 == i2) {
	    call printf ("Cannot determine SNR - move cursor")
	    return
	}

	# Compute avg, rms, snr
	nsum = i2 - i1 + 1
	avg = 0.
	rms = 0.
	snr = 0.

	if (nsum > 0) {
	    do i = i1, i2
		avg = avg + y[i]
	    avg = avg / nsum
	}

	if (nsum > 1) {
	    call alimr (y[i1], nsum, wy1, wy2)
	    wy1 = wy2 - wy1
	    if (wy1 > 0.) {
	        do i = i1, i2
		    rms = rms + ((y[i] - avg) / wy1) ** 2
	        rms = wy1 * sqrt (rms / (nsum-1))
	        snr = avg / rms
	    }
	}

	# Print out
	call printf ("avg: %10.4g  rms: %10.4g   snr: %8.2f\n")
	    call pargr (avg)
	    call pargr (rms)
	    call pargr (snr)
	if (fd1 != NULL) {
	    call fprintf (fd1, "avg: %10.4g  rms: %10.4g   snr: %8.2f\n")
		call pargr (avg)
		call pargr (rms)
		call pargr (snr)
	}
	if (fd2 != NULL) {
	    call fprintf (fd2, "avg: %10.4g  rms: %10.4g   snr: %8.2f\n")
		call pargr (avg)
		call pargr (rms)
		call pargr (snr)
	}
end
