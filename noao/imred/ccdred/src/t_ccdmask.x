include	<imhdr.h>


define	MAXBUF		500000	# Maximum pixel buffer

define	PLSIG		30.9	# Low percentile
define	PHSIG		69.1	# High percentile


# T_CCDMASK -- Create a bad pixel mask from CCD images.
# Deviant pixels relative to a local median and sigma are detected and
# written to a pixel mask file.  There is a special algorithm for detecting
# long column oriented features typical of CCD defects.  This task
# is intended for use on flat fields or, even better, the ratio of
# two flat fields at different exposure levels.

procedure t_ccdmask ()

pointer	image			# Input image
pointer	mask			# Output mask
int	ncmed, nlmed		# Median box size
int	ncsig, nlsig		# Sigma box size
real	lsig, hsig		# Threshold sigmas
int	ngood			# Minmum good pixel sequence
short	linterp			# Mask value for line interpolation 
short	cinterp			# Mask value for column interpolation 
short	eqinterp		# Mask value for equal interpolation

int	i, j, c1, c2, c3, c4, nc, nl, ncstep, nc1
pointer	sp, in, out, inbuf, outbuf
real	clgetr()
int	clgeti(), nowhite(), strmatch()
pointer	immap(), imgs2r(), imps2s(), imgl2s(), impl2s()
errchk	immap, imgs2r, imps2r, imgl2s, impl2s, cm_mask

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (mask, SZ_FNAME, TY_CHAR)

	# Get parameters.
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("mask", Memc[mask], SZ_FNAME)
	ncmed = clgeti ("ncmed")
	nlmed = clgeti ("nlmed")
	ncsig = clgeti ("ncsig")
	nlsig = clgeti ("nlsig")
	lsig = clgetr ("lsigma")
	hsig = clgetr ("hsigma")
	ngood = clgeti ("ngood")
	linterp = clgeti ("linterp")
	cinterp = clgeti ("cinterp")
	eqinterp = clgeti ("eqinterp")

	# Force a pixel list format.
	i = nowhite (Memc[mask], Memc[mask], SZ_FNAME)
	if (strmatch (Memc[mask], ".pl$") == 0)
	    call strcat (".pl", Memc[mask], SZ_FNAME)

	# Map the input and output images.
	in = immap (Memc[image], READ_ONLY, 0)
	out = immap (Memc[mask], NEW_COPY, in)

	# Go through the input in large blocks of columns.  If the
	# block is smaller than the whole image overlap the blocks
	# so the median only has boundaries at the ends of the image.
	# Set the mask values based on the distances to the nearest
	# good pixels.

	nc = IM_LEN(in,1)
	nl = IM_LEN(in,2)
	ncstep = max (1, MAXBUF / nl - ncmed)

	outbuf = NULL
	do i = 1, nc, ncstep {
	    c1 = i
	    c2 = min (nc, i + ncstep - 1)
	    c3 = max (1, c1 - ncmed / 2)
	    c4 = min (nc, c2 + ncmed / 2)
	    nc1 = c4 - c3 + 1
	    inbuf = imgs2r (in, c3, c4, 1, nl)
	    if (outbuf == NULL)
		call malloc (outbuf, nc1*nl, TY_SHORT)
	    else
		call realloc (outbuf, nc1*nl, TY_SHORT)
	    call aclrs (Memc[outbuf], nc1*nl)
	    call cm_mask (Memr[inbuf], Mems[outbuf], nc1, nl, c1-c3+1,
		c2-c3+1, ncmed, nlmed, ncsig, nlsig, lsig, hsig, ngood)
	    call cm_interp (Mems[outbuf], nc1, nl, c1-c3+1, c2-c3+1, nc,
		linterp, cinterp, eqinterp)
	    do j = 1, nl
		call amovs (Mems[outbuf+(j-1)*nc1+c1-c3],
		    Mems[imps2s(out,c1,c2,j,j)], c2-c1+1)
	}
	call mfree (outbuf, TY_SHORT)

	call imunmap (out)
	call imunmap (in)

	# If the image was searched in blocks we need another pass to find
	# the lengths of bad pixel regions along lines since they may
	# span the block edges.  Previously the mask values were set
	# to the column lengths so in this pass we can just look at
	# whole lines sequentially.

	if (nc1 != nc) {
	    out = immap (Memc[mask], READ_WRITE, 0)
	    do i = 1, nl {
		inbuf = imgl2s (out, i)
		outbuf = impl2s (out, i)
		call cm_interp1 (Mems[inbuf], Mems[outbuf], nc, nl,
		    linterp, cinterp, eqinterp)
	    }
	    call imunmap (out)
	}

	call sfree (sp)
end


# CM_MASK -- Compute the mask image.
# A local background is computed using moving box medians to avoid
# contaminating bad pixels.  The local sigma is computed in blocks (it is not
# a moving box for efficiency) by using a percentile point of the sorted
# pixel values to estimate the width of the distribution uncontaminated by
# bad pixels).  Once the background and sigma are known deviant pixels are
# found by using sigma threshold factors.  Sums of pixels along columns are
# checked at various scales from single pixels to whole columns with the
# sigma level set appropriately.  The provides sensitivity to weaker column
# features such as CCD traps.

procedure cm_mask (data, bp, nc, nl, nc1, nc2, ncmed, nlmed, ncsig, nlsig,
	lsig, hsig, ngood)

real	data[nc,nl]		#I Pixel array
short	bp[nc,nl]		#U Bad pixel array (0=good, 1=bad)
int	nc, nl			#I Number of columns and lines
int	nc1, nc2		#I Columns to compute
int	ncmed, nlmed		#I Median box size
int	ncsig, nlsig		#I Sigma box size
real	lsig, hsig		#I Threshold sigmas
int	ngood			#I Minimum good pixel sequence

int	i, j, k, l, m, nsum, plsig, phsig, jsig
real	back, sigma, sum1, sum2, low, high, amedr()
pointer	sp, bkg, sig, work, bp1, ptr

begin
	call smark (sp)
	call salloc (bkg, nl, TY_REAL)
	call salloc (sig, nl/nlsig, TY_REAL)
	call salloc (work, max (ncsig*nlsig, ncmed*nlmed), TY_REAL)
	call salloc (bp1, nl, TY_SHORT)

	bkg = bkg - 1
	sig = sig - 1

	i = nlsig * ncsig
	plsig = nint (PLSIG*i/100.-1)
	phsig = nint (PHSIG*i/100.-1)

	do i = nc1, nc2 {

	    # Compute median background.  This is a moving median.
	    l = min (nc, i+ncmed/2)
	    l = max (1, l-ncmed+1)
	    do j = 1, nl {
		k = min (nl, j+nlmed/2)
		k = max (1, k-nlmed+1)
		ptr = work
		do m = k, k+nlmed-1 {
		    call amovr (data[l,m], Memr[ptr], ncmed)
		    ptr = ptr + ncmed
		}
		back = amedr (Memr[work], ncmed * nlmed)
		Memr[bkg+j] = back
	    }

	    # Compute sigmas from percentiles.  This is done in blocks.
	    if (mod (i-nc1, ncsig) == 0 && i<nc-ncsig+1) {
		do j = 1, nl-nlsig+1, nlsig {
		    ptr = work
		    do k = j, j+nlsig-1 {
			call amovr (data[i,k], Memr[ptr], ncsig)
			ptr = ptr + ncsig
		    }
		    call asrtr (Memr[work], Memr[work], ncsig*nlsig)
		    sigma = Memr[work+phsig] - Memr[work+plsig]
		    jsig = (j+nlsig-1) / nlsig
		    Memr[sig+jsig] = sigma**2
		}
	    }

	    # Single pixel iterative rejection.
	    k = 0
	    do j = 1, nl {
		if (bp[i,j] == 1)
		    k = k + 1
		else {
		    jsig = min ((j+nlsig-1)/nlsig, nl/nlsig)
		    back = Memr[bkg+j]
		    sigma = sqrt (Memr[sig+jsig])
		    low = back - lsig * sigma
		    high = back + hsig * sigma
		    if (data[i,j] < low || data[i,j] > high) {
			bp[i,j] = 1
			k = k + 1
		    }
		}
	    }
	    if (k == nl)
		next

	    # Reject over column sums at various scales.
	    # Ignore previously rejected pixels.

	    l = 2
	    while (l <= nl) {
		do j = 1, nl
		    Mems[bp1+j-1] = bp[i,j]
		sum1 = 0
		sum2 = 0
		nsum = 0
		k = 1
		do j = k, l-1 {
		    if (bp[i,j] == 1)
			next
		    jsig = min ((j+nlsig-1)/nlsig, nl/nlsig)
		    sum1 = sum1 + data[i,j] - Memr[bkg+j]
		    sum2 = sum2 + Memr[sig+jsig]
		    nsum = nsum + 1
		}
		do j = l, nl {
		    if (bp[i,j] == 0) {
			jsig = min ((j+nlsig-1)/nlsig, nl/nlsig)
			sum1 = sum1 + data[i,j] - Memr[bkg+j]
			sum2 = sum2 + Memr[sig+jsig]
			nsum = nsum + 1
		    }
		    if (nsum > 0) {
			sigma = sqrt (sum2)
			low = -lsig * sigma
			high = hsig * sigma
			if (sum1 < low || sum1 > high)
			    do m = k, j
				bp[i,m] = 1
		    }
		    if (Mems[bp1+k-1] == 0) {
			jsig = min ((k+nlsig-1)/nlsig, nl/nlsig)
			sum1 = sum1 - data[i,k] + Memr[bkg+k]
			sum2 = sum2 - Memr[sig+jsig]
			nsum = nsum - 1
		    }
		    k = k + 1
		}

		if (l == nl)
		    break
		else if (l < 10)
		    l = l + 1
		else
		    l = min (l * 2, nl)
	    }

	    # Coalesce small good regions along columns.
	    if (ngood > 1) {
		for (k=1; k<=nl && bp[i,k]!=0; k=k+1)
		    ;
		while (k < nl) {
		    for (l=k+1; l<=nl && bp[i,l]==0; l=l+1)
			;
		    if (l-k < ngood)
			do j = k, l-1
			    bp[i,j] = 1
		    for (k=l+1; k<=nl && bp[i,k]!=0; k=k+1)
			;
		}
	    }
	}

	call sfree (sp)
end


# CM_INTERP -- Compute the lengths of bad regions along columns and lines.
# If only part of the image is buffered set the pixel mask values
# to the column lengths so a later pass can compare these values against
# the full line lengths.  If the whole image is buffered then both
# the column and line lengths can be determined and the the mask values
# set based on these lengths.

procedure cm_interp (bp, nc, nl, nc1, nc2, ncimage, linterp, cinterp, eqinterp)

short	bp[nc,nl]		#U Bad pixel array
int	nc, nl			#I Number of columns and lines
int	nc1, nc2		#I Columns to compute
int	ncimage			#I Number of columns in image
short	linterp			#I Mask value for line interpolation 
short	cinterp			#I Mask value for column interpolation 
short	eqinterp		#I Mask value for equal interpolation

int	i, j, k, l, m, n

begin
	do i = nc1, nc2 {

	    # Set values to column length.
	    for (k=1; k<=nl && bp[i,k]==0; k=k+1)
		;
	    while (k <= nl) {
		for (l=k+1; l<=nl && bp[i,l]!=0; l=l+1)
		    ;
		m = l - k
		do j = k, l-1
		    bp[i,j] = m
		for (k=l+1; k<=nl && bp[i,k]==0; k=k+1)
		    ;
	    }
	}

	# Set values to minimum axis length for interpolation.
	if (nc == ncimage) {
	    do j = 1, nl {
		for (k=1; k<=nc && bp[k,j]==0; k=k+1)
		    ;
		while (k <= nc) {
		    for (l=k+1; l<=nc && bp[l,j]!=0; l=l+1)
			;
		    m = l - k
		    do i = k, l-1 {
			n = bp[i,j]
			if (n > m || n == nl)
			    bp[i,j] = linterp
			else if (n < m)
			    bp[i,j] = cinterp
			else
			    bp[i,j] = eqinterp
		    }
		    for (k=l+1; k<=nc && bp[k,j]==0; k=k+1)
			;
		}
	    }
	}
end


# CM_INTERP1 -- Set the mask values based on the column and line lengths
# of the bad pixel regions.  If this routine is called the pixel mask
# is open READ/WRITE and the pixel mask values have been previously set
# to the column lengths.  So here we just need to compute the line
# lengths across the entire image and reset the mask values to the
# appropriate interpolation mask code.

procedure cm_interp1 (in, out, nc, nl, linterp, cinterp, eqinterp)

short	in[nc]			#I Bad pixel array with column length codes
short	out[nc]			#O Bad pixel array with interp axis codes
int	nc, nl			#I Image dimensions
short	linterp			#I Mask value for line interpolation 
short	cinterp			#I Mask value for column interpolation 
short	eqinterp		#I Mask value for equal interpolation

int	i, j, l, m, n

begin
	for (j=1; j<=nc && in[j]==0; j=j+1)
	    out[j] = 0
	while (j < nc) {
	    for (l=j+1; l<=nc && in[l]!=0; l=l+1)
		;
	    m = l - j
	    do i = j, l-1 {
		n = in[i]
		if (n > m || n == nl)
		    out[i] = linterp
		else if (n < m)
		    out[i] = cinterp
		else
		    out[i] = eqinterp
	    }
	    for (j=l+1; j<=nc && in[j]==0; j=j+1)
		out[j] = 0
	}
end
