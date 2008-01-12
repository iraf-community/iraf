include	<error.h>
include	<ctype.h>
include	<imhdr.h>
include	<imset.h>
include	<mach.h>
include	"skyblock.h"


# SKY_BLOCK - Determine sky and sky sigma in blocks.
#
# This is layered on MAPIO and CONVOLVE.

procedure sky_block (skb, dosky, dosig, in, bpm, expmap, skyname, signame,
	skymap, sigmap, logfd)

pointer	skb			#U Sky block structure
bool	dosky			#I Compute sky
bool	dosig			#I Compute sigma
pointer	in			#I Input image pointer
pointer	bpm			#I Input mask
pointer	expmap			#I Exposure map
char	skyname[ARB]		#I Sky map name (if none then no output)
char	signame[ARB]		#I Sigma map name (if none then no output)
pointer	skymap			#U Sky map
pointer	sigmap			#U Sigma map
int	logfd			#I Verbose?

int	l, blkstep, nc, nl
real	cnvwt
pointer	sp, cnv, cnvdata, bp
pointer	im[2], indata, skydata, sigdata, expdata
errchk	skb_pars, skb_iminit, convolve, skb_accum, skb_update

begin
	if (!(dosky||dosig))
	    return

	call smark (sp)

	# Log operation.
	if (logfd != NULL) {
	    if (dosky && dosig)
		call fprintf (logfd,
		    "    Determine sky and sigma by block statistics:\n")
	    else if (dosky)
		call fprintf (logfd, "    Determine sky by block statistics:\n")
	    else
		call fprintf (logfd,
		    "    Determine sigma by block statistics:\n")
	}

	# Set parameters if not set in a previous call or set externally.
	if (skb == NULL)
	    call skb_pars ("open", "", skb)

	# Set parameters for the image.
	blkstep = SKB_BLKSTEP(skb)
	call skb_iminit (skb, in, expmap, blkstep, logfd)

	# Set maximum number of image columns and lines to use.
	nc = SKB_NCSBLK(skb) * SKB_NCSPIX(skb)
	nl = SKB_NLSBLK(skb) * SKB_NLSPIX(skb)

	# Set up convolution.  Note we can't use convolution with a blkstep.
	cnv = SKB_CNV(skb)
	if (Memc[cnv] != EOS) {
	    if (blkstep > 1) {
		call salloc (cnv, 1, TY_CHAR)
		Memc[cnv] = EOS
	    } else
		call salloc (cnvdata, nc, TY_REAL)
	}

	# Setup bad pixel mask.
	if (bpm == NULL) {
	    call salloc (bp, nc, TY_INT)
	    call aclri (Memi[bp], nc)
	}

	# Go through image creating low resolution sky blocks.
	im[1] = in; im[2] = NULL
	do l = 1, nl, blkstep {
	    call convolve (im, bpm, skymap, sigmap, expmap, 0,
		1., l, Memc[cnv], indata, bp, cnvdata, skydata,
		sigdata, expdata, cnvwt, logfd)
	    call skb_accum (skb, l, blkstep, Memr[cnvdata], Memr[skydata],
		Memr[sigdata], Memr[expdata], Memi[bp], nc, cnvwt)
	}

	# Free convolution memory.
	call convolve (im, bpm, skymap, sigmap, expmap, 0,
	    1., 0, Memc[cnv], indata, bp, cnvdata, skydata,
	    sigdata, expdata, cnvwt, logfd)

	# Turn the sky blocks into sky maps.
	call skb_update (skb, dosky, dosig, in, skyname, signame,
	    skymap, sigmap, logfd)

	# Free memory.
	call skb_imfree (skb)
	call sfree (sp)
end


# SKB_IMINIT -- Initialize parameters and allocate memory for an image.

procedure skb_iminit (skb, im, expmap, blkstep, logfd)

pointer	skb			#U Sky block structure
pointer	im			#I Image pointer
pointer	expmap			#I Exposure map pointer
int	blkstep			#U Line step for speed
int	logfd			#I Log file descriptor

int	nc, nl

begin
	# Number of pixels per subblock.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	if (SKB_BLKSIZE(skb) < 0) {
	    if (nc < nl) {
		SKB_NCSPIX(skb) = max (SKB_NMINPIX(skb),
		    nc / (SKB_NSUBBLKS(skb) * max(1,-SKB_BLKSIZE(skb))))
		SKB_NLSPIX(skb) = SKB_NCSPIX(skb)
	    } else {
		SKB_NLSPIX(skb) = max (SKB_NMINPIX(skb),
		    nl / (SKB_NSUBBLKS(skb) * max(1,-SKB_BLKSIZE(skb))))
		SKB_NCSPIX(skb) = SKB_NLSPIX(skb)
	    }
	} else {
	    SKB_NCSPIX(skb) = max (SKB_NMINPIX(skb),
		min (nc, SKB_BLKSIZE(skb)) / SKB_NSUBBLKS(skb))
	    SKB_NLSPIX(skb) = max (SKB_NMINPIX(skb),
		min (nl, SKB_BLKSIZE(skb)) / SKB_NSUBBLKS(skb))
	}

	# Number of subblocks, blocks, and number of pixels per block.
	SKB_NCSBLK(skb) = max (1, nc / SKB_NCSPIX(skb))
	SKB_NLSBLK(skb) = max (1, nl / SKB_NLSPIX(skb))
	SKB_NCBLK(skb) = (SKB_NCSBLK(skb)+SKB_NSUBBLKS(skb)-1)/SKB_NSUBBLKS(skb)
	SKB_NLBLK(skb) = (SKB_NLSBLK(skb)+SKB_NSUBBLKS(skb)-1)/SKB_NSUBBLKS(skb)
	SKB_NCPIX(skb) = SKB_NCSPIX(skb) * SKB_NSUBBLKS(skb)
	SKB_NLPIX(skb) = SKB_NLSPIX(skb) * SKB_NSUBBLKS(skb)

	# Each subblock must have at least  SKYMIN or FRAC sky pixels.
	SKB_NSKYMIN(skb) = min (SKB_SKYMIN(skb),
	     nint (SKB_FRAC(skb) * SKB_NCSPIX(skb) * SKB_NLSPIX(skb)))

	# Histogram parameters.
	SKB_NAV(skb) = nint (real(SKB_NBINS(skb)) / (min (SKB_NBINS(skb),
	    SKB_NCSPIX(skb) * SKB_NLSPIX(skb) / SKB_NMINBINS(skb))))
	SKB_NAV(skb) = SKB_NAV(skb) + mod (SKB_NAV(skb)+1, 2)
	#SKB_NAV(skb) = 1

	# Set line subsampling for speed.
	if (blkstep > 1) {
	    blkstep = min (1 + SKB_NLSPIX(skb) / 30, blkstep)
	    SKB_NSKYMIN(skb) = SKB_NSKYMIN(skb) / blkstep
	}

	# Allocate and initialize memory.
	call calloc (SKB_BINS(skb), SKB_NBINS(skb)*(SKB_NCSBLK(skb)+1), TY_INT)
	call calloc (SKB_NSKY(skb), SKB_NCSBLK(skb), TY_INT)
	call calloc (SKB_SKYS(skb), SKB_NCSBLK(skb)*SKB_NLSBLK(skb), TY_REAL)
	call calloc (SKB_SIGS(skb), SKB_NCSBLK(skb)*SKB_NLSBLK(skb), TY_REAL)
	if (expmap == NULL) {
	    call malloc (SKB_EXP(skb), 1, TY_REAL)
	    Memr[SKB_EXP(skb)] = INDEFR
	} else
	    call calloc (SKB_EXP(skb), SKB_NCSBLK(skb), TY_REAL)

	# Set pointers to first line of blocks.
	SKB_SKY(skb) = SKB_SKYS(skb)
	SKB_SIG(skb) = SKB_SIGS(skb)

	if (logfd != NULL) {
	    call fprintf (logfd, "      Number of blocks: %d %d\n")
	        call pargi (SKB_NCBLK(skb))
	        call pargi (SKB_NLBLK(skb))
	    call fprintf (logfd, "      Number of pixels per block: %d %d\n")
	        call pargi (SKB_NCPIX(skb))
	        call pargi (SKB_NLPIX(skb))
	    call fprintf (logfd, "      Number of subblocks: %d %d\n")
	        call pargi (SKB_NCSBLK(skb))
	        call pargi (SKB_NLSBLK(skb))
	    call fprintf (logfd, "      Number of pixels per subblock: %d %d\n")
	        call pargi (SKB_NCSPIX(skb))
	        call pargi (SKB_NLSPIX(skb))
	    if (blkstep > 1) {
		call fprintf (logfd, "      Line sampling step: %d\n")
		    call pargi (blkstep)
	    }
	}
end


# SKB_IMFREE -- Free memory for an image.

procedure skb_imfree (skb)

pointer	skb			#I Sky block structure

begin
	call mfree (SKB_BINS(skb), TY_INT)
	call mfree (SKB_NSKY(skb), TY_INT)
	call mfree (SKB_SKYS(skb), TY_REAL)
	call mfree (SKB_SIGS(skb), TY_REAL)
	call mfree (SKB_EXP(skb), TY_REAL)
end


# SKB_ACCUM -- Accumulate sky pixels in block histograms.
# Evaluate histograms when a block is complete.

procedure skb_accum (skb, line, blkstep, cnv, sky, sig, exp, bp, nc, cnvwt)

pointer	skb			#I Sky block structure
int	line			#I Line
int	blkstep			#I Line step
real	cnv[nc]			#I Convolved image data
real	sky[nc]			#I Sky data
real	sig[nc]			#I Sky sigma data
real	exp[nc]			#I Exposure data
int	bp[nc]			#I Bad pixel values
int	nc			#I Number of columns
real	cnvwt			#I Sigma weight

real	a, b, s, t, rcnv, tcnv
int	c, n, ncmax, nbins, bin, csky
pointer	bins, skys, sigs, exps, nsky

begin
	if (line > SKB_NLSBLK(skb) * SKB_NLSPIX(skb))
	    return
	ncmax = min (nc, SKB_NCSBLK(skb) * SKB_NCSPIX(skb))

	a = SKB_A(skb)
	b = SKB_B(skb)
	n = SKB_NCSPIX(skb)
	nbins = SKB_NBINS(skb)
	bins = SKB_BINS(skb)
	skys = SKB_SKY(skb)
	sigs = SKB_SIG(skb)
	exps = SKB_EXP(skb)
	nsky = SKB_NSKY(skb)

	if (IS_INDEFR(Memr[exps])) {
	    do c = 1, ncmax {
		if (bp[c] != 0)
		    next

		s = sky[c]
		t = sig[c]
		rcnv = cnv[c] - s
		tcnv = t / cnvwt
		bin = a * rcnv / tcnv + b
		if (bin < 1 || bin > nbins)
		    next

		csky = (c-1) / n
		bin = bins + csky * nbins + bin - 1
		Memi[bin] = Memi[bin] + 1
		Memr[skys+csky] = Memr[skys+csky] + s
		Memr[sigs+csky] = Memr[sigs+csky] + t
		Memi[nsky+csky] = Memi[nsky+csky] + 1
	    }
	} else {
	    do c = 1, ncmax {
		if (bp[c] != 0)
		    next

		s = sky[c]
		t = sig[c]
		rcnv = cnv[c] - s
		tcnv = t / cnvwt
		bin = a * rcnv / tcnv + b
		if (bin < 1 || bin > nbins)
		    next

		csky = (c-1) / n
		bin = bins + csky * nbins + bin - 1
		Memi[bin] = Memi[bin] + 1
		Memr[skys+csky] = Memr[skys+csky] + s
		Memr[sigs+csky] = Memr[sigs+csky] + t
		Memr[exps+csky] = Memr[exps+csky] + exp[c]
		Memi[nsky+csky] = Memi[nsky+csky] + 1
	    }
	}

	# Evaluate histogram sky values if all lines have been accumulated.
	n = mod (line, SKB_NLSPIX(skb))
	if (n == 0 || n + blkstep > SKB_NLSPIX(skb)) {
	    n = SKB_NCSBLK(skb)
	    call skb_blkeval (Memi[bins], nbins, a, b, Memr[skys], Memr[sigs],
		Memr[exps], Memi[nsky], n, SKB_NSKYMIN(skb), SKB_NAV(skb),
		SKB_HISTWT(skb), SKB_SIGFAC(skb))

	    # Initialize for accumulation of next line of blocks.
	    SKB_SKY(skb) = skys + n
	    SKB_SIG(skb) = sigs + n
	    if (!IS_INDEFR(Memr[exps]))
		call aclrr (Memr[exps], n) 
	    call aclri (Memi[nsky], n) 
	    call aclri (Memi[bins], n*nbins) 
	}
end


# SKB_BLKEVAL -- Evaluate sky and sigma for each histogram in line of blocks.
# Set to INDEF if there are not enough pixels in the histogram.

procedure skb_blkeval (bins, nbins, a, b, skys, sigs, exps, nsky, ncsblk,
	nskymin, nav, histwt, sigfac)

int	bins[nbins,ncsblk]	#I Sky subblock bins
int	nbins			#I Number of bins
real	a, b			#I Binning coefficients
real	skys[ncsblk]		#U Sky sum in, sky estimate out
real	sigs[ncsblk]		#U Sigma sum in, sigma estimate out
real	exps[ncsblk]		#I Exposure sum
int	nsky[ncsblk]		#I Number of values in bin
int	ncsblk			#I Number of sky pixels per subblock
int	nskymin			#I Minimum number of sky pixels for good sky
int	nav			#I Number of bins to average
int	histwt			#I Histogram weighting power
real	sigfac			#I Sigma conversion factor from mean abs dev.

int	i, j, k, l, m, n
double	sky, sig, exp, x, wt, skymean, skymed, skybin, sigbin
double	sum1, sum2, sum3

begin
#	do i = 1, ncsblk {
#	    do j = 1, nbins {
#		call printf ("%d\n")
#		    call pargi (bins[j,i])
#	    }
#	}
	m = nav / 2
	do i = 1, ncsblk {
	    n = nsky[i]
	    if (n < nskymin) {
		skys[i] = INDEFR
		sigs[i] = INDEFR
		next
	    }

	    sky = skys[i] / n
	    sig = sigs[i] / n
	    if (!IS_INDEFR(exps[1])) {
		exp = exps[i] / n
		exps[i] = exp
	    } else
		exp = 1

	    # Compute mean and median using a power weighting of the histogram.
	    sum1 = 0.
	    sum2 = 0.
	    sum3 = 0.
	    k = ncsblk + 1
	    call aclri (bins[1,k], nbins)
	    do j = 1, nbins {
		n = bins[j,i]
		do l = max(1,j-m), min (nbins,j+m)
		    bins[l,k] = bins[l,k] + n
	    }
	    n = nsky[i]
	    switch (histwt) {
	    case 1:
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    x = j
		    sum1 = sum1 + wt * x
		    sum2 = sum2 + wt
		}
		sum2 = sum2
		x = 0
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    sum3 = sum3 + wt + x
		    if (sum3 >= sum2)
		       break
		    x = wt
		}
	    case 2:
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt
		    x = j
		    sum1 = sum1 + wt * x
		    sum2 = sum2 + wt
		}
		sum2 = sum2
		x = 0
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt
		    sum3 = sum3 + wt + x
		    if (sum3 >= sum2)
		       break
		    x = wt
		}
	    case 3:
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt * wt
		    x = j
		    sum1 = sum1 + wt * x
		    sum2 = sum2 + wt
		}
		sum2 = sum2
		x = 0
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt * wt
		    sum3 = sum3 + wt + x
		    if (sum3 >= sum2)
		       break
		    x = wt
		}
	    case 4:
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt
		    wt = wt * wt
		    x = j
		    sum1 = sum1 + wt * x
		    sum2 = sum2 + wt
		}
		sum2 = sum2
		x = 0
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt * wt
		    wt = wt * wt
		    sum3 = sum3 + wt + x
		    if (sum3 >= sum2)
		       break
		    x = wt
		}
	    default:
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt ** histwt
		    x = j
		    sum1 = sum1 + wt * x
		    sum2 = sum2 + wt
		}
		sum2 = sum2
		x = 0
		do j = 1, nbins {
		    wt = real (bins[j,k]) / n
		    wt = wt ** histwt
		    sum3 = sum3 + wt + x
		    if (sum3 >= sum2)
		       break
		    x = wt
		}
	    }
	    skymean = sum1 / sum2
	    skymed = j - (sum3 - sum2) / (wt + x)
	    #skybin = skymean - max (0D0, 3 * (skymean - skymed))
	    skybin = skymean - 3 * (skymean - skymed)
	    #skybin = skymean
	    skys[i] = ((skybin + 0.5 - b) / a) * sig + sky

	    sum1 = 0.
	    sum2 = 0.
	    do j = 1, nbins {
		wt = bins[j,k]
		x = abs (j - skybin)
		sum1 = sum1 + wt * x
		sum2 = sum2 + wt
	    }
	    sigbin = sum1 / sum2
	    sigs[i] = sigbin / a * sig * sqrt (exp) * sigfac
	}
end


# SKB_UPDATE -- Update the sky and sigma maps using the block values.

procedure skb_update (skb, dosky, dosig, im, skyname, signame,
	skymap, sigmap, logfd)

pointer	skb			#I Sky block structure
bool	dosky			#I Compute sky
bool	dosig			#I Compute sigma
pointer	im			#I Image pointer
char	skyname[ARB]		#I Output sky map name
char	signame[ARB]		#I Output sigma map name
pointer	skymap			#U Sky map pointer
pointer	sigmap			#U Sigma map pointer
int	logfd			#I Log file descriptor

bool	skydebug, sigdebug
pointer	sp, fname, tmp, map_open()
errchk	skb_wmap, skb_grow, skb_merge, skb_wmap, map_close, map_open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)


	if (dosky) {
	    skydebug = false
	    if (skydebug)
		call skb_wmap ("skydebug.fits", im, SKB_SKYS(skb),
		    SKB_NCSBLK(skb), SKB_NLSBLK(skb), SKB_NCSPIX(skb),
		    SKB_NLSPIX(skb), 0., NULL)

	    # Grow subblocks contaminated by large objects.
	    call skb_grow (SKB_SKYS(skb), SKB_NCSBLK(skb), SKB_NLSBLK(skb),
		SKB_GROW(skb))

	    if (skydebug)
		call skb_wmap ("skydebug.fits", im, SKB_SKYS(skb),
		    SKB_NCSBLK(skb), SKB_NLSBLK(skb), SKB_NCSPIX(skb),
		    SKB_NLSPIX(skb), 0., NULL)

	    # Merge sky from subblocks and interpolate missing regions.
	    call skb_merge (Memr[SKB_SKYS(skb)], SKB_NCSBLK(skb),
		SKB_NLSBLK(skb), Memr[SKB_SKYS(skb)], SKB_NCBLK(skb),
		SKB_NLBLK(skb))

	    # Write block maps and map them with the MAPIO interface.
	    # If no name is given then use a temporary image.
	    if (skyname[1] == EOS) {
		call mktemp ("tmpsky", Memc[fname], SZ_FNAME)
		call skb_wmap (Memc[fname], im, SKB_SKYS(skb),
		    SKB_NCBLK(skb), SKB_NLBLK(skb), SKB_NCPIX(skb),
		    SKB_NLPIX(skb), INDEFR, NULL)
	    } else {
		call strcpy (skyname, Memc[fname], SZ_FNAME)
		call skb_wmap (Memc[fname], im, SKB_SKYS(skb),
		    SKB_NCBLK(skb), SKB_NLBLK(skb), SKB_NCPIX(skb),
		    SKB_NLPIX(skb), INDEFR, logfd)
	    }
	    tmp = skymap
	    iferr (skymap = map_open (Memc[fname], im))
		skymap = NULL
	    if (skymap == NULL) {
		skymap = tmp
		call error (1, "Could not update sky")
	    }
	    call map_close (tmp)
	    if (skyname[1] == EOS)
		call map_seti (skymap, "delete", YES)
	}

	if (dosig) {
	    sigdebug = false
	    if (sigdebug)
		call skb_wmap ("sigdebug.fits", im, SKB_SIGS(skb),
		    SKB_NCSBLK(skb), SKB_NLSBLK(skb), SKB_NCSPIX(skb),
		    SKB_NLSPIX(skb), 0., NULL)

	    # Grow subblocks contaminated by large objects.
	    call skb_grow (SKB_SIGS(skb), SKB_NCSBLK(skb), SKB_NLSBLK(skb),
		SKB_GROW(skb))

	    if (sigdebug)
		call skb_wmap ("sigdebug.fits", im, SKB_SIGS(skb),
		    SKB_NCSBLK(skb), SKB_NLSBLK(skb), SKB_NCSPIX(skb),
		    SKB_NLSPIX(skb), 0., NULL)

	    # Merge sky sigma from subblocks and interpolate missing regions.
	    call skb_merge (Memr[SKB_SIGS(skb)], SKB_NCSBLK(skb),
		SKB_NLSBLK(skb), Memr[SKB_SIGS(skb)], SKB_NCBLK(skb),
		SKB_NLBLK(skb))

	    # Write block maps and map them with the MAPIO interface.
	    # If no name is given then use a temporary image.
	    if (signame[1] == EOS) {
		call mktemp ("tmpsig", Memc[fname], SZ_FNAME)
		call skb_wmap (Memc[fname], im, SKB_SIGS(skb),
		    SKB_NCBLK(skb), SKB_NLBLK(skb), SKB_NCPIX(skb),
		    SKB_NLPIX(skb), INDEFR, NULL)
	    } else {
		call strcpy (signame, Memc[fname], SZ_FNAME)
		call skb_wmap (Memc[fname], im, SKB_SIGS(skb),
		    SKB_NCBLK(skb), SKB_NLBLK(skb), SKB_NCPIX(skb),
		    SKB_NLPIX(skb), INDEFR, logfd)
	    }
	    tmp = sigmap
	    iferr (sigmap = map_open (Memc[fname], im))
		sigmap = NULL
	    if (sigmap == NULL) {
		sigmap = tmp
		call error (1, "Could not update sky sigma")
	    }
	    call map_close (tmp)
	    if (signame[1] == EOS)
		call map_seti (sigmap, "delete", YES)
	}

	call sfree (sp)
end


# SKB_GROW -- Grow around subblocks with insufficient data.

procedure skb_grow (sky, nc, nl, grow)

pointer	sky			# Pointer to real sky array to be grown
int	nc, nl			# Size of sky array
real	grow			# Grow radius

int	i, j, k, l1, l2, ngrow, nbufs
real	grow2, val1, val2, y2
pointer	buf, buf1, buf2, ptr
errchk	calloc

begin
	# Initialize.
	ngrow = int (grow)
	grow2 = grow * grow
	nbufs = min (1 + 2 * ngrow, nl)
	call calloc (buf, nc*nbufs, TY_REAL)

	l1 = 1; l2 = 1
	while (l1 <= nl) {
	    buf1 = sky + (l1 - 1) * nc
	    buf2 = buf + mod (l1, nbufs) * nc
	    do i = 1, nc {
		val1 = Memr[buf1]
		val2 = Memr[buf2]
		if (IS_INDEFR(val1)) {
		    do j = max(1,l1-ngrow), min (nl,l1+ngrow) {
			ptr = buf + mod (j, nbufs) * nc - 1
			y2 = (j - l1) ** 2
			do k = max(1,i-ngrow), min (nc,i+ngrow) {
			    if ((k-i)**2 + y2 > grow2)
				next
			    Memr[ptr+k] = INDEFR
			}
		    }
		} else if (!IS_INDEFR(val2))
		    Memr[buf2] = val1
		buf1 = buf1 + 1
		buf2 = buf2 + 1
	    }

	    if (l1 > ngrow) {
		while (l2 <= nl) {
		    buf1 = sky + (l2 - 1) * nc
		    buf2 = buf + mod (l2, nbufs) * nc
		    do i = 1, nc {
			Memr[buf1] = Memr[buf2]
			Memr[buf2] = 0
			buf1 = buf1 + 1
			buf2 = buf2 + 1
		    }
		    l2 = l2 + 1
		    if (l1 != nl)
			break
		}
	    }
	    l1 = l1 + 1
	}

	call mfree (buf, TY_REAL)
end


# SKB_MERGE -- Merge subblock into blocks.
# Use average of subblocks with minimum and maximum excluded.

procedure skb_merge (in, ncin, nlin, out, ncout, nlout)

real	in[ncin,nlin]
int	ncin, nlin
real	out[ncout,nlout]
int	ncout, nlout

int	ncs, nls
int	i, i1, i2, iout, j, j1, j2, jout, n, nindef
real	val, sum, minval, maxval
pointer	work

begin
	# Number of input subblocks per output block.
	ncs = nint (real (ncin) / ncout)
	nls = nint (real (nlin) / nlout)

	nindef = 0
	j2 = 0; jout = 0
	do j1 = 1, nlin, nls {
	    jout = jout + 1
	    j2 = min (nlin, j2 + nls)
	    i2 = 0; iout = 0
	    do i1 = 1, ncin, ncs {
		iout = iout + 1
		i2 = min (ncin, i2 + ncs)

		n = 0
		sum = 0.
		minval = MAX_REAL
		maxval = -MAX_REAL
		do j = j1, j2 {
		    do i = i1, i2 {
			if (IS_INDEFR(in[i,j]))
			    next
			val = in[i,j]
			sum = sum + val
			minval = min (val, minval)
			maxval = max (val, maxval)
			n = n + 1
		    }
		}
		if (n > 2)
		    out[iout,jout] = (sum - minval - maxval) / (n - 2)
		else if (n >= min (ncs, nls))
		    out[iout,jout] = sum / n
		else {
		    out[iout,jout] = INDEFR
		    nindef = nindef + 1
		}
	    }
	}

	# Interpolate to fill in blocks with no sky data.
	if (nindef > 0) {
	    call malloc (work, ncout*nlout, TY_REAL)
	    call interp2 (out, Memr[work], ncout, nlout)
	    call amovr (Memr[work], out, ncout*nlout)
	    call mfree (work, TY_REAL)
	}
end


## SKB_ESTIMATE -- Estimate of sky in block from subblocks.
## Use order selection.
#
#procedure skb_merge (in, ncin, nlin, out, ncout, nlout, select)
#
#real	in[ncin,nlin]
#int	ncin, nlin
#real	out[ncout,nlout]
#int	ncout, nlout
#real	select			# Selection fraction
#
#int	ncs, nls
#int	i, i1, i2, iout, j, j1, j2, jout, n, nindef, nselect
#pointer	sp, work, ptr
#real	asokr()
#
#begin
#	# Number of input subblocks per output block.
#	ncs = nint (real (ncin) / ncout)
#	nls = nint (real (nlin) / nlout)
#
#	call smark (sp)
#	call salloc (work, ncs*nls, TY_REAL)
#
#	nindef = 0
#	j2 = 0; jout = 0
#	do j1 = 1, nlin, nls {
#	    jout = jout + 1
#	    j2 = min (nlin, j2 + nls)
#	    i2 = 0; iout = 0
#	    do i1 = 1, ncin, ncs {
#		iout = iout + 1
#		i2 = min (ncin, i2 + ncs)
#		ptr = work
#		do j = j1, j2 {
#		    do i = i1, i2 {
#			if (IS_INDEFR(in[i,j]))
#			    next
#			Memr[ptr] = in[i,j]
#			ptr = ptr + 1
#		    }
#		}
#		n = ptr - work
#		if (n >= min (ncs, nls)) {
#		    nselect = nint (select * (n - 1)) + 1
#		    out[iout,jout] = asokr (Memr[work], n, nselect)
#		} else {
#		    out[iout,jout] = INDEFR
#		    nindef = nindef + 1
#		}
#	    }
#	}
#
#	# Interpolate to fill in blocks with no sky data.
#	if (nindef > 0) {
#	    call salloc (work, ncout*nlout, TY_REAL)
#	    call interp2 (out, Memr[work], ncout, nlout)
#	    call amovr (Memr[work], out, ncout*nlout)
#	}
#
#	call sfree (sp)
#end


# SKB_WMAP -- Write map from block data.

procedure skb_wmap (name, imref, data, ncblk, nlblk, ncpix, nlpix, blank, logfd)

char	name[ARB]		#I Output name
pointer	imref			#I Reference image pointer
pointer	data			#I Block image data
int	ncblk, nlblk		#I Block image dimensions
int	ncpix, nlpix		#I Number of reference image pixels per block
real	blank			#I Blank value
int	logfd			#I Log file descriptor

bool	strne()
int	i, j, imaccess(), strlen(), stridxs()
real	a[2]
pointer	sp, title, str
pointer	im, mw, buf, immap(), impl2r(), mw_openim()
errchk	immap, imrename

begin
	call smark (sp)
	call salloc (title, SZ_IMTITLE, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Create title for new image or to check for updating.
	call sprintf (Memc[title], SZ_IMTITLE, "Sky for ")
	i = strlen (Memc[title])
	call imstats (imref, IM_IMAGENAME, Memc[title+i], SZ_IMTITLE-i)

	iferr {
	    im = NULL; mw = NULL

#	    # Check for existing image and rename.
#	    if (imaccess (name, 0) == YES) {
#		j = strlen (name)
#		call malloc (fname, j+SZ_FNAME, TY_CHAR)
#		i = strldxs (".", name) - 1
#		if (i < 0)
#		    i = j
#		do j = 1, ARB {
#		    call strcpy (name, Memc[fname], i)
#		    call sprintf (Memc[fname+i], SZ_FNAME, "%d%s")
#			call pargi (j)
#			call pargstr (name[i+1])
#		    if (imaccess (Memc[fname], 0) == YES)
#			next
#		    call imrename (name, Memc[fname])
#		    break
#		}
#		call mfree (fname, TY_CHAR)
#	    }

	    if (imaccess (name, 0) == NO) {
		if (logfd != NULL) {
		    call strcpy (name, Memc[str], SZ_FNAME)
		    i = stridxs (",", Memc[str])
		    if (i > 0) {
		        Memc[str+i-1] = ']'
			Memc[str+i] = EOS
		    }
		    call fprintf (logfd, "    Write sky map: %s\n")
			call pargstr (Memc[str])
		}
		buf = immap (name, NEW_COPY, imref); im = buf
		IM_PIXTYPE(im) = TY_REAL
		IM_LEN(im,1) = ncblk
		IM_LEN(im,2) = nlblk
		call strcpy (Memc[title], IM_TITLE(im), SZ_IMTITLE)
		iferr (call imdelf (im, "BPM"))
		    ;
		iferr (call imdelf (im, "DATASEC"))
		    ;
		iferr (call imdelf (im, "TRIMSEC"))
		    ;

		do i = 1, nlblk {
		    buf = impl2r(im,i)
		    call amovr (Memr[data+(i-1)*ncblk], Memr[buf], ncblk)
		    if (!IS_INDEFR(blank)) {
			do j = 1, ncblk
			    if (IS_INDEFR(Memr[buf+j-1]))
				Memr[buf+j-1] = blank
		    }
		}

		# Update the WCS.
		mw = mw_openim (imref)
		a[1] = 1. / ncpix
		a[2] = 1. / nlpix
		call mw_scale (mw, a, 3)
		a[1] = 0.5
		a[2] = 0.5
		call mw_shift (mw, a, 3)
		call mw_saveim (mw, im)
	    } else {
		if (logfd != NULL) {
		    call strcpy (name, Memc[str], SZ_FNAME)
		    i = stridxs (",", Memc[str])
		    if (i > 0) {
		        Memc[str+i-1] = ']'
			Memc[str+i] = EOS
		    }
		    call fprintf (logfd, "    Update sky map: %s\n")
			call pargstr (Memc[str])
		}
		buf = immap (name, READ_WRITE, 0); im = buf
		if (strne (IM_TITLE(im), Memc[title]) ||
		    IM_LEN(im,1) != ncblk || IM_LEN(im,2) != nlblk)
		    call error (1, "Cannot update sky map")

		do i = 1, nlblk {
		    buf = impl2r(im,i)
		    call amovr (Memr[data+(i-1)*ncblk], Memr[buf], ncblk)
		    if (!IS_INDEFR(blank)) {
			do j = 1, ncblk
			    if (IS_INDEFR(Memr[buf+j-1]))
				Memr[buf+j-1] = blank
		    }
		}
	    }
	} then
	    call erract (EA_WARN)

	if (mw != NULL)
	    call mw_close (mw)
	if (im != NULL)
	    call imunmap (im)
	call sfree (sp)
end


# INTERP2 -- Interpolate 2D array by averaging 1D interpolations along lines
# and columns.  It is an error if there is no data to interpolate.

procedure interp2 (in, out, nc, nl)

real	in[nc,nl]		# Input data
real	out[nc,nl]		# Output data (not the same as input)
int	nc, nl			# Size of data

int	i, j, k1, k2, nerr
pointer	sp, flags, buf

begin
	call smark (sp)
	call salloc (flags, nl, TY_INT)
	call salloc (buf, nl, TY_REAL)

	call amovki (OK, Memi[flags], nl)

	# Interpolate along lines.  Flag lines with no data.
	nerr = 0
	do i = 1, nl
	    iferr (call interp1 (in[1,i], out[1,i], nc)) {
		Memi[flags+i-1] = ERR
		nerr = nerr + 1
	    }

	if (nerr == nl)
	    call error (1, "No data to interpolate")

	# Interpolate along columns.  Check for columns and lines with no data.
	do j = 1, nc {
	    do i = 1, nl
		Memr[buf+i-1] = in[j,i]

	    ifnoerr (call interp1 (Memr[buf], Memr[buf], nl)) {
		do i = 1, nl {
		    if (Memi[flags+i-1] == OK)
			out[j,i] = (out[j,i] + Memr[buf+i-1]) / 2.
		    else
			out[j,i] = Memr[buf+i-1]
		}
	    } else {
		do i = 1, nl {
		    if (Memi[flags+i-1] == ERR) {
			# Find nearest line with good data.
			do k1 = i-1, 1, -1
			    if (Memi[flags+k1-1] == OK)
				break
			do k2 = i+1, nl
			    if (Memi[flags+k2-1] == OK)
				break
			if (k1 >= 1 & k2 <= nl) {
			    if (i - k1 < k2 - i)
				out[j,i] = out[j,k1]
			    else
				out[j,i] = out[j,k2]
			} else if (k1 >= 1)
			    out[j,i] = out[j,k1]
			else if (k2 <= nl)
			    out[j,i] = out[j,k2]
		    }
		}
	    }
	}
	call sfree (sp)
end


# INTERP1 -- Interpolate 1D vectors.
# An error is generated if there is no data to interpolate.

procedure interp1 (in, out, npts)

real	in[npts]		# Input line
real	out[npts]		# Output line (may be the same as input)
int	npts			# Number of points in line

int	i, i1, i2, j
real	v, v1, dv

begin
	i1 = 0
	i2 = 1
	do i = 1, npts {
	    v = in[i]
	    if (IS_INDEFR(v))
		next
	    if (i > i2) {
		if (i1 > 0) {
		    dv = (v - v1) / (i - i1)
		    do j = i2, i-1
			out[j] = v + dv * (j - i)
		} else {
		    do j = i2, i-1
			out[j] = v
		}
	    }
	    out[i] = v
	    v1 = v
	    i1 = i
	    i2 = i1+1
	}

	if (i1 == 0)
	    call error (1, "No data to interpolate")
	else if (i2 <= npts) {
	    do j = i2, npts
		out[j] = v1
	}
	    
end
