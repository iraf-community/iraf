include	<imhdr.h>
include	<pmset.h>
include	<mach.h>
include	"ace.h"
include	"cat.h"
include	"objs.h"
include	"skyblock.h"
include	"detect.h"
include	"split.h"


# DETECT - Object detection.
#
# Get input image data (possibly convolved) and compare to sky using sky
# sigma and threshold factors.  Catagorize as bad pixel, sky, above sky, and
# below sky.  Write catagories to output mask.

procedure detect (det, spt, dosky, dosig, skyname, signame, im, bpm,
	skymap, sigmap, expmap, scale, offset, out, siglevmap, siglevels,
	logfd, cat)

pointer	det			#I Detection parameter structure
pointer	spt			#I Split parameter structure
bool	dosky			#I Do sky update?
bool	dosig			#I Do sigma update?
char	skyname[ARB]		#I Sky name for updating sky
char	signame[ARB]		#I Sigma name for updating sigma
pointer	im[2]			#I Input image pointers
pointer	bpm[2]			#I Bad pixel mask pointer
pointer	skymap[2]		#U Sky map
pointer	sigmap[2]		#U Sigma map
pointer	expmap[2]		#I Exposure map
real	scale[2]		#I Image scales
int	offset[2]		#I Offsets of second image
pointer	out			#I Output pixel mask (PMIO) pointer
pointer	siglevmap		#I Mask for sigma levels
pointer	siglevels		#O Sigma levels for mask
int	logfd			#I Verbose?
pointer	cat			#O Catalog of objects

pointer	cnv			# Convolution string pointer
real	hsig			# Detection threshold
real	splitstep		# Minimum split step in convolved sigma
real	splitthresh		# Transition convolved sigma
bool	hdetect			# Detection above sky
bool	ldetect			# Detection below sky

bool	dosky1, dosig1, overlap
int	i, c, l, nc, nl, nc2, siglevmax
int	nobjs, nalloc, navail
long	v[2]
real	z, cnvwt
pointer	sp, str, iptr, rptr, outdata, lastdata, orl, srl
pointer	skb, objs, ids, links
pointer	indata[2], bp, skydata[2], sigdata[2], expdata[2], cnvdata

errchk	convolve, drenum
errchk	detect, salloc, malloc, calloc, realloc


begin
	# Initialize parameters.
	call det_pars ("open", "", det)

	# The sky update requires the doxxx parameter to be true, a filename
	# to be specified and the skb pointer to be non-null.  The skb
	# pointer is set depending on the "updatesky" task parameter.

	dosky1 = (dosky && skyname[1] != EOS)
	dosig1 = (dosig && signame[1] != EOS)
	if (dosky1 || dosig1)
	    skb = DET_SKB(det)
	else
	    skb = NULL

	cnv = DET_CNV(det)
	hsig = DET_HSIG(det)
	if (spt != NULL) {
	    splitstep = SPT_SPLITSTEP(spt)
	    splitthresh = SPT_SPLITTHRESH(spt)
	}
	hdetect = (DET_HDETECT(det) == YES)
	ldetect = (DET_LDETECT(det) == YES)

	# Set sizes.
	nc = IM_LEN(im[1],1)
	nl = IM_LEN(im[1],2)
	if (ldetect)
	    nc2 = 2 * (nc + 2)
	else
	    nc2 = nc + 2

	# Allocate memory.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (outdata, nc2, TY_INT)
	call salloc (lastdata, nc2, TY_INT)
	call salloc (orl, 3+3*nc, TY_INT)
	call salloc (iptr, 1, TY_REAL)
	call salloc (rptr, 1, TY_REAL)

	Memr[iptr] = INDEFI
	Memr[rptr] = INDEFR

	if (siglevmap != NULL)
	    call salloc (srl, 3+3*nc, TY_INT)
	else
	    srl = iptr

	if (expmap[1] == NULL)
	    expdata[1] = rptr
	if (expmap[2] == NULL)
	    expdata[2] = rptr
	  
	if (im[2] == NULL) {
	    indata[2] = rptr
	    skydata[2] = rptr
	    sigdata[2] = rptr
	    expdata[2] = rptr
	    if (bpm[1] == NULL) {
		call salloc (bp, nc, TY_INT)
		call aclri (Memi[bp], nc)
	    }
	    if (Memc[cnv] != EOS)
		call salloc (cnvdata, nc, TY_REAL)
	} else {
	    overlap = true
	    if (1-offset[1] < 1 || nc-offset[1] > IM_LEN(im[2],1))
		overlap = false
	    if (1-offset[2] < 1 || nl-offset[2] > IM_LEN(im[2],2))
		overlap = false
	    if (!overlap) {
		call salloc (indata[2], nc, TY_REAL)
		call salloc (skydata[2], nc, TY_REAL)
		call salloc (sigdata[2], nc, TY_REAL)
		call salloc (expdata[2], nc, TY_REAL)
	    }
	    call salloc (bp, nc, TY_INT)
	    call aclri (Memi[bp], nc)
	    call salloc (cnvdata, nc, TY_REAL)
	}

	navail = (nc * nl) / 100
	call calloc (ids, navail, TY_INT) 
	call calloc (links, navail, TY_INT) 
	call calloc (objs, navail, TY_POINTER) 
	nalloc = 0

	# Setup sky updating.
	if (skb!=NULL && !overlap) {
	    l = 1
	    call skb_iminit (skb, im[1], expmap, l, NULL)
	}

	if (logfd != NULL)
	    call fprintf (logfd, "  Detect objects:\n")

	# Go through image.
	nobjs = NUMSTART - 1
	call aclri (Memi[outdata], nc2)
	if (siglevmap == NULL)
	    siglevmax = INDEFI
	else
	    siglevmax = 0

	v[1] = 1
	do l = 1, nl {
	    # Get data.
	    call convolve (im, bpm, skymap, sigmap, expmap, offset,
		scale, l, Memc[cnv], indata, bp, cnvdata, skydata,
		sigdata, expdata, cnvwt, logfd)
	    call amovi (Memi[outdata], Memi[lastdata], nc2)

	    call detect1 (det, spt, skb, Memr[indata[1]], Memr[skydata[1]],
		Memr[sigdata[1]], Memr[expdata[1]], Memr[indata[2]],
		Memr[skydata[2]], Memr[sigdata[2]], Memr[expdata[2]],
		scale, Memi[bp], Memr[cnvdata], cnvwt, Memi[outdata],
		Memi[lastdata], nc, nl, l, objs, ids, links, nobjs,
		nalloc, navail, Memi[orl], Memi[srl], siglevmax)

	    # Write to output masks.
	    v[2] = l
	    call pmplri (out, v, Memi[orl], 0, nc, PIX_SRC) 
	    if (siglevmap != NULL)
		call pmplri (siglevmap, v, Memi[srl], 0, nc, PIX_SRC) 
	}

	# Free convolution memory.
	call convolve (im, bpm, skymap, sigmap, expmap, offset,
	    scale, 0, Memc[cnv], indata, bp, cnvdata, skydata,
	    sigdata, expdata, cnvwt, logfd)

	# Free extra object structures.
	do c = nobjs, nalloc-1
	    call mfree (Memi[objs+c], TY_STRUCT)

	# Renumber and reject objects with less than a minimum area.
	call drenum (det, out, Memi[ids], Memi[objs], nobjs)

	call mfree (ids, TY_INT)
	call mfree (links, TY_INT)
	call realloc (objs, nobjs, TY_POINTER)

	CAT_NOBJS(cat) = nobjs
	CAT_NUMMAX(cat) = nobjs
	CAT_OBJS(cat) = objs

	# Set sigma levels if needed.
	if (spt != NULL) {
	    call calloc (siglevels, siglevmax+1, TY_REAL)
	    do i = 1, siglevmax {
		z = i * splitstep
		if (z > splitthresh) {
		    z = z / splitthresh
		    z = (z + 3) / 4
		    z = z * z * z * z
		    z = z * splitthresh
		}
		if (z > hsig)
		    Memr[siglevels+i-1] = z
	    }
	    Memr[siglevels+siglevmax] = MAX_REAL
	} else
	    siglevels = NULL

	if (logfd != NULL) {
	    call fprintf (logfd, "    %d objects detected\n")
		call pargi (nobjs - NUMSTART + 1)
	}

	if (skb != NULL) {
	    call skb_update (skb, dosky1, dosig1, im[1], skyname, signame,
	        skymap, sigmap, logfd)
	    call skb_imfree (skb)
	}

	call sfree (sp)
end


procedure detect1 (det, spt, skb, in, sky, sig, exp, in2, sky2, sig2, exp2,
	scale, bp, cnv, cnvwt, out, lastout, nc, nl, line, objs, ids,
	links, nobjs, nalloc, navail, orl, srl, siglevmax)

pointer	det			#I Parameters
pointer	spt			#I Parameters
pointer	skb			#I Sky block pointer
real	in[nc]			#I Image data
real	sky[nc]			#I Sky data
real	sig[nc]			#I Sky sigma data
real	exp[nc]			#I Exposure map data
real	in2[nc]			#I Image data
real	sky2[nc]		#I Sky data
real	sig2[nc]		#I Sky sigma data
real	exp2[nc]		#I Exposure map data
real	scale[2]		#I Image scales
int	bp[nc]			#I Bad pixel values
real	cnv[nc]			#I Convolved image data
real	cnvwt			#I Sigma weight
int	out[ARB]		#I Output data (extra pixel on each end)
int	lastout[ARB]		#I Last output data (extra pixel on each end)
int	nc			#I Number of columns
int	nl			#I Number of lines
int	line			#I Current line

pointer	objs			#I Pointer to array of object pointers
pointer	ids			#I Pointer to array of IDs
pointer	links			#I Pointer to array links to other IDs
int	nobjs			#I Number of objects pointers
int	nalloc			#I Number of object pointers allocated
int	navail			#I Size of allocated arrays

int	orl[3,ARB]		#O Output object mask range list
int	srl[3,ARB]		#O Output sigma level range list
int	siglevmax		#O Maximum sigma level (INDEF if not used)

real	hsig			# High detection sigma
real	lsig			# Low detection sigma
int	bpval			# Output bad pixel value
real	splitstep		# Minimum split step in convolved sigma
real	splitthresh		# Transition convolved sigma
bool	hdetect			# Detection above sky
bool	ldetect			# Detection below sky
int	neighbors		# Neighbor type

int	i, j, c, c1, c2, clast, nc2, nc3, num, numlast, bin, binlast
int	n, ncmax, nlmax, nbins, csky
real	z, s, t, z1, s1, t1, z2, s2, t2, zcnv, rcnv, tcnv, low, high, binscale
real	explast
bool	dodiff, dosrl

real	a, b
pointer	bins, skys, sigs, exps, nsky

errchk	dadd, realloc

begin
	# Parameters
	hsig = DET_HSIG(det)
	lsig = DET_LSIG(det)
	bpval = DET_BPVAL(det)
	hdetect = (DET_HDETECT(det) == YES)
	ldetect = (DET_LDETECT(det) == YES)
	neighbors = DET_NEIGHBORS(det)

	# Do sky updating?
	nlmax = 0
	if (skb != NULL) {
	    ncmax = min (nc, SKB_NCSBLK(skb) * SKB_NCSPIX(skb))
	    nlmax = min (nl, SKB_NLSBLK(skb) * SKB_NLSPIX(skb))

	    a = SKB_A(skb)
	    b = SKB_B(skb)
	    n = SKB_NCSPIX(skb)
	    nbins = SKB_NBINS(skb)
	    bins = SKB_BINS(skb)
	    skys = SKB_SKY(skb)
	    sigs = SKB_SIG(skb)
	    exps = SKB_EXP(skb)
	    nsky = SKB_NSKY(skb)
	}

	# Do difference detection?
	if (IS_INDEFR(in2[1])) {
	    dodiff = false
	    z1 = 0; s1 = 0; t1 = 1
	    z2 = 0; s2 = 0; t2 = 1
	} else
	    dodiff = true

	# Initialize output mask range lists.
	i = 1
	orl[1,i] = 0
	if (spt != NULL) {
	    splitstep = SPT_SPLITSTEP(spt)
	    splitthresh = SPT_SPLITTHRESH(spt)
	    binscale = splitthresh / splitstep

	    j = 1
	    srl[1,j] = 0
	    dosrl = true
	} else
	    dosrl = false
	clast = 0

	nc2 = nc + 2
	if (ldetect)
	    nc3 = nc2 + 1
	else
	    nc3 = 1

	explast = INDEFR

	# Find pixels which are masked, sky, above sky, and below sky.
	do c = 1, nc {
	    c1 = c + 1
	    c2 = c + nc3
	    out[c1] = 0
	    out[c2] = 0

	    # Mark masked pixels if any.
	    if (bp[c] != 0) {
		if (IS_INDEFI(bpval))
		    num = min (bp[c], NUMSTART-1)
		else
		    num = min (bpval, NUMSTART-1)

		if (num > 0) {
		    out[c1] = num
		    out[c2] = num

		    if (num != numlast || c != clast) {
			orl[2,i] = clast - orl[1,i]
			i = i + 1

			numlast = num
			orl[1,i] = c
			orl[3,i] = numlast
		    }
		    clast = c1
		}

		next
	    }

	    # Find sky and object pixels.
	    if (dodiff) {
		z1 = in[c]
		s1 = sky[c]
		t1 = sig[c]
		z2 = in2[c]
		s2 = sky2[c]
		t2 = sig2[c]
		z = scale[1] * z1 - scale[2] * z2
		s = scale[1] * s1 - scale[2] * s2
		t = sqrt ((scale[1]*t1)**2 + (scale[2]*t2)**2)
	    } else {
		z = in[c]
		s = sky[c]
		t = sig[c]
	    }
	    zcnv = cnv[c]
	    rcnv = zcnv - s
	    tcnv = t / cnvwt
	    low = -lsig * tcnv
	    high = hsig * tcnv

	    if (rcnv > high) {
		if (hdetect) {
		    call dadd (c1, line, out, lastout, nc2,
			Memi[ids], Memi[links], Memi[objs], nobjs, nalloc,
			z, s, t, z2, s2, t2, neighbors, 0, num)

		    if (nalloc == navail) {
			navail = max (100*nalloc*(nl+1)/line/100, nalloc+10000)
			call realloc (ids, navail, TY_INT)
			call realloc (links, navail, TY_INT)
			call realloc (objs, navail, TY_POINTER)
		    }

		    # Add to output masks.
		    if (num != numlast || c != clast) {
			orl[2,i] = clast - orl[1,i]
			i = i + 1

			numlast = num
			orl[1,i] = c
			orl[3,i] = numlast
		    }

		    if (dosrl) {
			rcnv = rcnv / tcnv / splitthresh
			if (rcnv > 1.)
			    rcnv = (4 * rcnv**0.25 - 3)
			bin = nint (rcnv * binscale)
			if (bin != binlast || c != clast) {
			    srl[2,j] = clast - srl[1,j]
			    j = j + 1

			    binlast = bin
			    srl[1,j] = c
			    srl[3,j] = binlast

			    siglevmax = max (bin, siglevmax)
			}
		    }
		    clast = c1
		}
	    } else if (rcnv < low) {
		if (ldetect) {
		    call dadd (c1, line, out[nc3], lastout[nc3], nc2,
			Memi[ids], Memi[links], Memi[objs], nobjs, nalloc,
			2*s-z, s, t, z1, s1, t1, neighbors, OBJ_DARK, num)

		    if (nalloc == navail) {
			navail = max (100*nalloc*(nl+1)/line/100, nalloc+10000)
			call realloc (ids, navail, TY_INT)
			call realloc (links, navail, TY_INT)
			call realloc (objs, navail, TY_POINTER)
		    }

		    # Add to output masks.
		    if (num != numlast || c != clast) {
			orl[2,i] = clast - orl[1,i]
			i = i + 1

			numlast = num
			orl[1,i] = c
			orl[3,i] = numlast
		    }
		    clast = c1
		}
	    }

	    if (line <= nlmax && c <= ncmax) {
		bin = a * (z - s) / t + b
		if (bin >= 1 && bin <= nbins) {
		    csky = (c-1) / n
		    bin = bins + csky * nbins + bin - 1
		    Memi[bin] = Memi[bin] + 1
		    Memr[skys+csky] = Memr[skys+csky] + s
		    Memr[sigs+csky] = Memr[sigs+csky] + t
		    Memi[nsky+csky] = Memi[nsky+csky] + 1
		    if (!IS_INDEFR(Memr[exps]))
			Memr[exps+csky] = Memr[exps+csky] + exp[c]
		}
	    }
	}

	# Finish up range lists.
	orl[2,i] = clast - orl[1,i]
	orl[1,1] = i
	orl[2,1] = nc
	if (dosrl) {
	    srl[2,j] = clast - srl[1,j]
	    srl[1,1] = j
	    srl[2,1] = nc
	}

	# Evaluate histogram sky values if all lines have been accumulated.
	if (line <= nlmax) {
	    if (mod (line, SKB_NLSPIX(skb)) == 0) {
		n = SKB_NCSBLK(skb)
		call skb_blkeval (Memi[bins], nbins, a, b, Memr[skys],
		    Memr[sigs], Memr[exps], Memi[nsky], n,
		    SKB_NSKYMIN(skb), SKB_NAV(skb), SKB_HISTWT(skb),
		    SKB_SIGFAC(skb))

		# Initialize for accumulation of next line of blocks.
		SKB_SKY(skb) = skys + n
		SKB_SIG(skb) = sigs + n
		if (!IS_INDEFR(Memr[exps]))
		    call aclrr (Memr[exps], n) 
		call aclri (Memi[nsky], n) 
		call aclri (Memi[bins], n*nbins) 
	    }
	}
end


# OBJADD -- Add a pixel to the object list and set the mask value.

procedure dadd (c, l, z, zlast, nc, ids, links, objs, nobjs, nalloc,
	data, sky, sigma, data2, sky2, sigma2, neighbors, flags, num)

int	c, l		#I Pixel coordinate 
int	z[nc]		#I Pixel values for current line
int	zlast[nc]	#I Pixel values for last line
int	nc		#I Number of pixels in a line
int	ids[ARB]	#I Mask ids
int	links[ARB]	#I Link to other mask ids with same number
pointer	objs[ARB]	#I Objects
int	nobjs		#U Number of objects
int	nalloc		#U Number of allocated objects
real	data		#I Data value (not sky subtracted)
real	sky		#I Sky value
real	sigma		#I Sky sigma value
real	data2		#I Data value (not sky subtracted)
real	sky2		#I Sky value
real	sigma2		#I Sky sigma value
int	neighbors	#I Neighbor type
int	flags		#I Flags
int	num		#O Object number assigned

int	i, num1, c1, c2
real	val
bool	merge
pointer	obj, obj1

begin
	# Inherit number of a neighboring pixel.
	num = INDEFI
	merge = false
	if (neighbors == 4) {
	    c1 = c - 1
	    c2 = c
	    if (z[c1] >= NUMSTART) {
		num = z[c1]
		merge = true
	    } else if (zlast[c] >= NUMSTART)
		num = ids[zlast[c]]
	} else {
	    c1 = c - 1
	    c2 = c + 1
	    if (z[c1] >= NUMSTART) {
		num = z[c1]
		merge = true
	    } else if (zlast[c1] >= NUMSTART)
		num = ids[zlast[c1]]
	    else if (zlast[c] >= NUMSTART)
		num = ids[zlast[c]]
	    else if (zlast[c2] >= NUMSTART)
		num = ids[zlast[c2]]
	}

	# If no number assign a new number.
	if (num == INDEFI) {
	    nobjs = nobjs + 1
	    num = nobjs
	    ids[num] = num
	    links[num] = 0
	    if (nalloc < nobjs) {
		call calloc (objs[num], OBJ_DETLEN, TY_STRUCT)
		nalloc = nobjs
	    }
	    obj = objs[num]
	    OBJ_XAP(obj) = 0.
	    OBJ_YAP(obj) = 0.
	    OBJ_FLUX(obj) = 0.
	    OBJ_NPIX(obj) = 0
	    OBJ_ISIGMAX(obj) = 0.
	    OBJ_ISIGAVG(obj) = 0.
	    OBJ_ISIGAVG2(obj) = 0.
	    OBJ_FLAGS(obj) = flags
	}
	obj = objs[num]

	# Merge overlapping objects from previous line. 
	if (merge) {
	    i = zlast[c2]
	    if (i >= NUMSTART && num != ids[i]) {
		num1 = ids[i]

		obj1 = objs[num1]
		OBJ_XAP(obj) = OBJ_XAP(obj) + OBJ_XAP(obj1)
		OBJ_YAP(obj) = OBJ_YAP(obj) + OBJ_YAP(obj1)
		OBJ_FLUX(obj) = OBJ_FLUX(obj) + OBJ_FLUX(obj1)
		OBJ_NPIX(obj) = OBJ_NPIX(obj) + OBJ_NPIX(obj1)
		OBJ_ISIGMAX(obj) = max (OBJ_ISIGMAX(obj), OBJ_ISIGMAX(obj1))
		OBJ_ISIGAVG(obj) = OBJ_ISIGAVG(obj) + OBJ_ISIGAVG(obj1)
		OBJ_ISIGAVG2(obj) = OBJ_ISIGAVG2(obj) + OBJ_ISIGAVG2(obj1)

		i = num
		while (links[i] != 0)
		    i = links[i]
		links[i] = num1 
		repeat {
		    i = links[i]
		    ids[i] = num
		} until (links[i] == 0)

		nalloc = nalloc + 1
		objs[nalloc] = obj1
		objs[num1] = NULL
	    }
	}

	z[c] = num
	OBJ_NPIX(obj) = OBJ_NPIX(obj) + 1
	val = (data - sky) / sigma
	OBJ_XAP(obj) = OBJ_XAP(obj) + val * c1
	OBJ_YAP(obj) = OBJ_YAP(obj) + val * l
	OBJ_FLUX(obj) = OBJ_FLUX(obj) + val
	OBJ_ISIGMAX(obj) = max (OBJ_ISIGMAX(obj), val)
	OBJ_ISIGAVG(obj) = OBJ_ISIGAVG(obj) + val
	#OBJ_ISIGAVG2(obj) = OBJ_ISIGAVG2(obj) + (data2 - sky2) / sigma2
	OBJ_ISIGAVG2(obj) = OBJ_ISIGAVG2(obj) + (data2 - sky2) / sigma
end


procedure drenum (det, out, ids, objs, nobjs)

pointer	det		#I Parameters
pointer	out		#I Output PMIO pointer
int	ids[nobjs]	#I Mask IDs
pointer	objs[nobjs]	#U Input and output object list
int	nobjs		#U Number of objects

int	minpix		# Minimum number of pixels
real	sigavg		# Cutoff of SIGAVG
real	sigmax		# Cutoff of SIGMAX
real	frac		# Fraction of sigavg2

int	i, j, n, nc, nl
real	rval
pointer	sp, v, rl, buf, obj

begin
	# Parameters.
	minpix = DET_MINPIX(det)
	sigavg = DET_SIGAVG(det)
	sigmax = DET_SIGPEAK(det)
	frac = DET_FRAC2(det)

	# Assign object numbers.  Eliminate objects, by setting object number
	# to zero, based on selection # critera (size, peak, etc.).

	j = NUMSTART - 1
	do i = NUMSTART, nobjs {
	    obj = objs[i]
	    if (obj == NULL)
		next

	    n = OBJ_NPIX(obj)
	    if (n < minpix) {
		OBJ_NUM(obj) = 0
		next
	    }
	    rval = sqrt (real(n))
	    OBJ_ISIGAVG(obj) = OBJ_ISIGAVG(obj) / rval
	    if ((OBJ_ISIGMAX(obj) < sigmax && OBJ_ISIGAVG(obj) < sigavg)) {
		OBJ_NUM(obj) = 0
		next
	    }
	    OBJ_ISIGAVG2(obj) = OBJ_ISIGAVG2(obj) / rval
	    if (OBJ_ISIGAVG(obj) < frac * OBJ_ISIGAVG2(obj)) {
		OBJ_NUM(obj) = 0
		next
	    }

	    rval = OBJ_FLUX(obj)
	    if (rval > 0.) {
		OBJ_XAP(obj) = OBJ_XAP(obj) / rval
		OBJ_YAP(obj) = OBJ_YAP(obj) / rval
	    } else {
		OBJ_XAP(obj) = INDEFR
		OBJ_YAP(obj) = INDEFR
	    }

	    j = j + 1
	    OBJ_NUM(obj) = j
	}

	# Set object mask.
	call smark (sp)
	call salloc (v, PM_MAXDIM, TY_LONG)
	call pm_gsize (out, i, Meml[v], j)
	nc = Meml[v]; nl = Meml[v+1]
	call salloc (rl, 3+3*nc, TY_INT)
	call salloc (buf, nc, TY_INT)
	call drenum1 (out, nc, nl, ids, objs, Meml[v], Memi[rl], Memi[buf])
	call sfree (sp)

	# Reorder the arrays and expand object structures.
	j = NUMSTART - 1
	do i = NUMSTART, nobjs {
	    obj = objs[i]
	    if (obj == NULL)
		next
	    if (OBJ_NUM(obj) == 0) {
		call mfree (objs[i], TY_STRUCT)
		next
	    }

	    call newobj (obj)

	    j = j + 1
	    objs[j] = obj
	}
	nobjs = j
end


procedure drenum1 (om, nc, nl, ids, objs, v, rl, buf)

pointer	om			#I Object mask pointer
int	nc, nl			#I Dimensions
int	ids[ARB]		#I Mask IDs
pointer	objs[ARB]		#I Objects
long	v[PM_MAXDIM]		#I Work array
int	rl[3,nc]		#I Work array
int	buf[nc]			#I Work array
	
int	i, j, l, id, andi(), ori()
pointer	obj

begin
	v[1] = 1
	do l = 1, nl {
	    v[2] = l
	    call pmglri (om, v, rl, 0, nc, 0) 
	    j = 1
	    do i = 2, rl[1,1] {
		id = rl[3,i]
		if (id >= NUMSTART) {
		    obj = objs[ids[id]]
		    id = OBJ_NUM(obj)
		    if (DARK(obj) && id > 0)
			id = MSETFLAG(id, MASK_DARK)
		}
		if (id > 0) {
		    j = j + 1
		    rl[1,j] = rl[1,i]
		    rl[2,j] = rl[2,i]
		    rl[3,j] = id
		}
	    }
	    rl[1,1] = j
	    call pmplri (om, v, rl, 0, nc, PIX_SRC) 
	}
end


procedure newobj (obj)

pointer	obj		#U Object structure

begin
	if (obj == NULL)
	    return

	call realloc (obj, OBJ_LEN, TY_STRUCT)
	OBJ_FLUX(obj) = INDEFR
	OBJ_SKY(obj) = INDEFR
	OBJ_SIG(obj) = INDEFR
	OBJ_PEAK(obj) = INDEFR
	OBJ_X1(obj) = INDEFR
	OBJ_Y1(obj) = INDEFR
	OBJ_WX(obj) = INDEFD
	OBJ_WY(obj) = INDEFD
	OBJ_XMIN(obj) = INDEFI
	OBJ_XMAX(obj) = INDEFI
	OBJ_YMIN(obj) = INDEFI
	OBJ_YMAX(obj) = INDEFI
end
