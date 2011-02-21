include	<fset.h>
include <imhdr.h>
include <error.h>
include <mach.h>

define	LEN_CP		32		# center structure pointer

# task parameters
define	SMALLBOX	Memi[($1)]
define	BIGBOX		Memi[($1)+1]
define	VERBOSE		Memi[($1)+2]
define	NEGATIVE	Memi[($1)+3]
define	BACKGROUND	Memr[P2R(($1)+4)]
define	LO_THRESH	Memr[P2R(($1)+5)]
define	HI_THRESH	Memr[P2R(($1)+6)]
define	MAX_TRIES	Memi[($1)+7]
define	TOL		Memi[($1)+8]
define	MAX_SHIFT	Memr[P2R(($1)+9)]

# other scalars
define	IM		Memi[($1)+10]
define	BOXSIZE		Memi[($1)+11]
define	BACK_LOCAL	Memr[P2R(($1)+12)]
define	LO_LOCAL	Memr[P2R(($1)+13)]
define	HI_LOCAL	Memr[P2R(($1)+14)]
define	NIMAGES		Memi[($1)+15]
define	NCOORDS		Memi[($1)+16]

# expensive, but the indexing isn't done excessively many times
define	OFF1D		(($1)-1)
define	OFF2D		((($2)-1)*NCOORDS($1)+(($3)-1))

# vectors and matrices
define	XINIT_PT	Memi[($1)+20]	# need space for NCOORDS of these
define	YINIT_PT	Memi[($1)+21]
define	XINIT		Memr[XINIT_PT($1)+OFF1D($2)]
define	YINIT		Memr[YINIT_PT($1)+OFF1D($2)]

define	XSHIFT_PT	Memi[($1)+22]	# space for NIMAGES of these
define	YSHIFT_PT	Memi[($1)+23]
define	XSHIFT		Memr[XSHIFT_PT($1)+OFF1D($2)]
define	YSHIFT		Memr[YSHIFT_PT($1)+OFF1D($2)]

define	XSIZE_PT	Memi[($1)+24]	# space for NIMAGES+1
define	YSIZE_PT	Memi[($1)+25]
define	XSIZE		Memr[XSIZE_PT($1)+OFF1D($2)]
define	YSIZE		Memr[YSIZE_PT($1)+OFF1D($2)]

define	XCENTER_PT	Memi[($1)+26]	# space for (NIMAGES+1)*NCOORDS
define	YCENTER_PT	Memi[($1)+27]
define	XCENTER		Memr[XCENTER_PT($1)+OFF2D($1,$2,$3)]
define	YCENTER		Memr[YCENTER_PT($1)+OFF2D($1,$2,$3)]

define	XSIGMA_PT	Memi[($1)+28]
define	YSIGMA_PT	Memi[($1)+29]
define	XSIGMA		Memr[XSIGMA_PT($1)+OFF2D($1,$2,$3)]
define	YSIGMA		Memr[YSIGMA_PT($1)+OFF2D($1,$2,$3)]

define	REJECTED_PT	Memi[($1)+30]
define	REJECTED	Memi[REJECTED_PT($1)+OFF2D($1,$2,$3)]


# list "template" structure, currently just read the file
define	LEN_LP		2

define	LP_FD		Memi[($1)]
define	LP_LEN		Memi[($1)+1]

# T_IMCENTROID -- Find the centroids of a list of sources in a list of
# images and compute the average shifts relative to a reference image.

procedure t_imcentroid()

pointer	imlist, coordlist, shiftlist
pointer	img, ref, refer, cp, im, sp
int	nimages, ncoords, nshifts, ncentered, i, j
real	x, y, junk
bool	error_seen, firsttime

pointer	imtopenp(), immap(), ia_openp2r(), ia_init()
int	imtlen(), imtgetim(), ia_len(), ia_center(), strmatch()

errchk	imtopenp, immap, imunmap
errchk	ia_init, ia_openp2r, ia_len, ia_close, ia_center

begin
	call smark (sp)
	call salloc (img, SZ_FNAME, TY_CHAR)
	call salloc (refer, SZ_FNAME, TY_CHAR)

	error_seen = false
	imlist = NULL
	coordlist = NULL
	shiftlist = NULL
	ref = NULL
	cp = NULL

	iferr {
	    # Flush on new line to avoid eprint output from appear
	    # in the middle of regular output.
	    call fseti (STDOUT, F_FLUSHNL, YES)

	    # Open the input image list.
	    imlist = imtopenp ("input")
	    nimages = imtlen (imlist)
	    if (nimages <= 0)
		call error (1, "No images specified")

	    # Get the reference image and check name for whitespace.
	    call clgstr ("reference", Memc[refer], SZ_FNAME)
	    if (Memc[refer] != EOS && strmatch (Memc[refer], "^#$") == 0)
		iferr (ref = immap (Memc[refer], READ_ONLY, 0)) {
		    ref = NULL
		    call error (1, "Reference not found")
		}

	    # Open the coordinate list.
	    coordlist = ia_openp2r ("coords")
	    ncoords = ia_len (coordlist)
	    if (ncoords <= 0)
		call error (1, "No coordinates found")

	    # Open the shifts file.
	    shiftlist = ia_openp2r ("shifts")
	    nshifts = ia_len (shiftlist)
	    if (nshifts <= 0)
		call ia_close (shiftlist)
	    else if (nshifts != nimages)
		call error (1, "Number of shifts doesn't match images")

	    # Initialize the centering structure.
	    cp = ia_init (shiftlist, nimages, coordlist, ncoords)

	    if (ref == NULL)
		VERBOSE(cp) = YES

	    if (VERBOSE(cp) == YES) {
		call printf ("#Coords%16tImage     X-center   Err")
	        call printf ("      Y-center   Err     Num\n")
		call flush (STDOUT)
	    }

	    # Loop over all the images
	    ncentered = 0
	    for (i=1; imtgetim (imlist, Memc[img], SZ_FNAME) != EOF; i=i+1) {
		im = immap (Memc[img], READ_ONLY, 0)
		IM(cp) = im

		if (IM_NDIM(im) != 2) {
		    call eprintf ("%s:  ")
			call pargstr (Memc[img])
		    call error (1, "Image is not 2 dimensional")
		}

		XSIZE(cp,i) = real (IM_LEN(im,1))
		YSIZE(cp,i) = real (IM_LEN(im,2))

		if (nshifts == 0) {
		    BOXSIZE(cp) = BIGBOX(cp)
		    if (ia_center (cp, XINIT(cp,1), YINIT(cp,1), x, y,
			junk, junk) == ERR)
			call error (1, "Problem with coarse centering")
		    XSHIFT(cp,i) = XINIT(cp,1) - x
		    YSHIFT(cp,i) = YINIT(cp,1) - y
		}

		firsttime = true
		do j = 1, ncoords {
		    x = XINIT(cp,j) - XSHIFT(cp,i)
		    y = YINIT(cp,j) - YSHIFT(cp,i)

		    if (x < 1 || x > XSIZE(cp,i) || y < 1 || y > YSIZE(cp,i)) {
			REJECTED(cp,i,j) = YES
			next
		    }

		    BOXSIZE(cp) = SMALLBOX(cp)
		    if (ia_center (cp, x, y, XCENTER(cp,i,j), YCENTER(cp,i,j),
			XSIGMA(cp,i,j), YSIGMA(cp,i,j)) == ERR) {
			REJECTED(cp,i,j) = YES
			next
		    }

		    if (abs (XCENTER(cp,i,j) - x) > MAX_SHIFT(cp)) {
			REJECTED(cp,i,j) = YES
			next
		    }
		    if (abs (YCENTER(cp,i,j) - y) > MAX_SHIFT(cp)) {
			REJECTED(cp,i,j) = YES
			next
		    }

		    if (firsttime)
			firsttime = false

		    if (VERBOSE(cp) == YES) {
			call printf (
			    "%20s   %9.3f (%.3f)   %9.3f (%.3f)  %4d\n")
			    call pargstr (Memc[img])
			    call pargr (XCENTER(cp,i,j))
			    call pargr (XSIGMA(cp,i,j))
			    call pargr (YCENTER(cp,i,j))
			    call pargr (YSIGMA(cp,i,j))
			    call pargi (j)
		    }
		}

		if (firsttime) {
		    call eprintf ("Warning: no sources centered in %s\n")
			call pargstr (Memc[img])
		    call flush (STDERR)
		} else
		    ncentered = ncentered + 1

		if (VERBOSE(cp) == YES) {
		    call printf ("\n")
		    call flush (STDOUT)
		}

		call imunmap (im)
	    }

	    # Measure the reference coordinates if any.
	    if (ref != NULL) {
		IM(cp) = ref

		if (IM_NDIM(ref) != 2) {
		    call eprintf ("%s:  ")
			call pargstr (Memc[refer])
		    call error (1, "Reference image is not 2 dimensional")
		}

		XSIZE(cp,nimages+1) = real (IM_LEN(ref,1))
		YSIZE(cp,nimages+1) = real (IM_LEN(ref,2))

		firsttime = true
		do j = 1, ncoords {
		    x = XINIT(cp,j)
		    y = YINIT(cp,j)

		    if (x < 1 || x > XSIZE(cp,nimages+1) ||
			y < 1 || y > YSIZE(cp,nimages+1)) {
			REJECTED(cp,nimages+1,j) = YES
			next
		    }

		    BOXSIZE(cp) = SMALLBOX(cp)
		    if (ia_center (cp, x, y, XCENTER(cp,nimages+1,j),
			YCENTER(cp,nimages+1,j), XSIGMA(cp,nimages+1,j),
			YSIGMA(cp,nimages+1,j)) == ERR) {
			REJECTED(cp,nimages+1,j) = YES
			next
		    }

		    if (abs (XCENTER(cp,nimages+1,j) - x) > MAX_SHIFT(cp)) {
			REJECTED(cp,nimages+1,j) = YES
			next
		    }
		    if (abs (YCENTER(cp,nimages+1,j) - y ) > MAX_SHIFT(cp)) {
			REJECTED(cp,nimages+1,j) = YES
			next
		    }

		    if (firsttime) {
			if (VERBOSE(cp) == YES) {
			    call printf (
			        "#Refcoords%12tReference     X-center   Err")
			    call printf ("      Y-center   Err     Num\n")
			}
			firsttime = false
		    }

		    if (VERBOSE(cp) == YES) {
			call printf (
			    "%20s   %9.3f (%0.3f)   %9.3f (%.3f)  %4d\n")
			    call pargstr (Memc[refer])
			    call pargr (XCENTER(cp,nimages+1,j))
			    call pargr (XSIGMA(cp,nimages+1,j))
			    call pargr (YCENTER(cp,nimages+1,j))
			    call pargr (YSIGMA(cp,nimages+1,j))
			    call pargi (j)
		    }
		}

		if (firsttime) {
		    call eprintf ("Warning: no sources centered in reference\n")
		    call flush (STDERR)

		} else {
		    if (VERBOSE(cp) == YES) {
			call printf ("\n")
			call flush (STDOUT)
		    }

		    call imtrew (imlist)
		    call ia_stats (cp, imlist)

		    if (ncentered > 1)
			call ia_trim (cp)
		}
	    }

	} then
	    error_seen = true

	call ia_free (cp)

	if (shiftlist != NULL)
	    call ia_close (shiftlist)
	if (ref != NULL)
	    call imunmap (ref)
	if (coordlist != NULL)
	    call ia_close (coordlist)
	if (imlist != NULL)
	    call imtclose (imlist)

	call sfree (sp)

	if (error_seen)
	    call erract (EA_WARN)
end


# IA_INIT -- Initialize the centering structure.

pointer procedure ia_init (shiftlist, nshifts, coordlist, ncoords)

pointer	shiftlist		#I shift "template" pointer
int	nshifts			#I number of shifts in list (or # images)
pointer	coordlist		#I coordinate "template" pointer
int	ncoords			#I number of coordinates in list

pointer	cp
int	boxsize, i
real	x, y

int	clgeti(), btoi(), ia_get2r()
real	clgetr()
bool	clgetb()

errchk	ia_get2r

begin
	call calloc (cp, LEN_CP, TY_STRUCT)

	boxsize = clgeti ("boxsize")
	if (mod (boxsize, 2) == 0) {
	    boxsize = boxsize + 1
	    call eprintf ("Warning: boxsize must be odd, using %d\n")
		call pargi (boxsize)
	}
	SMALLBOX(cp) = (boxsize - 1) / 2

	if (shiftlist == NULL) {
	    boxsize = clgeti ("bigbox")
	    if (mod (boxsize, 2) == 0) {
		boxsize = boxsize + 1
		call eprintf ("Warning: bigbox must be odd, using %d\n")
		    call pargi (boxsize)
	    }
	    BIGBOX(cp) = (boxsize - 1) / 2
	}

	NEGATIVE(cp)	= btoi (clgetb ("negative"))
	BACKGROUND(cp)	= clgetr ("background")

	x = clgetr ("lower")
	y = clgetr ("upper")

	if (IS_INDEFR(x) || IS_INDEFR(y)) {
	    LO_THRESH(cp) = x
	    HI_THRESH(cp) = y
	} else {
	    LO_THRESH(cp) = min (x, y)
	    HI_THRESH(cp) = max (x, y)
	}

	MAX_TRIES(cp)	= max (clgeti ("niterate"), 2)
	TOL(cp)		= abs (clgeti ("tolerance"))
	MAX_SHIFT(cp)   = clgetr ("maxshift")
	if (IS_INDEFR(MAX_SHIFT(cp)))
	    MAX_SHIFT(cp) = MAX_REAL
	else
	    MAX_SHIFT(cp) = abs (MAX_SHIFT(cp))
	VERBOSE(cp)	= btoi (clgetb ("verbose"))

	IM(cp)		= NULL

	NIMAGES(cp)	= nshifts
	NCOORDS(cp)	= ncoords

	call malloc (XINIT_PT(cp), ncoords, TY_REAL)
	call malloc (YINIT_PT(cp), ncoords, TY_REAL)
	call malloc (XSHIFT_PT(cp), nshifts, TY_REAL)
	call malloc (YSHIFT_PT(cp), nshifts, TY_REAL)
	call malloc (XSIZE_PT(cp), nshifts+1, TY_REAL)
	call malloc (YSIZE_PT(cp), nshifts+1, TY_REAL)
	call malloc (XCENTER_PT(cp), (nshifts+1)*ncoords, TY_REAL)
	call malloc (YCENTER_PT(cp), (nshifts+1)*ncoords, TY_REAL)
	call malloc (XSIGMA_PT(cp), (nshifts+1)*ncoords, TY_REAL)
	call malloc (YSIGMA_PT(cp), (nshifts+1)*ncoords, TY_REAL)
	call calloc (REJECTED_PT(cp), (nshifts+1)*ncoords, TY_INT)

	for (i=1; ia_get2r (coordlist, x, y) != EOF; i=i+1) {
	    if (i > ncoords)
		call error (1, "problem reading coordinate file")
	    XINIT(cp,i) = x
	    YINIT(cp,i) = y
	}

	for (i=1; ia_get2r (shiftlist, x, y) != EOF; i=i+1) {
	    if (i > nshifts)
		call error (1, "problem reading shifts file")
	    XSHIFT(cp,i) = x
	    YSHIFT(cp,i) = y
	}

	return (cp)
end


# IA_FREE -- Free the structure pointer.

procedure ia_free (cp)

pointer	cp			#O center structure pointer

begin
	if (cp == NULL)
	    return

	if (REJECTED_PT(cp) != NULL)
	    call mfree (REJECTED_PT(cp), TY_INT)
	if (XSIGMA_PT(cp) != NULL)
	    call mfree (XSIGMA_PT(cp), TY_REAL)
	if (YSIGMA_PT(cp) != NULL)
	    call mfree (YSIGMA_PT(cp), TY_REAL)
	if (XCENTER_PT(cp) != NULL)
	    call mfree (XCENTER_PT(cp), TY_REAL)
	if (YCENTER_PT(cp) != NULL)
	    call mfree (YCENTER_PT(cp), TY_REAL)
	if (XSIZE_PT(cp) != NULL)
	    call mfree (XSIZE_PT(cp), TY_REAL)
	if (YSIZE_PT(cp) != NULL)
	    call mfree (YSIZE_PT(cp), TY_REAL)
	if (XSHIFT_PT(cp) != NULL)
	    call mfree (XSHIFT_PT(cp), TY_REAL)
	if (YSHIFT_PT(cp) != NULL)
	    call mfree (YSHIFT_PT(cp), TY_REAL)
	if (XINIT_PT(cp) != NULL)
	    call mfree (XINIT_PT(cp), TY_REAL)
	if (YINIT_PT(cp) != NULL)
	    call mfree (YINIT_PT(cp), TY_REAL)

	call mfree (cp, TY_STRUCT)
	cp = NULL				# just in case...
end


# IA_CENTER -- Compute star center using MPC algorithm.

int procedure ia_center (cp, xinit, yinit, xcenter, ycenter, xsigma, ysigma)

pointer	cp			#I center structure pointer
real	xinit, yinit		#I initial x and y coordinates
real	xcenter, ycenter	#O centered x and y coordinates
real	xsigma, ysigma		#O centering errors

int	x1, x2, y1, y2, nx, ny, try
pointer	im, buf, xbuf, ybuf, sp
real	xold, yold, xnew, ynew
bool	converged

pointer	imgs2r()
real	ia_ctr1d()

errchk	imgs2r, ia_threshold, ia_rowsum, ia_colsum, ia_ctr1d

begin
	im = IM(cp)
	xold  = xinit
	yold  = yinit
	converged = false

	do try = 1, MAX_TRIES(cp) {
	    x1 = max (nint(xold) - BOXSIZE(cp), 1)
	    x2 = min (nint(xold) + BOXSIZE(cp), IM_LEN(im,1))
	    y1 = max (nint(yold) - BOXSIZE(cp), 1)
	    y2 = min (nint(yold) + BOXSIZE(cp), IM_LEN(im,2))

	    nx = x2 - x1 + 1
	    ny = y2 - y1 + 1

	    # inside the loop in case we're near an edge
	    call smark (sp)
	    call salloc (xbuf, nx, TY_REAL)
	    call salloc (ybuf, ny, TY_REAL)

	    iferr {
		buf = imgs2r (im, x1, x2, y1, y2)

		call ia_threshold (cp, Memr[buf], nx*ny)
		call ia_rowsum (cp, Memr[buf], Memr[xbuf], nx, ny)
		call ia_colsum (cp, Memr[buf], Memr[ybuf], nx, ny)

		xnew = x1 + ia_ctr1d (Memr[xbuf], nx, xsigma)
		ynew = y1 + ia_ctr1d (Memr[ybuf], ny, ysigma)
	    } then {
		call sfree (sp)
		call erract (EA_WARN)
		return (ERR)
	    }

	    call sfree (sp)

	    if (abs (nint(xnew) - nint(xold)) <= TOL(cp) &&
		abs (nint(ynew) - nint(yold)) <= TOL(cp)) {

		converged = true
		break
	    }

	    xold = xnew
	    yold = ynew
	}

	if (converged) {
	    xcenter = xnew
	    ycenter = ynew
	    return (OK)
	} else {
	    call eprintf ("Warning: failed to converge near (%d,%d)\n")
		call pargi (nint (xinit))
		call pargi (nint (yinit))
	    call flush (STDERR)
	    return (ERR)
	}
end


# IA_THRESHOLD -- Find the low and high thresholds for the subraster.

procedure ia_threshold (cp, raster, npix)

pointer	cp			#I center structure pointer
real	raster[ARB]		#I 2-D subraster
int	npix			#I size of the (apparently) 1-D subraster

real	lo, hi, junk

int	awvgr()

errchk	alimr, awvgr

begin
	# use the local data min or max for thresholds that are INDEF.
	if (IS_INDEFR(LO_THRESH(cp)) || IS_INDEFR(HI_THRESH(cp)))
	    call alimr (raster, npix, lo, hi)
	if (! IS_INDEFR(LO_THRESH(cp)))
	    lo = LO_THRESH(cp)
	if (! IS_INDEFR(HI_THRESH(cp)))
	    hi = HI_THRESH(cp)

	if (IS_INDEFR(BACKGROUND(cp))) {
	    if (awvgr (raster, npix, BACK_LOCAL(cp), junk, lo, hi) <= 0)
		call error (1, "no pixels between thresholds")
	} else
	    BACK_LOCAL(cp) = BACKGROUND(cp)

	if (NEGATIVE(cp) == YES) {
	    LO_LOCAL(cp) = lo
	    HI_LOCAL(cp) = min (hi, BACK_LOCAL(cp))
	} else {
	    LO_LOCAL(cp) = max (lo, BACK_LOCAL(cp))
	    HI_LOCAL(cp) = hi
	}
end


# IA_ROWSUM -- Sum all rows in a raster, subject to the thresholds, the
# background, and other parameters.

procedure ia_rowsum (cp, raster, row, nx, ny)

pointer	cp			#I center structure pointer
real	raster[nx,ny]		#I 2-D subraster
real	row[ARB]		#O 1-D squashed row vector
int	nx, ny			#I dimensions of the subraster

int	i, j
real	lo, hi, back, pix

begin
	call aclrr (row, nx)

	back = BACK_LOCAL(cp)
	lo   = LO_LOCAL(cp)
	hi   = HI_LOCAL(cp)

	do j = 1, ny
	    do i = 1, nx {
		pix = raster[i,j]
		if (lo <= pix && pix <= hi)
		    row[i] = row[i] + pix - back
	    }

	if (NEGATIVE(cp) == YES)
	    call adivkr (row, -real(ny), row, nx)
	else
	    call adivkr (row, real(ny), row, nx)

	# recycle lo (and hi)
	call alimr (row, nx, lo, hi)
	if (lo < 0.)
	    call error (1, "Negative value in marginal row\n")
end


# IA_COLSUM -- Sum all columns in a raster, subject to the thresholds, the
# background, and other parameters.

procedure ia_colsum (cp, raster, col, nx, ny)

pointer	cp			#I center structure pointer
real	raster[nx,ny]		#I 2-D subraster
real	col[ARB]		#O 1-D squashed col vector
int	nx, ny			#I dimensions of the subraster

int	i, j
real	lo, hi, back, pix

begin
	call aclrr (col, ny)

	back = BACK_LOCAL(cp)
	lo   = LO_LOCAL(cp)
	hi   = HI_LOCAL(cp)

	do j = 1, ny
	    do i = 1, nx {
		pix = raster[i,j]
		if (lo <= pix && pix <= hi)
		    col[j] = col[j] + pix - back
	    }

	if (NEGATIVE(cp) == YES)
	    call adivkr (col, -real(nx), col, ny)
	else
	    call adivkr (col, real(nx), col, ny)

	# recycle lo (and hi)
	call alimr (col, ny, lo, hi)
	if (lo < 0.)
	    call error (1, "Negative value in marginal column\n")
end


# IA_CNTR1D -- Compute the the first moment.

real procedure ia_ctr1d (a, npix, err)

real	a[ARB]			#I marginal vector
int	npix			#I size of the vector
real	err			#O error in the centroid

real	centroid, pix, sumi, sumix, sumix2
int	i

bool	fp_equalr()

begin
	sumi = 0.
	sumix = 0.
	sumix2 = 0.

	do i = 1, npix {
	    pix = a[i]
	    sumi = sumi + pix
	    sumix = sumix + pix * (i-1)
	    sumix2 = sumix2 + pix * (i-1) ** 2
	}

	if (fp_equalr (sumi, 0.))
	    call error (1, "zero marginal vector")

	else {
	    centroid = sumix / sumi
	    err = sumix2 / sumi - centroid ** 2
	    if (err > 0.)
		err = sqrt (err / sumi)
	    else
		err = 0.
	}

	return (centroid)
end


# IA_OPENP2R -- Open a list file from which two real values per line
# are expected.

pointer procedure ia_openp2r (param)

char	param[ARB]	#I parameter name

int	fd, length
pointer	lp, fname, sp
real	x1, x2

int	open(), fscan(), nscan(), strmatch()

errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	call clgstr (param, Memc[fname], SZ_FNAME)

	# Whitespace in the name ?
	if (strmatch (Memc[fname], "^#$") != 0) {
	    call sfree (sp)
	    return (NULL)
	}

	# This should be replaced by some template mechanism.
	ifnoerr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
	    length = 0
	    while (fscan (fd) != EOF) {
		call gargr (x1)
		call gargr (x2)

		switch (nscan()) {
		case 2:
		    length = length + 1
		case 1:
		    call error (1, "Reading file, only one value on line")
		default:
		    # read another line
		}
	    }
	    call seek (fd, BOF)
	} else {
	    fd = NULL
	    length = 0
	}

	call sfree (sp)

	call malloc (lp, LEN_LP, TY_STRUCT)
	LP_FD(lp) = fd
	LP_LEN(lp) = length

	return (lp)
end


# IA_LEN -- Return the length of a list file, given its descriptor.

int procedure ia_len (lp)

pointer	lp		#I list file descriptor

begin
	if (lp == NULL)
	    return (0)
	else
	    return (LP_LEN(lp))
end


# IA_GET2R -- Get two real numbers from the next line of the list file.

int procedure ia_get2r (lp, x1, x2)

pointer	lp		#I list file descriptor
real	x1, x2		#O values to read

int	fscan(), nscan()

begin
	if (lp == NULL) {
	    x1 = INDEFR
	    x2 = INDEFR
	    return (EOF)
	}

	while (fscan (LP_FD(lp)) != EOF) {
	    call gargr (x1)
	    call gargr (x2)

	    switch (nscan()) {
	    case 2:
		return (2)
	    case 1:
		call error (1, "only one value on line")
	    default:
		# read another line
	    }
	}

	x1 = INDEFR
	x2 = INDEFR
	return (EOF)
end


# IA_CLOSE -- Close a list file descriptor.

procedure ia_close (lp)

pointer	lp		#I list file descriptor

errchk	close

begin
	if (lp == NULL)
	    return

	if (LP_FD(lp) != NULL)
	    call close (LP_FD(lp))

	call mfree (lp, TY_STRUCT)
	lp = NULL			# just in case...
end


# IA_STATS -- Compute the x and y shifts.

procedure ia_stats (cp, imlist)

pointer	cp			#I center structure pointer
pointer	imlist			#I image template (for labeling)

real	xshift, yshift, xsum, ysum
real	xsum2, ysum2, xsig2, ysig2
real	xvar, yvar, xerr, yerr, xprop, yprop
int	nim, ncoo, nsources, i, j
pointer	img, sp
bool	firsttime

int	imtgetim()

begin
	call smark (sp)
	call salloc (img, SZ_FNAME, TY_CHAR)

	nim = NIMAGES(cp)
	ncoo = NCOORDS(cp)

	firsttime = true
	for (i=1; imtgetim (imlist, Memc[img], SZ_FNAME) != EOF; i=i+1) {
	    xsum = 0.
	    ysum = 0.
	    xsum2 = 0.
	    ysum2 = 0.
	    xsig2 = 0.
	    ysig2 = 0.
	    nsources = 0

	    do j = 1, ncoo {
		if (REJECTED(cp,i,j) == YES || REJECTED(cp,nim+1,j) == YES)
		    next

		xshift = XCENTER(cp,nim+1,j) - XCENTER(cp,i,j)
		yshift = YCENTER(cp,nim+1,j) - YCENTER(cp,i,j)

		xsum = xsum + xshift
		ysum = ysum + yshift

		# internal errors
		xsum2 = xsum2 + xshift*xshift
		ysum2 = ysum2 + yshift*yshift

		xsig2 = xsig2 + XSIGMA(cp,nim+1,j)**2 + XSIGMA(cp,i,j)**2
		ysig2 = ysig2 + YSIGMA(cp,nim+1,j)**2 + YSIGMA(cp,i,j)**2

		nsources = nsources + 1
	    }

	    if (nsources == 0) {
		XSHIFT(cp,i) = INDEFR
		YSHIFT(cp,i) = INDEFR
		next
	    }

	    XSHIFT(cp,i) = xsum / nsources
	    YSHIFT(cp,i) = ysum / nsources

	    if (nsources > 1) {
		xvar = (nsources*xsum2 - xsum*xsum) / (nsources * (nsources-1))
		yvar = (nsources*ysum2 - ysum*ysum) / (nsources * (nsources-1))
		xerr = sqrt (max (xvar/nsources, 0.))
		yerr = sqrt (max (yvar/nsources, 0.))
	    } else {
		xerr = INDEFR
		yerr = INDEFR
	    }

	    xprop = sqrt (max (xsig2, 0.)) / nsources
	    yprop = sqrt (max (ysig2, 0.)) / nsources

	    if (firsttime) {
		call printf ("#Shifts%16tImage    X-shift   Err      ")
		call printf ("Y-shift   Err      N      Internal\n")
		firsttime = false
	    }

	    call printf (
		"%20s   %8.3f (%.3f)   %8.3f (%.3f) %4d   (%.3f,%.3f)\n")
		call pargstr (Memc[img])
		call pargr (XSHIFT(cp,i))
		call pargr (xprop)
		call pargr (YSHIFT(cp,i))
		call pargr (yprop)
		call pargi (nsources)
		call pargr (xerr)
		call pargr (yerr)
	}

	call flush (STDOUT)
	call sfree (sp)
end


# IA_TRIM -- Compute the trim section.

procedure ia_trim (cp)

pointer	cp			#I center structure pointer

real	xlo, xhi, ylo, yhi, xmin, ymin
int	ixlo, ixhi, iylo, iyhi, ixlonew, ixhinew, iylonew, iyhinew, i
int	vxlo, vxhi, vylo, vyhi		# vignetted versions
bool	firsttime

begin
	firsttime = true
	do i = 1, NIMAGES(cp) {

	    if (IS_INDEFR(XSHIFT(cp,i)) || IS_INDEFR(YSHIFT(cp,i)))
		next

	    # Compute limits.
	    xlo = 1. + XSHIFT(cp,i)
	    ylo = 1. + YSHIFT(cp,i)
	    xhi = XSIZE(cp,i) + XSHIFT(cp,i)
	    yhi = YSIZE(cp,i) + YSHIFT(cp,i)

	    ixlonew = int (xlo)
	    if (xlo > ixlonew)			# round up
		ixlonew = ixlonew + 1

	    ixhinew = int (xhi)
	    if (xhi < ixhinew)			# round down
		ixhinew = ixhinew - 1

	    iylonew = int (ylo)			# round up
	    if (ylo > iylonew)
		iylonew = iylonew + 1

	    iyhinew = int (yhi)			# round down
	    if (yhi < iyhinew)
		iyhinew = iyhinew - 1

	    if (firsttime) {
		ixlo = ixlonew
		ixhi = ixhinew
		iylo = iylonew
		iyhi = iyhinew

		xmin = XSIZE(cp,i)
		ymin = YSIZE(cp,i)

		firsttime = false
	    } else {
		ixlo = max (ixlo, ixlonew)
		ixhi = min (ixhi, ixhinew)
		iylo = max (iylo, iylonew)
		iyhi = min (iyhi, iyhinew)

		xmin = min (XSIZE(cp,i), xmin)
		ymin = min (YSIZE(cp,i), ymin)
	    }
	}

	# Don't bother to complain.
	if (firsttime)
	    return

	call printf ("\n")

	# Vignetting is possible downstream since imshift and other tasks
	# preserve the size of the input image.

	vxlo = max (1, min (ixlo, int(xmin)))
	vxhi = max (1, min (ixhi, int(xmin)))
	vylo = max (1, min (iylo, int(ymin)))
	vyhi = max (1, min (iyhi, int(ymin)))
	if (vxlo != ixlo || vxhi != ixhi || vylo != iylo || vyhi != iyhi) {
	    call eprintf ("#Vignette_Section = [%d:%d,%d:%d]\n")
		call pargi (vxlo)
		call pargi (vxhi)
		call pargi (vylo)
		call pargi (vyhi)
	}

	# Output the trim section.
	call printf ("#Trim_Section = [%d:%d,%d:%d]\n")
	    call pargi (ixlo)
	    call pargi (ixhi)
	    call pargi (iylo)
	    call pargi (iyhi)

	call flush (STDOUT)
end
