include	<error.h>
include	<imhdr.h>
include	<mach.h>

define	LEN_UA		20000			# Maximum user header
define	LEN_COMMENT	70			# Maximum comment length

# Cosmic ray data structure
define	LEN_MKO		4
define	MKO_X		Memi[$1]		# X position
define	MKO_Y		Memi[$1+1]		# Y position
define	MKO_Z		Memi[$1+2]		# Flux
define	MKO_SORT	Memi[$1+3]		# Sort index


# T_MKNOISE -- Add cosmic rays and possion and readout noise to images.
# New images may be created or noise added to existing images.
# The noise is not completely random for reasons of speed.

procedure t_mknoise ()

int	ilist				# Input image list
int	olist				# Output image list
int	objects				# List of cosmic ray files
int	nl				# Number of lines
int	nc				# Number of columns
real	background			# Background level
real	gain				# Gain (electrons/DN)
int	ranbuf				# Random number buffer size
real	rdnoise				# Read noise (in electrons)
bool	poisson				# Add Poisson noise?
long	seed				# Random number seed
int	nobjects			# Number of random cosmic rays
real	energy				# Maximum random energy (electrons)
bool	cmmts				# Add comments?

bool	new, fcmmts
long	seed1
real	x, y, z, dmin, dmax
int	i, j, k, l, nx, ny, nlines, c1, c2, c3, c4, l1, l2, l3, l4, irbuf, ipbuf
pointer	sp, input, output, fname, comment, rbuf, pbuf
pointer	in, out, buf, obuf, lines, newlines, obj, ptr1, ptr2
pointer	mko, mkt

long	clgetl(), clktime()
bool	clgetb(), streq()
int	imtopenp(), imtlen(), imtgetim()
int	clgeti(), access(), nowhite(), open(), fscan(), nscan()
real	clgetr(), urand()
pointer	immap(), imgl2r(), impl2r()
pointer	mkt_star()
errchk	open, immap, imgl2r, impl2r, malloc, realloc, mkt_gstar

int	mko_compare()
extern	mko_compare
pointer	mko_sort
common	/mko_qsort/ mko_sort

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (comment, max (SZ_FNAME,LEN_COMMENT), TY_CHAR)

	# Get parameters which apply to all images.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	objects = imtopenp ("cosrays")
	background = clgetr ("background")
	gain = clgetr ("gain")
	ranbuf = clgeti ("ranbuf")
	if (ranbuf == 0)
	    ranbuf = -1
	rdnoise = clgetr ("rdnoise") / gain
	if (rdnoise > 0. && ranbuf > 0)
	    call salloc (rbuf, ranbuf, TY_REAL)
	poisson = clgetb ("poisson")
	if (poisson && ranbuf > 0)
	    call salloc (pbuf, ranbuf, TY_REAL)
	seed = clgetl ("seed")
	if (IS_INDEFL(seed))
	    seed1 = seed1 + clktime (long (0))
	else
	    seed1 = seed
	cmmts = clgetb ("comments")

	if (imtlen (ilist) == 0)
	    call error (1, "No input image list")

	# Loop through input, output, and cosmic ray lists.
	# Missing output images take the input image name.
	# The cosmic ray list will repeat if shorter than input list.

	Memc[fname] = EOS
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    i = imtgetim (objects, Memc[fname], SZ_FNAME)

	    # Map images.  Check for new, existing, and in-place images.
	    if (streq (Memc[input], Memc[output])) {
		ifnoerr (in = immap (Memc[input], READ_WRITE, LEN_UA)) {
		    out = in
		    new = false
		} else {
		    iferr (out = immap (Memc[output], NEW_IMAGE, LEN_UA)) {
			call erract (EA_WARN)
			next
		    }
		    in = out

		    call clgstr ("header", Memc[comment], SZ_FNAME)
		    iferr (call mkh_header (out, Memc[comment], true, false))
			call erract (EA_WARN)

		    IM_NDIM(in) = 2
		    IM_LEN(in,1) = clgeti ("ncols")
		    IM_LEN(in,2) = clgeti ("nlines")
		    IM_PIXTYPE(in) = TY_REAL
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)
		    new = true
		}
	    } else {
		iferr (in = immap (Memc[input], READ_ONLY, LEN_UA)) {
		    call erract (EA_WARN)
		    next
		}
		iferr (out = immap (Memc[output], NEW_COPY, in)) {
		    call erract (EA_WARN)
		    call imunmap (in)
		    next
		}
		new = false
	    }
	    nc = IM_LEN(in,1)
	    nl = IM_LEN(in,2)
	    IM_MIN(out) = MAX_REAL
	    IM_MAX(out) = -MAX_REAL

	    call imaddr (out, "gain", gain)
	    call imaddr (out, "rdnoise", rdnoise * gain)

	    # Read the object list.
	    call malloc (mko, LEN_MKO, TY_STRUCT)
	    call mkt_init ()

	    # Set cosmic ray templates.
	    mkt = mkt_star ("gaussian")

	    # Read the object list.  If none or a nonexistent list is given
	    # and a number of random events is specified then generate them.
	    # If a nonexistent object list is given then write the random
	    # events out.

	    fcmmts = false
	    energy = INDEF
	    nobjects = 0
	    i = nowhite (Memc[fname], Memc[fname], SZ_FNAME)
	    if (access (Memc[fname], 0, 0) == YES) {
		i = open (Memc[fname], READ_ONLY, TEXT_FILE)
		while (fscan (i) != EOF) {
		    call gargr (x)
		    call gargr (y)
		    call gargr (z)
		    if (nscan() < 3) {
			fcmmts = true
			next
		    }
		    if (x < 1 || x > nc || y < 1 || y > nl)
			next
		    if (nobjects == 0) {
			j = 100
			call malloc (MKO_X(mko), j, TY_REAL)
			call malloc (MKO_Y(mko), j, TY_REAL)
			call malloc (MKO_Z(mko), j, TY_REAL)
			call malloc (MKO_SORT(mko), j, TY_INT)
		    } else if (nobjects == j) {
			j = j + 100
			call realloc (MKO_X(mko), j, TY_REAL)
			call realloc (MKO_Y(mko), j, TY_REAL)
			call realloc (MKO_Z(mko), j, TY_REAL)
			call realloc (MKO_SORT(mko), j, TY_INT)
		    }

		    Memr[MKO_X(mko)+nobjects] = x
		    Memr[MKO_Y(mko)+nobjects] = y
		    Memr[MKO_Z(mko)+nobjects] = z / gain
		    Memi[MKO_SORT(mko)+nobjects] = nobjects
		    nobjects = nobjects + 1
		}
		call close (i)
	    } else {
		nobjects = clgeti ("ncosrays")
		if (nobjects > 0) {
		    energy = clgetr ("energy") / gain
		    call malloc (MKO_X(mko), nobjects, TY_REAL)
		    call malloc (MKO_Y(mko), nobjects, TY_REAL)
		    call malloc (MKO_Z(mko), nobjects, TY_REAL)
		    call malloc (MKO_SORT(mko), nobjects, TY_INT)
		    do i = 0, nobjects-1 {
			Memr[MKO_X(mko)+i] = 1 + (nc-1) * urand (seed1)
			Memr[MKO_Y(mko)+i] = 1 + (nl-1) * urand (seed1)
			Memr[MKO_Z(mko)+i] = energy * urand (seed1)
			Memi[MKO_SORT(mko)+i] = i
		    }
		    if (Memc[fname] != EOS) {
			i = open (Memc[fname], NEW_FILE, TEXT_FILE)
			do j = 0, nobjects-1 {
			    call fprintf (i, "%g %g %g\n")
				call pargr (Memr[MKO_X(mko)+j])
				call pargr (Memr[MKO_Y(mko)+j])
				call pargr (gain * Memr[MKO_Z(mko)+j])
			}
			call close (i)
		    }
		}
	    }

	    # If no objects are requested then do the image I/O
	    # line by line to add requested background and noise
	    # and then go on to the next image.

	    irbuf = 0
	    ipbuf = 0
	    if (nobjects == 0) {
		call mkt_free ()
		call mfree (mko, TY_STRUCT)

		if (new) {
		    do i = 1, nl {
			obuf = impl2r (out, i)
			if (background == 0.)
			    call aclrr (Memr[obuf], nc)
			else
			    call amovkr (background, Memr[obuf], nc)
			if (poisson)
			    call mkpnoise (Memr[obuf], Memr[obuf], nc, 0.,
				gain, pbuf, ranbuf, ipbuf, seed1)
			if (rdnoise > 0.)
			    call mkrnoise (Memr[obuf], nc, rdnoise,
				rbuf, ranbuf, irbuf, seed1)
			call alimr (Memr[obuf], nc, dmin, dmax)
			IM_MIN(out) = min (IM_MIN(out), dmin)
			IM_MAX(out) = max (IM_MAX(out), dmax)
		    }
		} else {
		    do i = 1, nl {
			obuf = impl2r (out, i)
			ptr1 = imgl2r (in, i)
			if (background == 0.)
			    call amovr (Memr[ptr1], Memr[obuf], nc)
			else
			    call aaddkr (Memr[ptr1], background,
				Memr[obuf], nc)
			if (poisson)
			    call mkpnoise (Memr[obuf], Memr[obuf], nc, 0.,
				gain, pbuf, ranbuf, ipbuf, seed1)
			if (rdnoise > 0.)
			    call mkrnoise (Memr[obuf], nc, rdnoise,
				rbuf, ranbuf, irbuf, seed1)
			call alimr (Memr[obuf], nc, dmin, dmax)
			IM_MIN(out) = min (IM_MIN(out), dmin)
			IM_MAX(out) = max (IM_MAX(out), dmax)
		    }
		}

		# Add comment history of task parameters.
		if (cmmts) {
		    call strcpy ("# ", Memc[comment], LEN_COMMENT)
		    call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
		    call mkh_comment (out, Memc[comment])
		    call mkh_comment (out, "begin        mknoise")
		    call mkh_comment1 (out, "background", 'r')
		    call mkh_comment1 (out, "gain", 'r')
		    call mkh_comment1 (out, "rdnoise", 'r')
		    call mkh_comment1 (out, "poisson", 'b')
		    call mkh_comment1 (out, "seed", 'i')
		}

		IM_LIMTIME(out) = IM_MTIME(out) + 1
		if (in != out)
		    call imunmap (in)
		call imunmap (out)
		next
	    }

	    # Add the cosmic rays.
	    #
	    # The object list is first sorted in Y for efficiency.
	    # Get buffer of as many lines as possible to minimize random
	    # access and speed up adding the objects.  Ideally the whole
	    # image should be in memory but if not we scroll a buffer
	    # using the fact that the objects are ordered in Y.
	    # Use error checking to determine how much memory is available.

	    mko_sort = MKO_Y(mko)
	    call qsort (Memi[MKO_SORT(mko)], nobjects, mko_compare)

	    for (nlines=nl;; nlines = 0.8 * nlines)
		ifnoerr (call malloc (buf, nlines * nc, TY_REAL))
		    break
	    call malloc (lines, nlines, TY_INT)
	    call malloc (lines, nlines, TY_INT)
	    call malloc (newlines, nl, TY_INT)
	    call amovki (YES, Memi[newlines], nl)

	    # Fill the line buffer.
	    do l = 1, nlines {
		j = mod (l, nlines) 
		ptr2 = buf + j * nc
		Memi[lines+j] = l
		if (new)
		    call aclrr (Memr[ptr2], nc)
		else
		    call amovr (Memr[imgl2r(in,l)], Memr[ptr2], nc) 
		Memi[newlines+l-1] = NO
	    }

	    do i = 0, nobjects-1 {
		j = Memi[MKO_SORT(mko)+i]
		x = Memr[MKO_X(mko)+j]
		y = Memr[MKO_Y(mko)+j]
		z = Memr[MKO_Z(mko)+j]

		call mkt_gstar (mkt, obj, nx, ny, x, y, z)

		c1 = x - nx/2 + 0.5
		c2 = c1 + nx - 1
		c3 = max (1, c1)
		c4 = min (nc, c2)
		l1 = y - ny/2 + 0.5
		l2 = l1 + ny - 1
		l3 = max (1, l1)
		l4 = min (nl, l2)
		k = c4 - c3 + 1 
		ptr1 = obj + (l3 - l1) * nx + c3 - c1
		c3 = c3 - 1
		do l = l3, l4 {
		    j = mod (l, nlines)
		    if (l != Memi[lines+j]) {
			ptr2 = buf + j * nc
			obuf = impl2r (out, Memi[lines+j])
			call amovr (Memr[ptr2], Memr[obuf], nc)
			call alimr (Memr[obuf], nc, dmin, dmax)
			IM_MIN(out) = min (IM_MIN(out), dmin)
			IM_MAX(out) = max (IM_MAX(out), dmax)
			Memi[lines+j] = l
			if (Memi[newlines+l-1] == NO)
			    call amovr (Memr[imgl2r(out,l)], Memr[ptr2], nc)
			else if (new)
			    call aclrr (Memr[ptr2], nc)
			else
			    call amovr (Memr[imgl2r(in,l)], Memr[ptr2], nc)
			Memi[newlines+l-1] = NO
		    }
		    ptr2 = buf + j * nc + c3
		    call aaddr (Memr[ptr1], Memr[ptr2], Memr[ptr2], k)
		    ptr1 = ptr1 + nx
		}
	    }

            # Flush out the line buffer.  If the whole image is in memory then
	    # we can add the background and noise before flushing the data.
	    # Otherwise, we need a second pass reading the image in line
	    # by line and adding the background and noise.

	    if (nlines == nl) {
		do i = 1, nlines {
		    j = mod (i, nlines) 
		    ptr2 = buf + j * nc
		    l = Memi[lines+j]
		    if (background != 0.)
			call aaddkr (Memr[ptr2], background, Memr[ptr2], nc)
		    if (poisson)
			call mkpnoise (Memr[ptr2], Memr[ptr2], nc, 0., gain,
			    pbuf, ranbuf, ipbuf, seed1)
		    if (rdnoise > 0.)
			call mkrnoise (Memr[ptr2], nc, rdnoise,
			    rbuf, ranbuf, irbuf, seed1)
		    obuf = impl2r (out, l)
		    call amovr (Memr[ptr2], Memr[obuf], nc) 
		    call alimr (Memr[obuf], nc, dmin, dmax)
		    IM_MIN(out) = min (IM_MIN(out), dmin)
		    IM_MAX(out) = max (IM_MAX(out), dmax)
		}
	    } else {
		do i = 1, nlines {
		    j = mod (i, nlines) 
		    ptr2 = buf + j * nc
		    l = Memi[lines+j]
		    obuf = impl2r (out, l)
		    call amovr (Memr[ptr2], Memr[obuf], nc) 
		    call alimr (Memr[obuf], nc, dmin, dmax)
		    IM_MIN(out) = min (IM_MIN(out), dmin)
		    IM_MAX(out) = max (IM_MAX(out), dmax)
		}

		call imflush (out)
		do i = 1, nl {
		    obuf = impl2r (out, i)
		    ptr1 = imgl2r (out, i)
		    if (background == 0.)
			call amovr (Memr[ptr1], Memr[obuf], nc)
		    else
			call aaddkr (Memr[ptr1], background, Memr[obuf], nc)
		    if (poisson)
			call mkpnoise (Memr[obuf], Memr[obuf], nc, 0., gain,
			    pbuf, ranbuf, ipbuf, seed1)
		    if (rdnoise > 0.)
			call mkrnoise (Memr[obuf], nc, rdnoise,
			    rbuf, ranbuf, irbuf, seed1)
		    call alimr (Memr[obuf], nc, dmin, dmax)
		    IM_MIN(out) = min (IM_MIN(out), dmin)
		    IM_MAX(out) = max (IM_MAX(out), dmax)
		}
	    }

            # Since each image is different and the object lists may be
            # different we free most of the memory within the image list
            # loop.

	    call mfree (buf, TY_REAL)
	    call mfree (lines, TY_INT)
	    call mkt_free ()
	    call mfree (MKO_X(mko), TY_REAL)
	    call mfree (MKO_Y(mko), TY_REAL)
	    call mfree (MKO_Z(mko), TY_REAL)
	    call mfree (MKO_SORT(mko), TY_INT)
	    call mfree (mko, TY_STRUCT)

	    # Add comment history of task parameters.
	    if (cmmts) {
		call strcpy ("# ", Memc[comment], LEN_COMMENT)
		call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
		call mkh_comment (out, Memc[comment])
		call mkh_comment (out, "begin        mknoise")
		call mkh_comment1 (out, "background", 'r')
		call mkh_comment1 (out, "gain", 'r')
		call mkh_comment1 (out, "rdnoise", 'r')
		call mkh_comment1 (out, "poisson", 'b')
		call mkh_comment1 (out, "seed", 'i')
		if (fcmmts && Memc[fname] != EOS) {
		    call mkh_comment1 (out, "cosrays", 's')
		    i = open (Memc[fname], READ_ONLY, TEXT_FILE)
		    while (fscan (i) != EOF) {
			call gargr (x)
			call gargr (y)
			call gargr (z)
			if (nscan() < 3) {
			    call reset_scan ()
			    call gargstr (Memc[comment], LEN_COMMENT)
			    call mkh_comment (out, Memc[comment])
			}
		    }
		    call close (i)
		}
		call sprintf (Memc[comment], LEN_COMMENT,
		    "%9tncosrays%24t%d")
		    call pargi (nobjects)
		call mkh_comment (out, Memc[comment])
		if (!IS_INDEF (energy))
		    call mkh_comment1 (out, "energy", 'r')
		call mkh_comment1 (out, "radius", 'r')
		call mkh_comment1 (out, "ar", 'r')
		call mkh_comment1 (out, "pa", 'r')
	    }

	    IM_LIMTIME(out) = IM_MTIME(out) + 1
	    if (in != out)
		call imunmap (in)
	    call imunmap (out)

	}

	call imtclose (ilist)
	call imtclose (olist)
	call sfree (sp)
end


# MKRNOISE -- Make gaussian read noise.  A buffer of saved noise values may be
# used to greatly speed up the noise.  In this case new noise values
# are randomly chosen from the buffer.

procedure mkrnoise (data, ndata, rdnoise, buf, nbuf, ibuf, seed)

real	data[ndata]		# Output data
int	ndata			# Number of data points
real	rdnoise			# Read noise (in data units)
pointer	buf			# Random value buffer	
int	nbuf			# Size of random value buffer (may be zero)
int	ibuf			# Number of random numbers saved
				#   ibuf < nbuf		Save new values
				#   ibuf = nbuf		Use saved values
				#   ibuf > nbuf		Use new values
long	seed			# Random number seed

int	i
real	val, urand(), gasdev()

begin
	if (ibuf == nbuf)
	    do i = 1, ndata
		data[i] = data[i] + Memr[buf+int(nbuf*urand (seed))]
	else if (ibuf > nbuf)
	    do i = 1, ndata
		data[i] = data[i] + rdnoise * gasdev (seed)
	else {
	    do i = 1, ndata {
		if (ibuf < nbuf) {
		    val = rdnoise * gasdev (seed)
		    Memr[buf+ibuf] = val
		    ibuf = ibuf + 1
		} else
		    val = Memr[buf+int(nbuf*urand (seed))]
		data[i] = data[i] + val
	    }
	}
end


# MKPNOISE -- Make poisson noise.  For speed, values greater than 20
# use a gaussian approximation with the square root of the value as
# the sigma.  The normalized gaussian values may be saved and reused
# by random selection to speed things up.

procedure mkpnoise (in, data, ndata, b, g, buf, nbuf, ibuf, seed)

real	in[ndata]		# Data to add noise
real	data[ndata]		# Output data
int	ndata			# Number of data points
real	b			# Background (in data units)
real	g			# Gain
pointer	buf			# Random value buffer	
int	nbuf			# Size of random value buffer (may be zero)
int	ibuf			# Number of random numbers saved
				#   ibuf < nbuf		Save new values
				#   ibuf = nbuf		Use saved values
				#   ibuf > nbuf		Use new values
long	seed			# Random number seed

int	i
real	v, gv, urand(), poidev(), gasdev()

begin
	if (ibuf == nbuf)
	    do i = 1, ndata {
		v = g * (in[i] + b)
		if (v < 20.)
		    data[i] = data[i] + (poidev (v, seed) - v) / g
		else
		    data[i] = data[i] +
			sqrt (v) * Memr[buf+int(nbuf*urand(seed))] / g
	    }
	else if (ibuf > nbuf)
	    do i = 1, ndata {
		v = g * (in[i] + b)
		data[i] = data[i] + (poidev (v, seed) - v) / g
	    }
	else {
	    do i = 1, ndata {
		v = g * (in[i] + b)
		if (v < 20.)
		    data[i] = data[i] + (poidev (v, seed) - v) / g
		else {
		    if (ibuf < nbuf) {
			gv = gasdev (seed)
			Memr[buf+ibuf] = gv
			ibuf = ibuf + 1
		    } else
			gv = Memr[buf+int(nbuf*urand (seed))]
		    data[i] = data[i] + sqrt (v) * gv / g
		}
	    }
	}
end
