include	<error.h>
include	<imhdr.h>
include	<math.h>

define	MAX_HDR		20000			# Maximum user header
define	COMMENT		"COMMENT   "		# Comment key
define	LEN_COMMENT	70			# Maximum comment length

# Object data structure
define	LEN_MKO		9
define	MKO_MKT		Memi[$1]		# Template
define	MKO_X		Memi[$1+1]		# X position
define	MKO_Y		Memi[$1+2]		# Y position
define	MKO_Z		Memi[$1+3]		# Flux
define	MKO_R		Memi[$1+4]		# Scale size
define	MKO_AR		Memi[$1+5]		# Axial ratio
define	MKO_PA		Memi[$1+6]		# Position angle
define	MKO_SAVE	Memi[$1+7]		# Save template?
define	MKO_SORT	Memi[$1+8]		# Sort index


# T_MKOBJECTS -- Add stars and galaxies to images.
# New images may be created with a background and noise.

procedure t_mkobjects ()

int	ilist				# Input image list
int	olist				# Output image list
int	objects				# List of model files
real	xo				# X offset
real	yo				# Y offset
int	nl				# Number of lines
int	nc				# Number of columns
real	background			# Background level
real	gain				# Gain (electrons/DN)
int	ranbuf				# Random number buffer size
real	rdnoise				# Read noise (in electrons)
bool	poisson				# Add Poisson noise?
real	exptime				# Exposure time
real	distance			# Relative distance
real	m0				# Magnitude zero point
long	seed				# Random number seed

int	nobjects, save
real	x, y, z, r, ar, pa

bool	new, bsave
int	i, j, k, l, nx, ny, nlines, c1, c2, c3, c4, l1, l2, l3, l4, irbuf, ipbuf
pointer	sp, input, output, fname, star, comment, rbuf, pbuf
pointer	in, out, buf, lines, newlines, obj, ptr1, ptr2
pointer	mko, mkt

long	clgetl(), clktime()
bool	clgetb(), streq()
int	imtopenp(), imtlen(), imtgetim(), btoi()
int	clgeti(), open(), fscan(), nscan(), imaccess()
real	clgetr()
pointer	immap(), imgl2r(), impl2r()
pointer	mkt_star(), mkt_object()
errchk	open, immap, imgl2r, impl2r, malloc, realloc, mkt_star, mkt_object

int	mko_compare()
extern	mko_compare
pointer	mko_sort
common	/mko_qsort/ mko_sort

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (star, SZ_FNAME, TY_CHAR)
	call salloc (comment, LEN_COMMENT, TY_CHAR)

	# Get parameters which apply to all images.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	objects = imtopenp ("objects")
	xo = clgetr ("xoffset")
	yo = clgetr ("yoffset")
	call clgstr ("star", Memc[star], SZ_FNAME)
	distance = clgetr ("distance")
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
	exptime = clgetr ("exptime")
	m0 = clgetr ("magzero")
	seed = clgetl ("seed")

	background = exptime * background

	if (imtlen (objects) == 0)
	    call error (1, "No objects list")

	# Loop through input, output, and object lists.
	# Missing output images take the input image name.
	# The object list will repeat if shorter than input list.

	Memc[fname] = EOS
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input], Memc[output], SZ_FNAME)

	    # Get and check object list.
	    i = imtgetim (objects, Memc[fname], SZ_FNAME)
	    iferr (i = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		next
	    }

	    # Map images.  Check for new, existing, and in-place images.
	    if (streq (Memc[input], Memc[output])) {
		if (imaccess (Memc[input], 0) == YES) {
		    iferr (in = immap (Memc[input], READ_WRITE, MAX_HDR)) {
			call erract (EA_WARN)
			next
		    }
		    out = in
		    new = false
		} else {
		    iferr (in = immap (Memc[input], NEW_IMAGE, MAX_HDR)) {
			call erract (EA_WARN)
			next
		    }
		    out = in

		    IM_NDIM(out) = 2
		    IM_LEN(out,1) = clgeti ("ncols")
		    IM_LEN(out,2) = clgeti ("nlines")
		    IM_PIXTYPE(out) = TY_REAL
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)
		    call imaddr (out, "exptime", exptime)
		    call imaddr (out, "gain", gain)
		    call imaddr (out, "rdnoise", rdnoise * gain)
		    call mko_header (out)

		    new = true
		}
	    } else {
		iferr (in = immap (Memc[input], READ_ONLY, MAX_HDR)) {
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

	    # Read the object list.
	    call malloc (mko, LEN_MKO, TY_STRUCT)
	    call mkt_init ()

	    # Set star and seeing templates.
	    mkt = mkt_star (Memc[star])

	    # Read the object list.
	    nobjects = 0
	    while (fscan (i) != EOF) {
		call gargr (x)
		call gargr (y)
		call gargr (z)
		if (nscan() < 3) {
		    call reset_scan ()
		    call gargstr (Memc[comment], LEN_COMMENT)
		    call mko_comment (out, Memc[comment])
		    next
		}
		call gargwrd (Memc[fname], SZ_FNAME)
		call gargr (r)
		call gargr (ar)
		call gargr (pa)
		call gargb (bsave)
		x = (x + xo) / distance
		y = (y + yo) / distance
		if (x < 1 || x > nc || y < 1 || y > nl)
		    next
		if (nobjects == 0) {
		    j = 100
		    call malloc (MKO_MKT(mko), j, TY_POINTER)
		    call malloc (MKO_X(mko), j, TY_REAL)
		    call malloc (MKO_Y(mko), j, TY_REAL)
		    call malloc (MKO_Z(mko), j, TY_REAL)
		    call malloc (MKO_R(mko), j, TY_REAL)
		    call malloc (MKO_AR(mko), j, TY_REAL)
		    call malloc (MKO_PA(mko), j, TY_REAL)
		    call malloc (MKO_SAVE(mko), j, TY_INT)
		    call malloc (MKO_SORT(mko), j, TY_INT)
		} else if (nobjects == j) {
		    j = j + 100
		    call realloc (MKO_MKT(mko), j, TY_POINTER)
		    call realloc (MKO_X(mko), j, TY_REAL)
		    call realloc (MKO_Y(mko), j, TY_REAL)
		    call realloc (MKO_Z(mko), j, TY_REAL)
		    call realloc (MKO_R(mko), j, TY_REAL)
		    call realloc (MKO_AR(mko), j, TY_REAL)
		    call realloc (MKO_PA(mko), j, TY_REAL)
		    call realloc (MKO_SAVE(mko), j, TY_INT)
		    call realloc (MKO_SORT(mko), j, TY_INT)
		}

		Memr[MKO_X(mko)+nobjects] = x
		Memr[MKO_Y(mko)+nobjects] = y
		Memr[MKO_Z(mko)+nobjects] =
		    exptime / (distance * distance) * 10. ** (-0.4*(z-m0))
		if (nscan() < 7)
		    Memi[MKO_MKT(mko)+nobjects] = mkt_star (Memc[star])
		else {
		    Memi[MKO_MKT(mko)+nobjects] = mkt_object (Memc[fname])
		    Memr[MKO_R(mko)+nobjects] = r / distance
		    Memr[MKO_AR(mko)+nobjects] = ar
		    Memr[MKO_PA(mko)+nobjects] = DEGTORAD (pa)
		    if (nscan() == 8)
			Memi[MKO_SAVE(mko)+nobjects] = btoi (bsave)
		    else
			Memi[MKO_SAVE(mko)+nobjects] = NO
		}
		Memi[MKO_SORT(mko)+nobjects] = nobjects
		nobjects = nobjects + 1
	    }
	    call close (i)

	    # Add comment history of task parameters.
	    call strcpy ("# ", Memc[comment], LEN_COMMENT)
	    call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
	    call mko_comment (out, Memc[comment])
	    call mko_comment (out, "begin\tmkobjects")
	    call mko_comment1 (out, "background", 'r', Memc[comment])
	    call mko_comment1 (out, "xoffset", 'r', Memc[comment])
	    call mko_comment1 (out, "yoffset", 'r', Memc[comment])
	    call mko_comment1 (out, "star", 's', Memc[comment])
	    call mko_comment1 (out, "radius", 'r', Memc[comment])
	    call mko_comment1 (out, "beta", 'r', Memc[comment])
	    call mko_comment1 (out, "ar", 'r', Memc[comment])
	    call mko_comment1 (out, "pa", 'r', Memc[comment])
	    call mko_comment1 (out, "distance", 'r', Memc[comment])
	    call mko_comment1 (out, "exptime", 'r', Memc[comment])
	    call mko_comment1 (out, "magzero", 'r', Memc[comment])
	    call mko_comment1 (out, "gain", 'r', Memc[comment])
	    call mko_comment1 (out, "rdnoise", 'r', Memc[comment])
	    call mko_comment1 (out, "poisson", 'b', Memc[comment])
	    call mko_comment1 (out, "seed", 'i', Memc[comment])

	    # If no objects are requested then do the image I/O
	    # line by line.  Add noise if creating a new image or
	    # copy the input image if a new output image is desired.
	    # Then go on to the next image.

	    irbuf = 0
	    ipbuf = 0
	    if (nobjects == 0) {
		call mkt_free ()
		call mfree (mko, TY_STRUCT)

		if (new) {
		    do i = 1, nl {
			ptr2 = impl2r (out, i)
			if (background == 0.)
			    call aclrr (Memr[ptr2], nc)
			else
			    call amovkr (background, Memr[ptr2], nc)
			if (poisson)
			    call mkpnoise (Memr[ptr2], Memr[ptr2], nc, 0., gain,
				pbuf, ranbuf, ipbuf, seed)
			if (rdnoise > 0.)
			    call mkrnoise (Memr[ptr2], nc, rdnoise,
				rbuf, ranbuf, irbuf, seed)
		    }
		} else if (in != out) {
		    do i = 1, nl
			call amovr (Memr[imgl2r(in,i)], Memr[impl2r(out,i)],
			    IM_LEN(in,1))
		}

		if (in != out)
		    call imunmap (in)
		call imunmap (out)
		next
	    }

	    # Add the objects.
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
		
	    # Generate the object subrasters, add noise if needed, and
	    # add the data to the line buffer.  Check for parts of the
	    # object off the image (the object center is guarenteed to
	    # be on the image).  Do image I/O if needed.

	    do i = 0, nobjects-1 {
		j = Memi[MKO_SORT(mko)+i]
		mkt = Memi[MKO_MKT(mko)+j]
		if (mkt == NULL)
		    next
		x = Memr[MKO_X(mko)+j]
		y = Memr[MKO_Y(mko)+j]
		z = Memr[MKO_Z(mko)+j]
		r = Memr[MKO_R(mko)+j]
		ar = Memr[MKO_AR(mko)+j]
		pa = Memr[MKO_PA(mko)+j]
		save = Memi[MKO_SAVE(mko)+j]

		call mkt_gobject (mkt, obj, nx, ny, x, y, z, r, ar, pa, save)

		c1 = nint (x - nx/2)
		c2 = c1 + nx - 1
		c3 = max (1, c1)
		c4 = min (nc, c2)
		l1 = nint (y - ny/2)
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
			call amovr (Memr[ptr2],
			    Memr[impl2r(out, Memi[lines+j])], nc)
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
		    if (!new) {
		        if (poisson)
			    call mkpnoise (Memr[ptr1], Memr[ptr2], k,
				background, gain, pbuf, ranbuf, ipbuf, seed)
		        if (rdnoise > 0.)
			    call mkrnoise (Memr[ptr2], k,
				rdnoise, rbuf, ranbuf, irbuf, seed)
		    }
		    ptr1 = ptr1 + nx
		}
	    }

	    # Flush out the line buffer.  A new image requires addition of
	    # background and noise.  If the whole image is in memory then
	    # we can add the background and noise before flushing the data.
	    # Otherwise, we need a second pass reading the image in line
	    # by line and adding the background and noise.  Note that if
	    # the image was not new then noise was added only to the
	    # objects.
	    
	    if (nlines == nl) {
		do i = 1, nlines {
		    j = mod (i, nlines) 
		    ptr2 = buf + j * nc
		    l = Memi[lines+j]
		    if (new) {
			if (background != 0.)
			    call aaddkr (Memr[ptr2], background, Memr[ptr2], nc)
			if (poisson)
			    call mkpnoise (Memr[ptr2], Memr[ptr2], nc, 0., gain,
				pbuf, ranbuf, ipbuf, seed)
			if (rdnoise > 0.)
			    call mkrnoise (Memr[ptr2], nc, rdnoise,
				rbuf, ranbuf, irbuf, seed)
		    }
		    call amovr (Memr[ptr2], Memr[impl2r(out,l)], nc) 
		}
	    } else {
		do i = 1, nlines {
		    j = mod (i, nlines) 
		    ptr2 = buf + j * nc
		    l = Memi[lines+j]
		    call amovr (Memr[ptr2], Memr[impl2r(out,l)], nc) 
		}

		if (new) {
		    call imflush (out)
		    do i = 1, nl {
			ptr2 = impl2r (out, i)
			ptr1 = imgl2r (out, i)
			if (background == 0.)
			    call amovr (Memr[ptr1], Memr[ptr2], nc)
			else
			    call aaddkr (Memr[ptr1], background, Memr[ptr2], nc)
			if (poisson)
			    call mkpnoise (Memr[ptr2], Memr[ptr2], nc, 0., gain,
				pbuf, ranbuf, ipbuf, seed)
			if (rdnoise > 0.)
			    call mkrnoise (Memr[ptr2], nc, rdnoise,
				rbuf, ranbuf, irbuf, seed)
		    }
		}
	    }

	    # Since each image is different and the object lists may be
	    # different we free most of the memory within the image list
	    # loop.

	    call mfree (buf, TY_REAL)
	    call mfree (lines, TY_INT)
	    call mfree (newlines, TY_INT)
	    call mkt_free ()
	    call mfree (MKO_MKT(mko), TY_POINTER)
	    call mfree (MKO_X(mko), TY_REAL)
	    call mfree (MKO_Y(mko), TY_REAL)
	    call mfree (MKO_Z(mko), TY_REAL)
	    call mfree (MKO_R(mko), TY_REAL)
	    call mfree (MKO_AR(mko), TY_REAL)
	    call mfree (MKO_PA(mko), TY_REAL)
	    call mfree (MKO_SAVE(mko), TY_INT)
	    call mfree (MKO_SORT(mko), TY_INT)
	    call mfree (mko, TY_STRUCT)

	    if (in != out)
		call imunmap (in)
	    call imunmap (out)

	}

	call imtclose (ilist)
	call imtclose (olist)
	call sfree (sp)
end


# MKO_COMPARE -- Compare two values in the mko_sort array.

int procedure mko_compare (i, j)

int	i, j		# Array indices to be compared.

pointer	mko_sort
common	/mko_qsort/ mko_sort

begin
	if (Memr[mko_sort+i] < Memr[mko_sort+j])
	    return (-1)
	else if (Memr[mko_sort+i] > Memr[mko_sort+j])
	    return (1)
	else
	    return (0)
end


# MKO_COMMENT -- Add comment to header.

procedure mko_comment (im, comment)

pointer	im			# image descriptor
char	comment[ARB]		# comment

pointer	ua

begin
	ua = IM_USERAREA(im)
	if (Memc[ua] == EOS)
	    call strcat ("\n", Memc[ua], MAX_HDR)
	call strcat (COMMENT, Memc[ua], MAX_HDR)
	call strcat (comment, Memc[ua], MAX_HDR)
	call strcat ("\n", Memc[ua], MAX_HDR)
end


# MKO_COMMENT1 -- Make comment out of CL parameter.

procedure mko_comment1 (im, param, type, comment)

pointer	im			# image descriptor
char	param[ARB]		# parameter name
int	type			# datatype
char	comment[ARB]		# comment string

bool	clgetb()
int	clgeti()
real	clgetr()
pointer	str

begin
	switch (type) {
	case 'b':
	    call sprintf (comment, LEN_COMMENT, "\t%s%24t%b")
		call pargstr (param)
	    	call pargb (clgetb (param))
	case 'i':
	    call sprintf (comment, LEN_COMMENT, "\t%s%24t%d")
		call pargstr (param)
	    	call pargi (clgeti (param))
	case 'r':
	    call sprintf (comment, LEN_COMMENT, "\t%s%24t%g")
		call pargstr (param)
	    	call pargr (clgetr (param))
	case 's':
	    call malloc (str, SZ_FNAME, TY_CHAR)
	    call clgstr (param, Memc[str], SZ_FNAME)
	    call sprintf (comment, LEN_COMMENT, "\t%s%24t%s")
		call pargstr (param)
	    	call pargstr (Memc[str])
	    call mfree (str, TY_CHAR)
	}

	call mko_comment (im, comment)
end


# MKO_HEADER -- Set new image header.

procedure mko_header (im)

pointer	im			# Image pointer

int	i, fd, open(), nowhite(), fscan(), nscan()
real	r
pointer	sp, key, type, str
errchk	open

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (type, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	call clgstr ("header", Memc[key], SZ_FNAME)
	if (nowhite (Memc[key], Memc[key], SZ_FNAME) > 0) {
	    iferr {
		fd = NULL
		i = open (Memc[key], READ_ONLY, TEXT_FILE)
		fd = i
		while (fscan (fd) != EOF) {
		    call gargwrd (Memc[key], SZ_FNAME)
		    call gargwrd (Memc[type], SZ_FNAME)
		    if (nscan() < 2)
			next
		    switch (Memc[type]) {
		    case 'i':
			call gargi (i)
			if (nscan() == 3)
			    call imaddi (im, Memc[key], i)
		    case 'r':
			call gargr (r)
			if (nscan() == 3)
			    call imaddr (im, Memc[key], r)
		    default:
			call gargstr (Memc[str], SZ_LINE)
			if (nscan() == 3)
			    call imastr (im, Memc[key], Memc[str])
		    }
		}
		call close (fd)
	    } then {
		call erract (EA_WARN)
		if (fd != NULL)
		    call close (fd)
	    }
	}
	call sfree (sp)
end
