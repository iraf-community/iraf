include	<error.h>
include	<imhdr.h>
include	<math/iminterp.h>

define	LEN_UA		20000		# Maximum user header
define	LEN_COMMENT	70		# Maximum comment length

define	NALLOC		10		# Alloc block size
define	NPROF		201		# Length of profile

# Each spectrum is described by a 1D spectrum and shape and position info.
define	LEN_MOD		7		# Length of model spectrum structure
define	SPEC	Memi[$1]		# Pointer to spectrum
define	NPTS	Memi[$1+1]		# Number of points in spectrum
define	PTYPE	Memi[$1+2]		# Profile type
define	WIDTH	Memr[P2R($1+3)]		# Profile width (FWHM at center line)
define	DWIDTH	Memr[P2R($1+4)]		# Derivative of width
define	POS	Memr[P2R($1+5)]		# Profile position (at center line)
define	DPOS	Memr[P2R($1+6)]		# Derivative of position

define	PTYPES	"|gaussian|slit|"
define	GAUSS	1			# Gaussian (pexp = 2)
define	SLIT	2			# Slit (pexp = 10)


# T_MK2DSPEC -- Make a 2D spectrum from 1D template and a profile function.
# The dispersion axis is along the columns and the spectrum is taken from
# 1D spectrum.  The cross dispersion profile is either a Gaussian or a
# slit with a specified FWHM.  The center of the profile along the dispersion
# axis is a sloped line.  The width of the profile may also be variable.

procedure t_mk2dspec ()

pointer	input				# Input image
pointer	output				# Output image
pointer	models				# Spectrum models (file)
int	nc				# Number of columns
int	nl				# Number of lines
bool	cmmts				# Add comments?

pointer	template			# Template spectrum name
real	scale				# Intensity scale
int	ptype				# Profile type
real	width				# Profile width (FWHM at center line)
real	dwidth				# Derivative of profile width
real	pos				# Profile position (center of image)
real	dpos				# Deriviative of position

bool	new
int	ilist, olist, mlist
int	i, j, k, k1, k2, fd, npts, nmods, nalloc
real	pcen[2], fwhm[2], flux[2], peak, pstep, pstart, pend, x1, x2, dx
pointer	sp, comment, mod, mods, asi, asis[2], data, in, out, temp, pname

bool	streq(), clgetb()
real	asigrl()
int	clgeti(), access(),  open(), fscan(), nscan(), strdic()
int	imtopenp(), imtlen(), imtgetim(), clktime()
pointer	immap(), imgl1r(), imgl2r(), impl2r()
errchk	open, immap

begin
	call smark (sp)
	call salloc (comment, LEN_COMMENT, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (models, SZ_FNAME, TY_CHAR)
	call salloc (template, SZ_FNAME, TY_CHAR)
	call salloc (pname, SZ_FNAME, TY_CHAR)

	# Make the profile templates stored as an interpolation function
	# with the returned center, fwhm, and flux.

	call mkprof (2., asis[1], pcen[1], fwhm[1], flux[1])
	call mkprof (10., asis[2], pcen[2], fwhm[2], flux[2])

	# Get the file lists and loop through them.
	ilist = imtopenp ("input")
	olist = imtopenp ("output")
	mlist = imtopenp ("models")
	cmmts = clgetb ("comments")

	if (max (1, imtlen (olist)) != imtlen (ilist))
	    call error (1, "Output image list does not match input image list")

	Memc[models] = EOS
	while (imtgetim (ilist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (olist, Memc[output], SZ_FNAME) == EOF)
	        call strcpy (Memc[input], Memc[output], SZ_FNAME)
	    i = imtgetim (mlist, Memc[models], SZ_FNAME)
	    if (access (Memc[models], 0, 0) == NO) {
		call eprintf ("WARNING: Can't access model file (%s)\n")
		    call pargstr (Memc[models])
		next
	    }

	    # Map images.  Check for new, existing, and in-place images.
	    if (streq (Memc[input], Memc[output])) {
		ifnoerr (in = immap (Memc[input], READ_WRITE, 0)) {
		    out = in
	            new = false
	        } else {
		    iferr (out = immap (Memc[output], NEW_IMAGE, LEN_UA)) {
			call erract (EA_WARN)
			next
		    }
		    in = out
	            new = true

		    call clgstr ("header", Memc[comment], LEN_COMMENT)
		    iferr (call mkh_header (out, Memc[comment], true, false))
			call erract (EA_WARN)

	            IM_NDIM(out) = 2
	            IM_LEN(out,1) = clgeti ("ncols")
	            IM_LEN(out,2) = clgeti ("nlines")
	            IM_PIXTYPE(out) = TY_REAL
		    call clgstr ("title", IM_TITLE(out), SZ_IMTITLE)
		    call imaddi (out, "dispaxis", 2)
	        }
	    } else {
	        iferr (in = immap (Memc[input], READ_ONLY, 0)) {
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
	    nc = IM_LEN(out,1)
	    nl = IM_LEN(out,2)

	    # Read the models file.
	    fd = open (Memc[models], READ_ONLY, TEXT_FILE)
	    nmods = 0
	    while (fscan (fd) != EOF) {
	        call gargwrd (Memc[template], SZ_FNAME)
	        call gargr (scale)
	        call gargwrd (Memc[pname], SZ_FNAME)
	        call gargr (width)
	        call gargr (dwidth)
	        call gargr (pos)
	        call gargr (dpos)
	        if (nscan() != 7)
		    next

	        temp = immap (Memc[template], READ_ONLY, 0)
	        npts = IM_LEN(temp,1)
	        call malloc (data, npts, TY_REAL)
	        call amulkr (Memr[imgl1r(temp)], scale, Memr[data], npts)
	        call imunmap (temp)

		call malloc (mod, LEN_MOD, TY_STRUCT)
	        SPEC(mod) = data
	        NPTS(mod) = npts
	        PTYPE(mod) = strdic (Memc[pname], Memc[pname], SZ_FNAME, PTYPES)
	        WIDTH(mod) = width - nl / 2. * dwidth
	        DWIDTH(mod) = dwidth
	        POS(mod) = pos - 1 - nl / 2. * dpos
	        DPOS(mod) = dpos

	        if (nmods == 0) {
		    nalloc = NALLOC
		    call malloc (mods, nalloc, TY_POINTER)
	        } else if (nmods == nalloc) {
		    nalloc = nalloc + NALLOC
		    call realloc (mods, nalloc, TY_POINTER)
	        }
	        Memi[mods+nmods] = mod
	        nmods = nmods + 1
	    }
	    call close (fd)
	    if (nmods == 0) {
	        call imunmap (out)
	        call sfree (sp)
	        call error (1, "No model spectra defined")
	    }

	    # Now expand the 1D spectra into 2D profiles.

	    do i = 1, nl {
	        data = impl2r (out, i)
	        if (new)
	            call aclrr (Memr[data], nc)
	        else
		    call amovr (Memr[imgl2r(in,i)], Memr[data], nc)
	        do j = 1, nmods {
	            mod = Memi[mods+j-1]
	            if (NPTS(mod) < i)
		        next
		    ptype = PTYPE(mod)
		    asi = asis[ptype]
	            peak = Memr[SPEC(mod)+i-1] / flux[ptype]
		    width = WIDTH(mod) + i * DWIDTH(mod)
	            pos = POS(mod) + i * DPOS(mod)
		    pstep = width / fwhm[ptype]
		    pstart = max (-0.5, pos - pcen[ptype] * pstep)
		    pend = min (nc - 0.51, pos + pcen[ptype] * pstep)
		    if (pstart >= pend)
			next

		    k1 = pstart + 0.5
		    k2 = pend + 0.5
		    x1 = (pstart - pos) / pstep + pcen[ptype] + 1
		    x2 = (k1 + 0.5 - pos) / pstep + pcen[ptype] + 1
		    x1 = max (1., x1)
		    x2 = max (1., x2)
		    Memr[data+k1] = Memr[data+k1] + peak * asigrl (asi, x1, x2)

		    dx = 1 / pstep
		    do k = k1+1, k2-1 {
		       x1 = x2
		       x2 = x1 + dx
		       Memr[data+k] = Memr[data+k] + peak * asigrl (asi, x1, x2)
		    }
		    x1 = x2
		    x2 = (pend - pos) / pstep + pcen[ptype] + 1
		    Memr[data+k2] = Memr[data+k2] + peak * asigrl (asi, x1, x2)
	        }
	    }

	    # Add comment history of task parameters.
	    if (cmmts) {
		call strcpy ("# ", Memc[comment], LEN_COMMENT)
		call cnvtime (clktime (0), Memc[comment+2], LEN_COMMENT-2)
		call mkh_comment (out, Memc[comment])
		call mkh_comment (out, "begin        mk2dspec")
		call mkh_comment1 (out, "models", 's')

		fd = open (Memc[models], READ_ONLY, TEXT_FILE)
		while (fscan (fd) != EOF) {
		    call gargstr (Memc[comment], LEN_COMMENT)
		    call mkh_comment (out, Memc[comment])
		}
		call close (fd)
	    }

	    if (in != out)
	        call imunmap (in)
	    call imunmap (out)
	    do i = 0, nmods-1
		call mfree (SPEC(Memi[mods+i]), TY_REAL)
	    call mfree (mods, TY_POINTER)
	}

	call asifree (asis[1])
	call asifree (asis[2])
	call imtclose (ilist)
	call imtclose (olist)
	call imtclose (mlist)
	call sfree (sp)
end


# MKPROF -- Make a well sampled profile and fit it by an interpolation
# function.  Return the interpolation function, the center, the FWHM,
# and total flux.

procedure mkprof (pexp, asi, center, fwhm, flux)

real	pexp		# Profile exponent
pointer	asi		# IMINTERP pointer
real	center		# Profile center
real	fwhm		# FWHM of profile
real	flux		# Flux of profile

int	i
real	scale, x, asigrl()
pointer	sp, prof

begin
	call smark (sp)
	call salloc (prof, NPROF, TY_REAL)

	# Put the profile center at the center of the array.  Set the
	# scale so the array extends to the 0.5% level.  Compute the
	# FWHM.  Generate the profile values and fit the interpolation
	# function.  Finally, compute the total flux by integrating
	# the interpolation function.

	center = (NPROF - 1) / 2.
	scale = center / (log (200.) ** (1/pexp))
	fwhm = 2 * scale * log(2.) ** (1/pexp)
	do i = 0, NPROF-1 {
	    x = abs (i - center) / scale
	    Memr[prof+i] = exp (-(x**pexp))
	}
	call asiinit (asi, II_LINEAR)
	call asifit (asi, Memr[prof], NPROF)

	flux = asigrl (asi, 1., real (NPROF))

	call sfree (sp)
end
