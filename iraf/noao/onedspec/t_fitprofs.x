include	<error.h>
include	<imhdr.h>
include	<smw.h>
include	<gset.h>
include	<ctotok.h>


# Profile types.
define	PTYPES	"|gaussian|lorentzian|voigt|"
define  GAUSS           1       # Gaussian profile
define  LORENTZ         2       # Lorentzian profile
define  VOIGT           3       # Voigt profile

# Type of constraints.
define	FITTYPES "|fixed|single|all|"
define  FIXED           1       # Fixed parameter
define  SINGLE          2       # Fit a single value for all lines
define  INDEP           3       # Fit independent values for all lines

# Elements of fit array.
define  BKG             1       # Background
define  POS             2       # Position
define  INT             3       # Intensity
define  GAU             4       # Gaussian FWHM
define  LOR             5       # Lorentzian FWHM

# Output image options.
define	OPTIONS	"|difference|fit|"
define	DIFF		1
define	FIT		2

# Monte-Carlo errors
define  MC_N    50      # Monte-Carlo samples (overridden by users)
define  MC_P    10      # Percent done interval (percent)
define  MC_SIG  68      # Sigma sample point (percent)

define  NSUB    3       # Number of pixel subsamples

 
# T_FITPROFS -- Fit image profiles.
 
procedure t_fitprofs()
 
int	inlist			# List of input spectra
pointer	aps			# Aperture list
pointer	bands			# Band list

int	ptype			# Profile type
pointer	pg, xg, yg, sg, lg	# Fitting region and initial components
real	gfwhm			# Default gfwhm
real	lfwhm			# Default lfwhm
int	fit[5]			# Fit flags: background, position, gfwhm, lfwhm

int	nerrsample		# Number of error samples to use
real	sigma0			# Constant noise
real	invgain			# Inverse gain

pointer	components		# List of components
bool	verbose			# Verbose?
int	log			# Log file
int	plot			# Plot file
int	outlist			# List of output spectra
int	option			# Output image option
bool	clobber			# Clobber existing images?
bool	merge			# Merge with existing images?
 
real	x, y, g, l
bool	complement
int	i, p, ng, nalloc
pointer	sp, input, output, ptr
 
real	clgetr()
bool	clgetb()
int	clgeti(), clgwrd(), clscan()
int	imtopenp(), imtgetim(), imtlen()
int	open(), fscan(), nscan(), strdic(), nowhite()
pointer	rng_open()
errchk	open
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
 
	# Get parameters.
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	if (imtlen (outlist) > 1 && imtlen (outlist) != imtlen (inlist))
	    call error (1, "Input and output image lists do not make sense")

	verbose = clgetb ("verbose")
	call clgstr ("logfile", Memc[output], SZ_FNAME)
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) == 0)
	    log = NULL
	else
	    log = open (Memc[output], APPEND, TEXT_FILE)
	call clgstr ("plotfile", Memc[output], SZ_FNAME)
	if (nowhite (Memc[output], Memc[output], SZ_FNAME) == 0)
	    plot = NULL
	else
	    plot = open (Memc[output], APPEND, BINARY_FILE)

	ptype = clgwrd ("profile", Memc[output], SZ_FNAME, PTYPES)
	gfwhm = clgetr ("gfwhm")
	lfwhm = clgetr ("lfwhm")

	if (clgetb ("fitbackground"))
	    fit[BKG] = SINGLE
	else
	    fit[BKG] = FIXED
	fit[POS] = clgwrd ("fitpositions", Memc[output], SZ_FNAME, FITTYPES)
	fit[INT] = INDEP
	fit[GAU] = clgwrd ("fitgfwhm", Memc[output], SZ_FNAME, FITTYPES)
	fit[LOR] = clgwrd ("fitlfwhm", Memc[output], SZ_FNAME, FITTYPES)
	option = clgwrd ("option", Memc[output], SZ_FNAME, OPTIONS)
	clobber = clgetb ("clobber")
	merge = clgetb ("merge")
	nerrsample = clgeti ("nerrsample")
	sigma0 = clgetr ("sigma0")
	invgain = clgetr ("invgain")
	if (IS_INDEF(sigma0) || IS_INDEF(invgain) || sigma0<0. || invgain<0.) {
	    sigma0 = INDEF
	    invgain = INDEF
	}

	# Get the initial positions/peak/ptype/gfwhm/lfwhm.
	call clgstr ("positions", Memc[input], SZ_FNAME)
	if (nowhite (Memc[input], Memc[input], SZ_FNAME) == 0) {
	    call sfree (sp)
	    call error (1, "A 'positions' file must be specified")
	}
	i = open (Memc[input], READ_ONLY, TEXT_FILE)
	ng = 0
	while (fscan (i) != EOF) {
	    call gargr (x)
	    call gargr (y)
	    call gargwrd (Memc[output], SZ_FNAME)
	    call gargr (g)
	    call gargr (l)
	    p = strdic (Memc[output], Memc[output], SZ_FNAME, PTYPES)
	    if (p == 0)
		p = ptype
	    switch (nscan()) {
	    case 0:
		next
	    case 1:
		y = INDEF
		p = ptype
		g = gfwhm
		l = lfwhm
	    case 2:
		p = ptype
		g = gfwhm
		l = lfwhm
	    case 3:
		g = gfwhm
		l = lfwhm
	    case 4:
		switch (p) {
		case GAUSS:
		    l = lfwhm
		case LORENTZ:
		    l = g
		    g = gfwhm
		case VOIGT:
		    l = lfwhm
		}
	    }

	    if (ng == 0) {
		nalloc = 10
		call malloc (pg, nalloc, TY_INT)
		call malloc (xg, nalloc, TY_REAL)
		call malloc (yg, nalloc, TY_REAL)
		call malloc (sg, nalloc, TY_REAL)
		call malloc (lg, nalloc, TY_REAL)
	    } else if (ng == nalloc) {
		nalloc = nalloc + 10
		call realloc (pg, nalloc, TY_INT)
		call realloc (xg, nalloc, TY_REAL)
		call realloc (yg, nalloc, TY_REAL)
		call realloc (sg, nalloc, TY_REAL)
		call realloc (lg, nalloc, TY_REAL)
	    }
	    switch (p) {
	    case GAUSS:
		Memi[pg+ng] = p
		Memr[xg+ng] = x
		Memr[yg+ng] = y
		Memr[sg+ng] = g
		Memr[lg+ng] = 0.
	    case LORENTZ:
		Memi[pg+ng] = p
		Memr[xg+ng] = x
		Memr[yg+ng] = y
		Memr[sg+ng] = 0.
		Memr[lg+ng] = g
	    case VOIGT:
		Memi[pg+ng] = p
		Memr[xg+ng] = x
		Memr[yg+ng] = y
		Memr[sg+ng] = g
		Memr[lg+ng] = l
	    }
	    ng = ng + 1
	}
	call close (i)
	if (ng == 0)
	    call error (1, "No profiles defined")

	call realloc (xg, ng+2, TY_REAL)
	call realloc (yg, ng+2, TY_REAL)
	call realloc (sg, ng+2, TY_REAL)
	call realloc (lg, ng+2, TY_REAL)

	# Get fitting region and add to end of xg array.
	i = clscan ("region")
	    call gargr (Memr[xg+ng])
	    call gargr (Memr[xg+ng+1])
	    if (i == EOF || nscan() < 1)
 
	# Decode range strings and set complement if needed.
	complement = false
	call clgstr ("lines", Memc[input], SZ_FNAME)
	ptr = input
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	iferr (aps = rng_open (Memc[ptr], INDEF, INDEF, INDEF))
	    call error (1, "Bad lines/column/aperture list")

	call clgstr ("bands", Memc[input], SZ_FNAME)
	ptr = input
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	iferr (bands = rng_open (Memc[ptr], INDEF, INDEF, INDEF))
	    call error (1, "Bad band list")

	# Decode components.
	call clgstr ("components", Memc[input], SZ_FNAME)
	iferr (components = rng_open (Memc[input], INDEF, INDEF, INDEF))
	    call error (1, "Bad component list")

	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		Memc[output] = EOS

	    call fp_ms (Memc[input], aps, bands, complement, Memi[pg], Memr[xg],
		Memr[yg], Memr[sg], Memr[lg], ng, fit, nerrsample,
		sigma0, invgain, components, verbose, log, plot, Memc[output],
		option, clobber, merge)
	}

	if (log != NULL)
	    call close (log)
	if (plot != NULL)
	    call close (plot)
	call rng_close (aps)
	call rng_close (bands)
	call rng_close (components)
	call imtclose (inlist)
	call imtclose (outlist)
	call mfree (pg, TY_INT)
	call mfree (xg, TY_REAL)
	call mfree (yg, TY_REAL)
	call mfree (sg, TY_REAL)
	call mfree (lg, TY_REAL)
	call sfree (sp)
end

 
# FP_MS -- Handle I/O and call fitting procedure.

procedure fp_ms (input, aps, bands, complement, pg, xg, yg, sg, lg, ng, fit,
	nerrsample, sigma0, invgain, components, verbose, log, plot, output,
	option, clobber, merge)

char	input[ARB]		# Input image
pointer	aps			# Apertures
pointer	bands			# Bands
bool	complement		# Complement aperture selection

int	pg[ng]			# Profile type
real	xg[ng]			# Positions
real	yg[ng]			# Peaks
real	sg[ng]			# Gaussian FWHM
real	lg[ng]			# Lorentzian FWHM
int	ng			# Number of profiles
int	fit[5]			# Fit flags

int	nerrsample		# Number of error samples
real	sigma0			# Constant noise
real	invgain			# Inverse gain

pointer	components		# Output Component list
bool	verbose			# Verbose output?
int	log			# Log file descriptor
int	plot			# Plot file descriptor
char	output[ARB]		# Output image
int	option			# Output image option
bool	clobber			# Clobber existing image?
bool	merge			# Merge with existing image?

real	aplow[2], aphigh[2]
double	a, b, w1, wb, dw, z, p1, p2, p3
bool	select
int	i, j, k, l, ap, beam, dtype, nw, ninaps, noutaps, nbands, naps, last
int	mwoutdim, axis[3]
pointer	ptr, in, out, tmp, mwin, mwout, sh, shout
pointer	sp, str, key, temp, ltm1, ltv1, ltm2, ltv2, coeff, outaps
pointer	model

double	shdr_lw()
int	imaccess(), imgnfn()
bool	streq(), strne(), rng_elementi(), fp_equald()
pointer	smw_openim(), mw_open()
pointer	immap(), imgl3r(), impl3r(), imofnlu()
errchk	immap, smw_openim, mw_open, shdr_open, imunmap, imgstr, imgl3r, impl3r
errchk	imdelete
data	axis/1,2,3/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm1, 3*3, TY_DOUBLE)
	call salloc (ltv1, 3, TY_DOUBLE)
	call salloc (ltm2, 3*3, TY_DOUBLE)
	call salloc (ltv2, 3, TY_DOUBLE)
	coeff = NULL

	# Initialize.
	in = NULL; out = NULL; tmp = NULL
	mwin = NULL; mwout = NULL
	sh = NULL; shout = NULL
	ninaps = 0; noutaps = 0; nbands = 0

	iferr {
	# Check for existing output image and abort if clobber is not set.
	if (output[1] != EOS && imaccess (output, READ_ONLY) == YES) {
	    if (!clobber) {
		call sprintf (Memc[str], SZ_LINE,
		    "Output spectrum %s already exists")
		    call pargstr (output)
		call error (1, Memc[str])
	    } else if (merge) {
		# Merging when the input and output are the same is a nop.
		if (streq (input, output)) {
		    call sfree (sp)
		    return
		}

		# Open the output and check the type.
		ptr = immap (output, READ_ONLY, 0); out = ptr
		ptr = smw_openim (out); mwout = ptr
                if (SMW_FORMAT(mwout) == SMW_ND) {
                    call sprintf (Memc[str], SZ_LINE, "%s - Wrong format")
                        call pargstr (output)
                    call error (1, Memc[str])
                }

		# Determine existing apertures.
		noutaps = SMW_NSPEC(mwout)
		nbands = SMW_NBANDS(mwout)
		call salloc (outaps, noutaps, TY_INT)
		do i = 1, noutaps {
		    call shdr_open (out, mwout, i, 1, INDEFI, SHHDR, sh)
		    Memi[outaps+i-1] = AP(sh)
		}
	    }
	    call mktemp ("temp", Memc[temp], SZ_FNAME)
	} else
	    call strcpy (output, Memc[temp], SZ_FNAME)

	# Open the input and determine the number of final output
	# apertures in order to set the output dimensions.

	ptr = immap (input, READ_ONLY, 0); in = ptr
	ptr = smw_openim (in); mwin = ptr

	naps = noutaps

	j = 1
	if (SMW_FORMAT(mwin) != SMW_ND) {
	    j = 0
	    do i = 1, SMW_NBANDS(mwin) {
		select = rng_elementi (bands, i)
		if (!select)
		    next
		j = j + 1
	    }
	    if (j == 0)
		call error (1, "No bands selected in image")
	}
	nbands = max (j, nbands)

	do i = 1, SMW_NSPEC(mwin) {
	    call shdr_open (in, mwin, i, 1, INDEFI, SHHDR, sh)
	    ap = AP(sh)
	    if (SMW_FORMAT(mwin) == SMW_ND) {
		call smw_mw (mwin, i, 1, ptr, j, k)
		select = rng_elementi (aps, j) && rng_elementi (bands, k)
	    } else
		select = rng_elementi (aps, ap)

	    if ((complement && select) || (!complement && !select))
		next
	    for (j=0; j<noutaps && Memi[outaps+j]!=ap; j=j+1)
		;
	    if (j == noutaps)
		naps = naps + 1
	    ninaps = ninaps + 1
	}
	if (ninaps == 0) {
	    call sprintf (Memc[str], SZ_LINE, "No apertures selected in %s")
		call pargstr (input)
	    call error (1, Memc[str])
	}

	# Set the output spectrum.  For merging with an existing output
	# copy to a temporary spectrum with size set appropriately.
	# For a new output setup copy the input header, reset the
	# physical line mapping, and clear all dispersion parameters.
	
	if (out != NULL) {
	    ptr = immap (Memc[temp], NEW_COPY, out); tmp = ptr
	    if (IM_PIXTYPE(tmp) != TY_DOUBLE)
		IM_PIXTYPE(tmp) = TY_REAL

	    IM_LEN(tmp,1) = max (SMW_LLEN(mwin,1), IM_LEN(out,1))
	    IM_LEN(tmp,2) = naps
	    IM_LEN(tmp,3) = max (nbands, IM_LEN(out,3))
	    if (nbands > 1)
		IM_NDIM(tmp) = 3
	    else if (naps > 1)
		IM_NDIM(tmp) = 2
	    else
		IM_NDIM(tmp) = 1

	    do j = 1, IM_LEN(out,3)
		do i = 1, IM_LEN(out,2) {
		    ptr = impl3r (tmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(tmp,1))
		    call amovr (Memr[imgl3r(out,i,j)], Memr[ptr], IM_LEN(out,1))
		}
	    do j = 1, IM_LEN(out,3)
		do i = IM_LEN(out,2)+1, IM_LEN(tmp,2) {
		    ptr = impl3r (tmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(tmp,1))
		}
	    do j = IM_LEN(out,3)+1, nbands
		do i = 1, IM_LEN(tmp,2) {
		    ptr = impl3r (tmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(tmp,1))
		}
	    call imunmap (out)
	    out = tmp
	    tmp = NULL
	} else if (Memc[temp] != EOS) {
	    ptr = immap (Memc[temp], NEW_COPY, in); out = ptr
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL

	    # Set header
	    IM_LEN(out,1) = SMW_LLEN(mwin,1)
	    IM_LEN(out,2) = naps
	    IM_LEN(out,3) = nbands
	    if (nbands > 1)
		IM_NDIM(out) = 3
	    else if (naps > 1)
		IM_NDIM(out) = 2
	    else
		IM_NDIM(out) = 1
	    mwoutdim = IM_NDIM(out)

	    j = imofnlu (out, "DISPAXIS,APID*,BANDID*")
	    while (imgnfn (j, Memc[key], SZ_LINE) != EOF)
		call imdelf (out, Memc[key])
	    call imcfnl (j)

	    i = SMW_PDIM(mwin)
	    j = SMW_PAXIS(mwin,1)

	    ptr = mw_open (NULL, mwoutdim); mwout = ptr
	    call mw_newsystem (mwout, "equispec", mwoutdim)
	    call mw_swtype (mwout, axis, mwoutdim, "linear", "")
	    if (LABEL(sh) != EOS)
		call mw_swattrs (mwout, 1, "label", LABEL(sh))
	    if (UNITS(sh) != EOS)
		call mw_swattrs (mwout, 1, "units", UNITS(sh))

	    call mw_gltermd (SMW_MW(mwin,0), Memd[ltm1], Memd[ltv1], i)
            call mw_gltermd (mwout, Memd[ltm2], Memd[ltv2], mwoutdim)
            Memd[ltv2] = Memd[ltv1+(j-1)]
            Memd[ltm2] = Memd[ltm1+(i+1)*(j-1)]
	    call mw_sltermd (mwout, Memd[ltm2], Memd[ltv2], mwoutdim)
	    call smw_open (mwout, NULL, out)
	}

	if (out != NULL) {
	    # Check dispersion function compatibility
	    # Nonlinear functions can be copied to different physical
	    # coordinate system though the linear dispersion can be
	    # modified.

	    call mw_gltermd (SMW_MW(mwout,0), Memd[ltm2], Memd[ltv2], mwoutdim)
	    a = Memd[ltv2]
	    b = Memd[ltm2]
	    if (DC(sh) == DCFUNC) {
		i = SMW_PDIM(mwin)
		j = SMW_PAXIS(mwin,1)

		call mw_gltermd (SMW_MW(mwin,0), Memd[ltm1], Memd[ltv1], i)
		Memd[ltv1] = Memd[ltv1+(j-1)]
		Memd[ltm1] = Memd[ltm1+(i+1)*(j-1)]
	       if (!fp_equald (a,Memd[ltv1]) || !fp_equald (b,Memd[ltm1])) {
		    call error (1,
		"Physical basis for nonlinear dispersion functions don't match")
		}
	    }
	}

	# Now do the actual fitting
	call salloc (model, SMW_LLEN(mwin,1), TY_REAL)
	last = noutaps
	do i = 1, SMW_NSPEC(mwin) {
	    call shdr_open (in, mwin, i, 1, INDEFI, SHHDR, sh)

	    # Check apertures.
	    ap = AP(sh)
	    if (SMW_FORMAT(mwin) == SMW_ND) {
		call smw_mw (mwin, i, 1, ptr, j, k)
		select = rng_elementi (aps, j) && rng_elementi (bands, k)
	    } else
		select = rng_elementi (aps, ap)

	    if ((complement && select) || (!complement && !select))
		next

	    call fp_title (sh, Memc[str], verbose, log)

	    call shdr_open (in, mwin, i, 1, INDEFI, SHDATA, sh)
	    if (SN(sh) < SMW_LLEN(mwin,1))
		call aclrr (Memr[model], SMW_LLEN(mwin,1))
	    iferr (call fp_fit (sh, Memr[SX(sh)], Memr[SY(sh)], SN(sh), pg,
		xg, yg, sg, lg, ng, fit, nerrsample, sigma0, invgain,
		components, verbose, log, plot, Memc[str], Memr[model])) {
		call erract (EA_WARN)
	    }

	    if (out != NULL) {
		for (j=0; j<noutaps && Memi[outaps+j]!=ap; j=j+1)
		    ;

		# Set output logical and physical lines
		if (j < noutaps)
		    l = j + 1
		else {
		    l = last + 1
		    last = l
		}

		# Copy and adjust dispersion info
		call smw_gwattrs (mwin, i, 1, AP(sh), beam,
		    dtype, w1, dw, nw, z, aplow, aphigh, coeff)

		w1 = shdr_lw (sh, 1D0)
		wb = shdr_lw (sh, double (SN(sh)))
		p1 = (NP1(sh) - a) / b
		p2 = (NP2(sh) - a) / b
		p3 = (IM_LEN(out,1) - a) / b
		nw = nint (min (max (p1 ,p3), max (p1 ,p2))) + NP1(sh) - 1
		if (p1 != p2)
		    dw = (wb - w1) / (p2 - p1) * (1 + z)
		w1 = w1 * (1 + z) - (p1 - 1) * dw

		call smw_swattrs (mwout, l, 1, ap, beam, dtype,
		    w1, dw, nw, z, aplow, aphigh, Memc[coeff])

		# Copy titles
		call smw_sapid (mwout, l, 1, TITLE(sh))
		if (Memc[SID(sh,1)] != EOS)
		    call imastr (out, "BANDID1", Memc[SID(sh,1)])

		# Copy the data
		switch (option) {
		case DIFF:
		    call asubr (Memr[SY(sh)], Memr[model],
			Memr[impl3r(out,l,1)+NP1(sh)-1], SN(sh))
		case FIT:
		    call amovr (Memr[model], Memr[impl3r(out,l,1)+NP1(sh)-1],
			SN(sh))
		}

		# Verify copy
		if (verbose) {
		    call shdr_open (out, mwout, l, 1, INDEFI, SHHDR, shout)
		    call printf ("%s%s(%d)  -->  %s%s(%s)\n")
			call pargstr (IMNAME(sh))
			call pargstr (IMSEC(sh))
			call pargi (AP(sh))
			call pargstr (IMNAME(shout))
			call pargstr (IMSEC(shout))
			call pargi (AP(shout))
		    call flush (STDOUT)
		}
	    }
	}

	call smw_close (MW(sh))
	if (out != NULL) {
	    call smw_saveim (mwout, out)
	    if (shout != NULL)
		call smw_close (MW(shout))
	    call imunmap (out)
	    if (strne (Memc[temp], output)) {
		call imdelete (output)
		call imrename (Memc[temp], output)
	    }
	}
	call imunmap (in)
	} then {
	    if (shout != NULL)
		call smw_close (MW(shout))
	    else if (mwout != NULL)
		call smw_close (mwout)
	    if (sh != NULL)
		call smw_close (MW(sh))
	    else if (mwin != NULL)
		call smw_close (mwin)
	    if (tmp != NULL)
	        call imunmap (tmp)
	    if (out != NULL)
	        call imunmap (out)
	    if (in != NULL)
	        call imunmap (in)
	    call erract (EA_WARN)
	}
    
	call shdr_close (shout)
	call shdr_close (sh)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


define	SQ2PI	2.5066283

# FP_FIT -- Fit profile functions

procedure fp_fit (sh, x, y, n, ptypes, pos, peaks, gfwhms, lfwhms, ng, fit,
	nerrsample, sigma0, invgain, components, verbose, log, plot, title, mod)

pointer	sh			# Spectrum data structure
real	x[n]			# Coordinates
real	y[n]			# Data
int	n			# Number of data points

int	ptypes[ARB]		# Profile types
real	pos[ARB]		# Fitting region and initial positions
real	peaks[ARB]		# Peak values
real	gfwhms[ARB]		# Background levels and initial gfwhm
real	lfwhms[ARB]		# Initial lfwhm
int	ng			# Number of gaussian components

int	fit[5]			# Fit flags

int	nerrsample		# Number of error samples
real	sigma0			# Constant noise
real	invgain			# Inverse gain

pointer	components		# Component list
bool	verbose			# Output to STDOUT?
int	log			# Log file descriptor
int	plot			# Plot file descriptor
char	title[ARB]		# Plot title
real	mod[n]			# Model

int	i, j, k, i1, i2, nfit, nsub, mc_n, mc_p, mc_sig
long	seed
real	xc, x1, x2, dx, y1, dy, z1, dz, w, z, scale, sscale
real	peak, flux, cont, gfwhm, lfwhm, eqw, chisq
real	flux1, cont1, eqw1, wyc1, slope1, v, u
bool	doerr
pointer	sp, str, xd, yd, sd, xg, yg, sg, lg, pg, yd1, xg1, yg1, sg1, lg1
pointer	ym, conte, xge, yge, sge, lge, fluxe, eqwe
pointer	gp, gopen()
bool	rng_elementi()
real	model(), gasdev(), asumr()
double	shdr_lw(), shdr_wl
errchk	fp_background, dofit, dorefit

begin
	# Determine fitting region.
	x1 = pos[ng+1]
	x2 = pos[ng+2]
	i1 = nint (shdr_wl (sh, double(x1)))
	i2 = nint (shdr_wl (sh, double(x2)))
	i = min (n, max (i1, i2))
	i1 = max (1, min (i1, i2))
	i2 = i
	nfit = i2 - i1 + 1
	if (nfit < 3) {
	    call aclrr (mod, n)
	    call error (1, "Too few data points in fitting region")
	}
	x1 = shdr_lw (sh, double(i1))
	x2 = shdr_lw (sh, double(i2))

	# Allocate memory.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (xd, nfit, TY_REAL)
	call salloc (yd, nfit, TY_REAL)
	call salloc (sd, nfit, TY_REAL)
	call salloc (xg, ng, TY_REAL)
	call salloc (yg, ng, TY_REAL)
	call salloc (sg, ng, TY_REAL)
	call salloc (lg, ng, TY_REAL)
	call salloc (pg, ng, TY_INT)

	# Subtract the continuum and scale the data.
	call fp_background (sh, x, y, n, x1, x2, y1, dy)
	scale = 0.
	doerr = !IS_INDEF(sigma0)
	do i = i1, i2 {
	    Memr[xd+i-i1] = x[i]
	    Memr[yd+i-i1] = y[i] - (y1 + dy * (x[i]-x1))
	    if (y[i] <= 0.)
		doerr = false
	    scale = max (scale, abs (Memr[yd+i-i1]))
	}
	if (doerr) {
	    do i = i1, i2
		Memr[sd+i-i1] = sqrt (sigma0 ** 2 + invgain * y[i])
	    sscale = asumr (Memr[sd], nfit) / nfit
	} else {
	    call amovkr (1., Memr[sd], nfit)
	    sscale = 1.
	}
	call adivkr (Memr[yd], scale, Memr[yd], nfit)
	call adivkr (Memr[sd], sscale, Memr[sd], nfit)
	y1 = y1 / scale
	dy = dy / scale

	# Setup initial estimates.
	do i = 1, ng {
	    Memr[xg+i-1] = pos[i]
	    Memr[sg+i-1] = gfwhms[i]
	    Memr[lg+i-1] = lfwhms[i]
	    Memi[pg+i-1] = ptypes[i]
	    if (IS_INDEF(peaks[i])) {
		j = max (1, min (nfit, nint (shdr_wl(sh,double(pos[i])))-i1+1))
		Memr[yg+i-1] = Memr[yd+j-1]
	    } else
		Memr[yg+i-1] = peaks[i] / scale
	}
	z1 = 0.
	dz = 0.
	dx = (x[n] - x[1]) / (n - 1)
	nsub = NSUB
	call dofit (fit, Memr[xd], Memr[yd], Memr[sd],
	    nfit, dx, nsub, z1, dz, Memr[xg], Memr[yg], Memr[sg],
	    Memr[lg], Memi[pg], ng, chisq)

	# Compute Monte-Carlo errors.
	mc_n = nerrsample
	mc_p = nint (mc_n * MC_P / 100.)
	mc_sig = nint (mc_n * MC_SIG / 100.)
	if (doerr && mc_sig > 9) {
	    call salloc (yd1, nfit, TY_REAL)
	    call salloc (ym, nfit, TY_REAL)
	    call salloc (xg1, ng, TY_REAL)
	    call salloc (yg1, ng, TY_REAL)
	    call salloc (sg1, ng, TY_REAL)
	    call salloc (lg1, ng, TY_REAL)
	    call salloc (conte, mc_n*ng, TY_REAL)
	    call salloc (xge, mc_n*ng, TY_REAL)
	    call salloc (yge, mc_n*ng, TY_REAL)
	    call salloc (sge, mc_n*ng, TY_REAL)
	    call salloc (lge, mc_n*ng, TY_REAL)
	    call salloc (fluxe, mc_n*ng, TY_REAL)
	    call salloc (eqwe, mc_n*ng, TY_REAL)
	    do i = 1, nfit {
		w = Memr[xd+i-1]
		Memr[ym+i-1] = model (w, dx, nsub, Memr[xg], Memr[yg],
		    Memr[sg], Memr[lg], Memi[pg], ng)
	    }
	    seed = 1
	    do i = 0, mc_n-1 {
		do j = 1, nfit
		    Memr[yd1+j-1] = Memr[ym+j-1] +
			sscale / scale * Memr[sd+j-1] * gasdev (seed)
		wyc1 = z1
		slope1 = dz
		call amovr (Memr[xg], Memr[xg1], ng)
		call amovr (Memr[yg], Memr[yg1], ng)
		call amovr (Memr[sg], Memr[sg1], ng)
		call amovr (Memr[lg], Memr[lg1], ng)
		call dorefit (fit, Memr[xd], Memr[yd1], Memr[sd],
		    nfit, dx, nsub, wyc1, slope1,
		    Memr[xg1], Memr[yg1], Memr[sg1],
		    Memr[lg1], Memi[pg], ng, chisq)

		do j = 0, ng-1 {
		    cont = y1 + z1 + (dy + dz) * Memr[xg+j] - dy * x1
		    cont1 = y1 + wyc1 + (dy + slope1) * Memr[xg+j] - dy * x1
		    switch (Memi[pg+j]) {
		    case GAUSS:
			flux = 1.064467 * Memr[yg+j] * Memr[sg+j]
			flux1 = 1.064467 * Memr[yg1+j] * Memr[sg1+j]
		    case LORENTZ:
			flux = 1.570795 * Memr[yg+j] * Memr[lg+j]
			flux1 = 1.570795 * Memr[yg1+j] * Memr[lg1+j]
		    case VOIGT:
			call voigt (0., 0.832555*Memr[lg+j]/Memr[sg+j], v, u)
			flux = 1.064467 * Memr[yg+j] * Memr[sg+j] / v
			call voigt (0., 0.832555*Memr[lg1+j]/Memr[sg1+j], v, u)
			flux1 = 1.064467 * Memr[yg1+j] * Memr[sg1+j] / v
		    }
		    if (cont > 0. && cont1 > 0.) {
			eqw = -flux / cont
			eqw1 = -flux1 / cont1
		    } else {
			eqw = 0.
			eqw1 = 0.
		    }
		    Memr[conte+j*mc_n+i] = abs (cont1 - cont)
		    Memr[xge+j*mc_n+i] = abs (Memr[xg1+j] - Memr[xg+j])
		    Memr[yge+j*mc_n+i] = abs (Memr[yg1+j] - Memr[yg+j])
		    Memr[sge+j*mc_n+i] = abs (Memr[sg1+j] - Memr[sg+j])
		    Memr[lge+j*mc_n+i] = abs (Memr[lg1+j] - Memr[lg+j])
		    Memr[fluxe+j*mc_n+i] = abs (flux1 - flux)
		    Memr[eqwe+j*mc_n+i] = abs (eqw1 - eqw)
		}
	    }
	    do j = 0, ng-1 {
		call asrtr (Memr[conte+j*mc_n], Memr[conte+j*mc_n], mc_n)
		call asrtr (Memr[xge+j*mc_n], Memr[xge+j*mc_n], mc_n)
		call asrtr (Memr[yge+j*mc_n], Memr[yge+j*mc_n], mc_n)
		call asrtr (Memr[sge+j*mc_n], Memr[sge+j*mc_n], mc_n)
		call asrtr (Memr[lge+j*mc_n], Memr[lge+j*mc_n], mc_n)
		call asrtr (Memr[fluxe+j*mc_n], Memr[fluxe+j*mc_n], mc_n)
		call asrtr (Memr[eqwe+j*mc_n], Memr[eqwe+j*mc_n], mc_n)
	    }
	    call amulkr (Memr[conte], scale, Memr[conte], mc_n*ng)
	    call amulkr (Memr[yge], scale, Memr[yge], mc_n*ng)
	    call amulkr (Memr[fluxe], scale, Memr[fluxe], mc_n*ng)
	}

	call amulkr (Memr[yg], scale, Memr[yg], ng)
	y1 = (y1 + z1 + dz * x1) * scale
	dy = (dy + dz) * scale

	# Log computed values
	call sprintf (Memc[str], SZ_LINE,
	    "# Nfit=%d, background=%b, positions=%s, gfwhm=%s, lfwhm=%s\n")
	    call pargi (ng)
	    call pargb (fit[BKG] == SINGLE)
	    if (fit[POS] == FIXED)
		call pargstr ("fixed")
	    else if (fit[POS] == SINGLE)
		call pargstr ("single")
	    else
		call pargstr ("all")
	    if (fit[GAU] == FIXED)
		call pargstr ("fixed")
	    else if (fit[GAU] == SINGLE)
		call pargstr ("single")
	    else
		call pargstr ("all")
	    if (fit[LOR] == FIXED)
		call pargstr ("fixed")
	    else if (fit[LOR] == SINGLE)
		call pargstr ("single")
	    else
		call pargstr ("all")
	if (log != NULL)
	    call fprintf (log, Memc[str])
	if (verbose)
	    call printf (Memc[str])
	call sprintf (Memc[str], SZ_LINE, "# %8s%10s%10s%10s%10s%10s%10s\n")
	    call pargstr ("center")
	    call pargstr ("cont")
	    call pargstr ("flux")
	    call pargstr ("eqw")
	    call pargstr ("core")
	    call pargstr ("gfwhm")
	    call pargstr ("lfwhm")
	if (log != NULL)
	    call fprintf (log, Memc[str])
	if (verbose)
	    call printf (Memc[str])
	do i = 1, ng {
	    if (!rng_elementi (components, i))
		next
	    xc = Memr[xg+i-1]
	    cont = y1 + dy * (xc - x1)
	    peak = Memr[yg+i-1]
	    gfwhm = Memr[sg+i-1]
	    lfwhm = Memr[lg+i-1]
	    switch (Memi[pg+i-1]) {
	    case 1:
		flux = 1.064467 * peak * gfwhm
	    case 2:
		flux = 1.570795 * peak * lfwhm
	    case 3:
		call voigt (0., 0.832555*lfwhm/gfwhm, v, u)
		flux = 1.064467 * peak * gfwhm / v
	    }

	    if (cont > 0.)
		eqw = -flux / cont
	    else
		eqw = INDEF

	    call sprintf (Memc[str], SZ_LINE,
		" %9.7g %9.7g %9.6g %9.4g %9.6g %9.4g %9.4g\n")
		call pargr (xc)
		call pargr (cont)
		call pargr (flux)
		call pargr (eqw)
		call pargr (peak)
		call pargr (gfwhm)
		call pargr (lfwhm)
	    if (log != NULL)
		call fprintf (log, Memc[str])
	    if (verbose)
		call printf (Memc[str])
	    if (doerr && mc_sig > 9) {
		call sprintf (Memc[str], SZ_LINE,
		" (%7.7g) (%7.7g) (%7.6g) (%7.4g) (%7.6g) (%7.4g) (%7.4g)\n")
		    call pargr (Memr[xge+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[conte+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[fluxe+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[eqwe+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[yge+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[sge+(i-1)*mc_n+mc_sig])
		    call pargr (Memr[lge+(i-1)*mc_n+mc_sig])
		if (log != NULL)
		    call fprintf (log, Memc[str])
		if (verbose)
		    call printf (Memc[str])
	    }
	}

	# Compute model.
	call aclrr (mod, n)
	do i = 0, ng-1 {
	    if (!rng_elementi (components, i+1))
		next
	    do j = 1, n
		#mod[j] = model (x[j], dx, nsub, Memr[xg+i], Memr[yg+i],
		#    Memr[sg+i], Memr[lg+i], Memi[pg+i], ng)
		mod[j] = mod[j] + model (x[j], dx, nsub, Memr[xg+i], Memr[yg+i],
		    Memr[sg+i], Memr[lg+i], Memi[pg+i], 1)
	}

	# Draw graphs
	if (plot != NULL) {
	    gp = gopen ("stdvdm", NEW_FILE, plot)
	    call gascale (gp, y[i1], nfit, 2)
	    call asubr (y[i1], mod[i1], Memr[yd], nfit)
	    call grscale (gp, Memr[yd], nfit, 2)
	    do i = i1, i2
		Memr[yd+i-i1] = mod[i] + y1 + dy * (x[i] - x1)
	    call grscale (gp, Memr[yd], nfit, 2)
	    call gswind (gp, x1, x2, INDEF, INDEF)
	    call glabax (gp, title, "", "")
	    call gseti (gp, G_PLTYPE, 1)
	    call gpline (gp, Memr[xd], y[i1], nfit)
	    call gseti (gp, G_PLTYPE, 2)
	    call gpline (gp, Memr[xd], Memr[yd], nfit)
	    call gline (gp, x1, y1, x2, y1+dy*(x2-x1))
	    call gseti (gp, G_PLTYPE, 3)
	    call asubr (y[i1], mod[i1], Memr[yd], nfit)
	    call gpline (gp, Memr[xd], Memr[yd], nfit)
	    call gseti (gp, G_PLTYPE, 4)
	    do i = 0, ng-1 {
		if (!rng_elementi (components, i+1))
		    next
		k = 0
		do j = i1, i2 {
		    w = x[j]
		    z = model (w, dx, nsub, Memr[xg+i], Memr[yg+i],
			Memr[sg+i], Memr[lg+i], Memi[pg+i], 1)
		    z = z + y1 + dy * (w - x1)
		    if (k == 0) {
			call gamove (gp, w, z)
			k = 1
		    } else
			call gadraw (gp, w, z)
		}
	    }
	    call gclose (gp)
	}

	call sfree (sp)
end


# FP_BACKGROUND -- Iniital background.

procedure fp_background (sh, x, y, n, x1, x2, y1, dy)

pointer	sh			#I Spectrum pointer
real	x[n]			#I Coordinate values
real	y[n]			#I Data
int	n			#I Number of data points
real	x1, x2			#I Fit endpoints
real	y1, dy			#O Background

int	i, j, k, m, func
real	xval[2], yval[2]
double	z1, z2, z3
pointer	sp, bkg, str

int	ctotok(), ctor(), ctod(), strdic(), nscan()
real	asumr(), amedr()
double	shdr_wl(), shdr_lw()

define	err_	10

begin
	call smark (sp)
	call salloc (bkg, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	xval[1] = x1
	xval[2] = x2

	call clgstr ("background", Memc[bkg], SZ_LINE)
	call sscan (Memc[bkg])
	do j = 1, 2 {
	    call gargwrd (Memc[bkg], SZ_LINE)
	    if (nscan() != j) {
		i = max (1, min (n, nint (shdr_wl (sh, double(xval[j])))))
		xval[j] = shdr_lw (sh, double(i))
		yval[j] = y[i]
		next
	    }

	    k = 1
	    if (ctor (Memc[bkg], k, yval[j]) == 0) {
		if (ctotok (Memc[bkg], k, Memc[str], SZ_LINE) != TOK_IDENTIFIER)
		    goto err_
		func = strdic (Memc[str], Memc[str], SZ_LINE, "|avg|med|")
		if (func == 0)
		    goto err_
		k = k + 1
		if (ctod (Memc[bkg], k, z1) == 0)
		    goto err_
		k = k + 1
		if (ctod (Memc[bkg], k, z2) == 0)
		    goto err_
		k = k + 1
		if (ctod (Memc[bkg], k, z3) == 0)
		    z3 = 1

		z1 = shdr_wl (sh, z1)
		z2 = shdr_wl (sh, z2)
		i = max (1, nint(min(z1,z2)))
		m = min (n, nint(max(z1,z2))) - i + 1
		if (m < 1)
		    goto err_

		# This is included to eliminate an optimizer bug on solaris.
		call sprintf (Memc[bkg], SZ_LINE, "%g %g %g %d %d\n")
		    call pargd (z1)
		    call pargd (z2)
		    call pargd (z3)
		    call pargi (i)
		    call pargi (m)

		switch (func) {
		case 1:
		    xval[j] = z3 * asumr (x[i], m) / m
		    yval[j] = z3 * asumr (y[i], m) / m
		case 2:
		    xval[j] = z3 * asumr (x[i], m) / m
		    yval[j] = z3 * amedr (y[i], m)
		}
	    }
	}

	if (xval[1] == xval[2]) {
	   dy = 0.
	   y1 = (yval[1] + yval[2]) / 2.
	} else {
	    dy = (yval[2] - yval[1]) / (xval[2] - xval[1])
	    y1 = yval[1] + dy * (x1 - xval[1])
	}
	return
    
err_
	call sfree (sp)
	call error (1, "Syntax error in background specification")
end


include	<time.h>

# FP_TITLE -- Set title string and print.

procedure fp_title (sh, str, verbose, log)

pointer	sh			# Spectrum header structure
char	str[SZ_LINE]		# Title string
bool	verbose			# Verbose?
int	log			# Log file descriptor

pointer	sp, time, smw
long	clktime()

begin
	# Select title format.
	smw = MW(sh)
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call sprintf (str, SZ_LINE, "%s%s: %s")
		call pargstr (IMNAME(sh))
		call pargstr (IMSEC(sh))
		call pargstr (TITLE(sh))
	case SMW_ES, SMW_MS:
	    call sprintf (str, SZ_LINE, "%s - Ap %d: %s")
		call pargstr (IMNAME(sh))
		call pargi (AP(sh))
		call pargstr (TITLE(sh))
	}

	# Set time and log header.
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call cnvdate (clktime(0), Memc[time], SZ_DATE)
       if (log != NULL) {
	    call fprintf (log, "# %s %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	}
       if (verbose) {
	    call printf ("# %s %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	}

	call sfree (sp)
end
