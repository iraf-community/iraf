include	<error.h>
include	<imhdr.h>
include	<smw.h>
include	<gset.h>

# Fit types.
define	FITTYPES "|fixed|single|all|"

# Output image options.
define	OPTIONS	"|difference|fit|"
define	DIFF		1
define	FIT		2

# Monte-Carlo errors
define	MC_SAMPLE		50
define	MC_SIGMA		34
 
# T_FITPROFS -- Fit image profiles.
 
procedure t_fitprofs()
 
int	inlist			# List of input spectra
pointer	aps			# Aperture list
pointer	bands			# Band list

pointer	pos, sig		# Fitting region and initial components
real	sigma			# Default sigma
int	fitbkg			# Fit background?
int	fitpos			# Position fit flag (1=fixed, 2=single, 3=all)
int	fitsig			# Sigma fit flag (1=fixed, 2=single, 3=all)

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
 
real	x, s
bool	complement
int	i, ng, nalloc
pointer	sp, input, output, ptr
 
real	clgetr()
bool	clgetb()
int	clgwrd(), clscan()
int	imtopenp(), imtgetim(), imtlen()
int	open(), fscan(), nscan()
int	nowhite(), btoi()
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

	fitbkg = btoi (clgetb ("fitbackground"))
	fitpos = clgwrd ("fitpositions", Memc[output], SZ_FNAME, FITTYPES)
	fitsig = clgwrd ("fitsigmas", Memc[output], SZ_FNAME, FITTYPES)
	option = clgwrd ("option", Memc[output], SZ_FNAME, OPTIONS)
	sigma = clgetr ("sigma")
	clobber = clgetb ("clobber")
	merge = clgetb ("merge")
        sigma0 = clgetr ("sigma0")
        invgain = clgetr ("invgain")
	if (IS_INDEF(sigma0) || IS_INDEF(invgain) || sigma0<0. || invgain<0.) {
	    sigma0 = INDEF
	    invgain = INDEF
	}

	# Get the initial positions/sigmas.
	call clgstr ("positions", Memc[input], SZ_FNAME)
	i = open (Memc[input], READ_ONLY, TEXT_FILE)
	ng = 0
	while (fscan (i) != EOF) {
	    call gargr (x)
	    call gargr (s)
	    if (nscan() < 0)
		next
	    if (nscan() < 2)
		s = sigma
	    if (ng == 0) {
		nalloc = 10
		call malloc (pos, nalloc, TY_REAL)
		call malloc (sig, nalloc, TY_REAL)
	    } else if (ng == nalloc) {
		nalloc = nalloc + 10
		call realloc (pos, nalloc, TY_REAL)
		call realloc (sig, nalloc, TY_REAL)
	    }
	    Memr[pos+ng] = x
	    Memr[sig+ng] = s
	    ng = ng + 1
	}
	call close (i)
	if (ng == 0)
	    call error (1, "No profiles defined")

	call realloc (pos, ng+2, TY_REAL)
	call realloc (sig, ng+2, TY_REAL)

	# Get fitting region and background points and add to end of
	# pos and sig arrays.

	i = clscan ("region")
	    call gargr (Memr[pos+ng])
	    call gargr (Memr[pos+ng+1])
	    if (i == EOF || nscan() < 1)
		call error (1, "Error specifying fitting region")
	i = clscan ("background")
	    call gargr (Memr[sig+ng])
	    call gargr (Memr[sig+ng+1])
	    if (nscan() < 1)
		Memr[sig+ng] = INDEF
	    if (nscan() < 2)
		Memr[sig+ng+1] = Memr[sig+ng]
 
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

	    call fp_ms (Memc[input], aps, bands, complement,
		Memr[pos], Memr[sig], ng, fitbkg,fitpos, fitsig,
		sigma0, invgain, components, verbose, log, plot, Memc[output],
		option, clobber,merge)
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
	call mfree (pos, TY_REAL)
	call mfree (sig, TY_REAL)
	call sfree (sp)
end

 
# FP_MS -- Handle I/O and call fitting procedure.

procedure fp_ms (input, aps, bands, complement, pos, sig, ng,
	fitbkg, fitpos, fitsig, sigma0, invgain, components, verbose, log, plot,
	output, option, clobber,merge)

char	input[ARB]		# Input image
pointer	aps			# Apertures
pointer	bands			# Bands
bool	complement		# Complement aperture selection

real	pos[ng], sig[ng]	# Positions and sigmas of initial profiles
int	ng			# Number of profiles
int	fitbkg			# Background fit?
int	fitpos			# Position fit flag (1=fixed, 2=single, 3=all)
int	fitsig			# Sigma fit flag (1=fixed, 2=single, 3=all)

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
	sh = NULL
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
	    iferr (call fp_fit (sh, Memr[SX(sh)], Memr[SY(sh)], SN(sh),
		pos, sig, ng, fitbkg, fitpos, fitsig, sigma0, invgain,
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
	call imunmap (in)
	if (out != NULL) {
	    call smw_saveim (mwout, out)
	    call smw_close (MW(shout))
	    call imunmap (out)
	    if (strne (Memc[temp], output)) {
		call imdelete (output)
		call imrename (Memc[temp], output)
	    }
	}
	} then {
	    if (mwout != NULL)
		call smw_close (MW(shout))
	    if (mwin != NULL)
		call smw_close (MW(sh))
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

procedure fp_fit (sh, x, y, n, pos, sig, ng, fitbkg, fitpos, fitsig,
	sigma0, invgain, components, verbose, log, plot, title, mod)

pointer	sh			# Spectrum data structure
real	x[n]			# Coordinates
real	y[n]			# Data
int	n			# Number of data points

real	pos[ARB]		# Fitting region and initial positions
real	sig[ARB]		# Background levels and initial sigmas
int	ng			# Number of gaussian components

int	fitbkg			# Fit background?
int	fitpos			# Fit position (1=fixed, 2=single, 3=all)
int	fitsig			# Fit sigma flag (1=fixed, 2=single, 3=all)

real	sigma0			# Constant noise
real	invgain			# Inverse gain

pointer	components		# Component list
bool	verbose			# Output to STDOUT?
int	log			# Log file descriptor
int	plot			# Plot file descriptor
char	title[ARB]		# Plot title
real	mod[n]			# Model

int	i, j, k, i1, i2, nfit, nsub
long	seed
real	xc, x1, x2, dx, y1, y2, dy, z1, dz, w, z, scale, sscale
real	height, flux, cont, sigma, eqw, chisq
real	flux1, cont1, eqw1, wyc1, slope1
bool	doerr
pointer	sp, str, xd, yd, sd, xg, yg, sg, yd1, xg1, yg1, sg1
pointer	ym, conte, xge, yge, sge, fluxe, eqwe
pointer	gp, gopen()
bool	rng_elementi()
real	model(), gasdev(), asumr()
double	shdr_lw(), shdr_wl
errchk	dofit

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

	# Subtract the continuum and scale the data.
	y1 = sig[ng+1]
	y2 = sig[ng+2]
	if (IS_INDEF (y1))
	    y1 = y[i1]
	if (IS_INDEF (y2))
	    y2 = y[i2]
	dy = (y2 - y1) / (x2 - x1)
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
	    j = max (1, min (nfit, nint (shdr_wl (sh, double(pos[i])))-i1+1))
	    Memr[xg+i-1] = pos[i]
	    Memr[yg+i-1] = Memr[yd+j-1]
	    Memr[sg+i-1] = sig[i]
	}
	z1 = 0.
	dz = 0.
	dx = (x[n] - x[1]) / (n - 1)
	nsub = 3
	call dofit (fitbkg, fitpos, 3, fitsig, Memr[xd], Memr[yd], Memr[sd],
	    nfit, dx, nsub, z1, dz, Memr[xg], Memr[yg], Memr[sg], ng, chisq)

	# Compute Monte-Carlo errors.
	if (doerr) {
	    call salloc (yd1, nfit, TY_REAL)
	    call salloc (ym, nfit, TY_REAL)
	    call salloc (xg1, ng, TY_REAL)
	    call salloc (yg1, ng, TY_REAL)
	    call salloc (sg1, ng, TY_REAL)
	    call salloc (conte, MC_SAMPLE*ng, TY_REAL)
	    call salloc (xge, MC_SAMPLE*ng, TY_REAL)
	    call salloc (yge, MC_SAMPLE*ng, TY_REAL)
	    call salloc (sge, MC_SAMPLE*ng, TY_REAL)
	    call salloc (fluxe, MC_SAMPLE*ng, TY_REAL)
	    call salloc (eqwe, MC_SAMPLE*ng, TY_REAL)
	    do i = 1, nfit {
		w = Memr[xd+i-1]
		Memr[ym+i-1] = model (w, dx, nsub, Memr[xg], Memr[yg],
		    Memr[sg], ng)
	    }
	    seed = 1
	    do i = 0, MC_SAMPLE-1 {
		do j = 1, nfit
		    Memr[yd1+j-1] = Memr[ym+j-1] +
			sscale / scale * Memr[sd+j-1] * gasdev (seed)
		wyc1 = z1
		slope1 = dz
		call amovr (Memr[xg], Memr[xg1], ng)
		call amovr (Memr[yg], Memr[yg1], ng)
		call amovr (Memr[sg], Memr[sg1], ng)
		call dofit (fitbkg, fitpos, 3, fitsig, Memr[xd],
		    Memr[yd1], Memr[sd], nfit, dx, nsub, wyc1, slope1,
		    Memr[xg1], Memr[yg1], Memr[sg1], ng, chisq)

		do j = 0, ng-1 {
		    cont = y1 + z1 + (dy + dz) * Memr[xg+j] - dy * x1
		    cont1 = y1 + wyc1 + (dy + slope1) * Memr[xg+j] - dy * x1
		    flux = Memr[sg+j] * Memr[yg+j] * SQ2PI
		    flux1 = Memr[sg1+j] * Memr[yg1+j] * SQ2PI
		    if (cont > 0. && cont1 > 0.) {
			eqw = -flux / cont
			eqw1 = -flux1 / cont1
		    } else {
			eqw = 0.
			eqw1 = 0.
		    }
		    Memr[conte+j*MC_SAMPLE+i] = abs (cont1 - cont)
		    Memr[xge+j*MC_SAMPLE+i] = abs (Memr[xg1+j] - Memr[xg+j])
		    Memr[yge+j*MC_SAMPLE+i] = abs (Memr[yg1+j] - Memr[yg+j])
		    Memr[sge+j*MC_SAMPLE+i] = abs (Memr[sg1+j] - Memr[sg+j])
		    Memr[fluxe+j*MC_SAMPLE+i] = abs (flux1 - flux)
		    Memr[eqwe+j*MC_SAMPLE+i] = abs (eqw1 - eqw)
		}
	    }
	    do j = 0, ng-1 {
		call asrtr (Memr[conte+j*MC_SAMPLE], Memr[conte+j*MC_SAMPLE],
		    MC_SAMPLE)
		call asrtr (Memr[xge+j*MC_SAMPLE], Memr[xge+j*MC_SAMPLE],
		    MC_SAMPLE)
		call asrtr (Memr[yge+j*MC_SAMPLE], Memr[yge+j*MC_SAMPLE],
		    MC_SAMPLE)
		call asrtr (Memr[sge+j*MC_SAMPLE], Memr[sge+j*MC_SAMPLE],
		    MC_SAMPLE)
		call asrtr (Memr[fluxe+j*MC_SAMPLE], Memr[fluxe+j*MC_SAMPLE],
		    MC_SAMPLE)
		call asrtr (Memr[eqwe+j*MC_SAMPLE], Memr[eqwe+j*MC_SAMPLE],
		    MC_SAMPLE)
	    }
	    call amulkr (Memr[conte], scale, Memr[conte], MC_SAMPLE*ng)
	    call amulkr (Memr[yge], scale, Memr[yge], MC_SAMPLE*ng)
	    call amulkr (Memr[fluxe], scale, Memr[fluxe], MC_SAMPLE*ng)
	}

	call amulkr (Memr[yg], scale, Memr[yg], ng)
	y1 = (y1 + z1 + dz * x1) * scale
	dy = (dy + dz) * scale

	# Log computed values
	call sprintf (Memc[str], SZ_LINE,
	    "# Nfit=%d, background=%b, positions=%s, sigmas=%s\n")
	    call pargi (ng)
	    call pargi (fitbkg)
	    if (fitpos == 1)
		call pargstr ("fixed")
	    else if (fitpos == 2)
		call pargstr ("single")
	    else
		call pargstr ("all")
	    if (fitsig == 1)
		call pargstr ("fixed")
	    else if (fitsig == 2)
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
	    call pargstr ("sigma")
	    call pargstr ("fwhm")
	if (log != NULL)
	    call fprintf (log, Memc[str])
	if (verbose)
	    call printf (Memc[str])
	do i = 1, ng {
	    if (!rng_elementi (components, i))
		next
	    xc = Memr[xg+i-1]
	    cont = y1 + dy * (xc - x1)
	    height = Memr[yg+i-1]
	    sigma = Memr[sg+i-1]
	    flux = sigma * height * SQ2PI
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
		call pargr (height)
		call pargr (sigma)
		call pargr (2.355 * sigma)
	    if (log != NULL)
		call fprintf (log, Memc[str])
	    if (verbose)
		call printf (Memc[str])
	    if (doerr) {
		call sprintf (Memc[str], SZ_LINE,
		" (%7.7g) (%7.7g) (%7.6g) (%7.4g) (%7.6g) (%7.4g) (%7.4g)\n")
		    call pargr (Memr[xge+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (Memr[conte+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (Memr[fluxe+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (Memr[eqwe+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (Memr[yge+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (Memr[sge+(i-1)*MC_SAMPLE+MC_SIGMA])
		    call pargr (2.355 * Memr[sge+(i-1)*MC_SAMPLE+MC_SIGMA])
		if (log != NULL)
		    call fprintf (log, Memc[str])
		if (verbose)
		    call printf (Memc[str])
	    }
	}

	# Compute model.
	call aclrr (mod, n)
	do i = 1, ng {
	    if (!rng_elementi (components, i))
		next
	    xc = Memr[xg+i-1]
	    height = Memr[yg+i-1]
	    sigma = Memr[sg+i-1]
	    do j = 1, n {
	       z = 0.5 * ((x[j] - xc) / sigma) ** 2
	       if (z < 5)
		   mod[j] = mod[j] + height * exp (-z)
	    }
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
	    do i = 1, ng {
		if (!rng_elementi (components, i))
		    next
		xc = Memr[xg+i-1]
		height = Memr[yg+i-1]
		sigma = Memr[sg+i-1]
		k = 0
		do j = i1, i2 {
		    w = x[j]
		    z = 0.5 * ((w - xc) / sigma) ** 2
		    if (z < 5) {
		        z = height * exp (-z) + y1 + dy * (w - x1)
		        if (k == 0) {
			    call gamove (gp, w, z)
			    k = 1
			} else
			    call gadraw (gp, w, z)
		    }
		}
	    }
	    call gclose (gp)
	}

	call sfree (sp)
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
	    if (SMW_LAXIS(smw,1) == 1)
		call sprintf (str, SZ_LINE, "%s[*,%d:%d]: %s")
	    else
		call sprintf (str, SZ_LINE, "%s[%d:%d,*]: %s")
		call pargstr (IMNAME(sh))
		call pargi (nint (APLOW(sh,1)))
		call pargi (nint (APHIGH(sh,1)))
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
