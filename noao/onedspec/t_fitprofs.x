include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<gset.h>
include	"shdr.h"

# Fit types
define	FITTYPES "|fixed|single|all|"

# Output image options
define	OPTIONS	"|difference|fit|"
define	DIFF		1
define	FIT		2
 
# T_FITPROFS -- Fit image profiles.
 
procedure t_fitprofs()
 
int	inlist			# List of input spectra
pointer	aps			# Aperture list

pointer	pos, sig		# Fitting region and initial components
real	sigma			# Default sigma
int	fitbkg			# Fit background?
int	fitpos			# Position fit flag (1=fixed, 2=single, 3=all)
int	fitsig			# Sigma fit flag (1=fixed, 2=single, 3=all)

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
pointer	sp, input, output
 
real	clgetr()
bool	clgetb()
int	clgeti(), clgwrd(), clscan()
int	imtopenp(), imtgetim(), imtlen()
int	open(), fscan(), nscan()
int	decode_ranges(), nowhite(), btoi()
errchk	open
 
begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (aps, 300, TY_INT)
	call salloc (components, 300, TY_INT)
 
	inlist = imtopenp ("input")
	outlist = imtopenp ("output")
	if (imtlen (outlist) > 1 && imtlen (outlist) != imtlen (inlist))
	    call error (1, "Input and output image lists do not make sense")

	call shdr_2d (NULL, clgeti ("dispaxis"), clgeti ("nsum"))

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
 
	# Decode range string and set complement if needed.
	call clgstr ("lines", Memc[input], SZ_FNAME)
	if (Memc[input] == '!') {
	    complement = true
	    if (decode_ranges (Memc[input+1], Memi[aps], 100, i) == ERR)
	        call error (1, "Bad aperture/record list")
	} else {
	    complement = false
	    if (decode_ranges (Memc[input], Memi[aps], 100, i) == ERR)
	        call error (1, "Bad lines/column/aperture list")
	}

	# Decode components.
	call clgstr ("components", Memc[input], SZ_FNAME)
	if (decode_ranges (Memc[input], Memi[components], 100, i) == ERR)
	    call error (1, "Bad component list")

	while (imtgetim (inlist, Memc[input], SZ_FNAME) != EOF) {
	    if (imtgetim (outlist, Memc[output], SZ_FNAME) == EOF)
		Memc[output] = EOS

	    call fp_ms (Memc[input], Memi[aps], complement,
		Memr[pos], Memr[sig], ng, fitbkg,fitpos, fitsig,
		Memi[components], verbose, log, plot, Memc[output],
		option, clobber,merge)
	}
 
	if (log != NULL)
	    call close (log)
	if (plot != NULL)
	    call close (plot)
	call imtclose (inlist)
	call imtclose (outlist)
	call mfree (pos, TY_REAL)
	call mfree (sig, TY_REAL)
	call shdr_2d (NULL, 0, 0)
	call sfree (sp)
end

 
# FP_MS -- Handle I/O and call fitting procedure.

procedure fp_ms (input, aps, complement, pos, sig, ng, fitbkg, fitpos, fitsig,
	components, verbose, log, plot, output, option, clobber,merge)

char	input[ARB]		# Input image
int	aps[ARB]		# Apertures
bool	complement		# Complement aperture selection

real	pos[ng], sig[ng]	# Positions and sigmas of initial profiles
int	ng			# Number of profiles
int	fitbkg			# Background fit?
int	fitpos			# Position fit flag (1=fixed, 2=single, 3=all)
int	fitsig			# Sigma fit flag (1=fixed, 2=single, 3=all)

int	components[ARB]		# Output Component list
bool	verbose			# Verbose output?
int	log			# Log file descriptor
int	plot			# Plot file descriptor
char	output[ARB]		# Output image
int	option			# Output image option
bool	clobber			# Clobber existing image?
bool	merge			# Merge with existing image?

real	a, b
double	w1, dw, z, aplow, aphigh
bool	select
int	i, j, k, l, p, ap, beam, dtype, nw, ninaps, noutaps, naps, last
int	axis[2]
pointer	ptr, in, out, tmp, mwin, mwout, sh, ct
pointer	sp, str, key, temp, ltm, ltv, coeff, outaps
pointer	model

real	mw_c1tranr()
int	imaccess(), imgnfn()
bool	streq(), strne(), is_in_range(), fp_equalr()
pointer	imofnlu()
pointer	immap(), smw_openim(), mw_open(), mw_sctran(), imgl2r(), impl2r()
errchk	immap, smw_openim, mw_open, shdr_open, imunmap, imgstr, imgl2r, impl2r
errchk	imdelete
data	axis/1,2/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm, 3*3, TY_REAL)
	call salloc (ltv, 3, TY_REAL)
	coeff = NULL

	in = NULL
	out = NULL
	tmp = NULL
	mwin = NULL
	mwout = NULL
	sh = NULL
	ninaps = 0
	noutaps = 0

	iferr {
	    # Check for existing output image and abort if clobber is not set.
	    if (output[1] != EOS && imaccess (output, READ_ONLY) == YES) {
	        if (!clobber) {
		    call sprintf (Memc[str], SZ_LINE,
		        "Output spectrum %s already exists")
		        call pargstr (output)
		    call error (1, Memc[str])
	        } else if (merge) {
		    # Merging when the input and output are the same is nop.
		    if (streq (input, output)) {
			call sfree (sp)
			return
		    }

		    # Open the output and check the type.
		    ptr = immap (output, READ_ONLY, 0); out = ptr
		    ptr = smw_openim (out); mwout = ptr
		    call shdr_open (out, mwout, 1, 1, INDEFI, SHHDR, sh)
		    if (FORMAT(sh) != MULTISPEC) {
			call sprintf (Memc[str], SZ_LINE, "%s - Wrong format")
			    call pargstr (output)
			call error (1, Memc[str])
		    }

		    # Determine existing apertures and renumber them if needed
		    noutaps = IM_LEN(out,2)
		    call salloc (outaps, noutaps, TY_INT)
		    do i = 1, IM_LEN(out,2) {
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
	    call shdr_open (in, mwin, 1, 1, INDEFI, SHHDR, sh)

	    naps = noutaps
	    do i = 1, IM_LEN(in,AAXIS(sh)) {
		call shdr_open (in, mwin, i, 1, INDEFI, SHHDR, sh)
		ap = AP(sh)
		select = is_in_range (aps, ap)
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
		ptr = smw_openim (out); mwout = ptr

		IM_NDIM(tmp) = 2
		IM_LEN(tmp,1) = max (IM_LEN(in,DAXIS(sh)), IM_LEN(out,1))
		IM_LEN(tmp,2) = naps
		do i = 1, IM_LEN(out,2)
		    call amovr (Memr[imgl2r(out,i)], Memr[impl2r(tmp,i)],
			IM_LEN(out,1))
		call imunmap (out)
		out = tmp
		tmp = NULL
	    } else if (Memc[temp] != EOS) {
		ptr = immap (Memc[temp], NEW_COPY, in); out = ptr

		# Set header
		IM_LEN(out,1) = IM_LEN(in,DAXIS(sh))
		IM_LEN(out,2) = naps
		if (IM_LEN(out,2) == 1)
		    IM_NDIM(out) = 1
		else
		    IM_NDIM(out) = 2

		i = imofnlu (out, "DISPAXIS,APID*")
		while (imgnfn (i, Memc[key], SZ_LINE) != EOF)
		    call imdelf (out, Memc[key])
		call imcfnl (i)

		# Set WCS preserving the input phys coordinates for wavelength
		ptr = mw_open (NULL, 2); mwout = ptr
		call mw_newsystem (mwout, "multispec", 2)
		call mw_swtype (mwout, axis, 2, "multispec", "")
		if (LABEL(sh) != EOS)
		    call mw_swattrs (mwout, 1, "label", LABEL(sh))
		if (UNITS(sh) != EOS)
		    call mw_swattrs (mwout, 1, "units", UNITS(sh))
		call aclrr (Memr[ltv], 3)
		call aclrr (Memr[ltm], 3*3)
		call mw_gltermr (mwin, Memr[ltm], Memr[ltv], PNDIM(sh))
		if (DAXIS(sh) == 2) {
		    Memr[ltv] = Memr[ltv+1]
		    Memr[ltm] = Memr[ltm+PNDIM(sh)+1]
		}
		Memr[ltv+1] = 0.
		Memr[ltm+1] = 0.
		Memr[ltm+3] = 1.
		call mw_sltermr (mwout, Memr[ltm], Memr[ltv], 2)
	    }

	    if (out != NULL) {
		# Check dispersion function compatibility
		# Nonlinear functions can be copied to different physical
		# coordinate system though the linear dispersion can be
		# modified.

		call mw_gltermr (mwout,Memr[ltm],Memr[ltv], 2)
		a = Memr[ltv]
		b = Memr[ltm]
		if (DC(sh) == DCFUNC) {
		    call mw_gltermr (mwin, Memr[ltm], Memr[ltv], PNDIM(sh))
		    if (DAXIS(sh) == 2) {
			Memr[ltv] = Memr[ltv+1]
			Memr[ltm] = Memr[ltm+PNDIM(sh)+1]
		    }
		   if (!fp_equalr(a,Memr[ltv])||!fp_equalr(b,Memr[ltm])) {
			call error (1,
	    "Physical basis for nonlinear dispersion functions don't match")
		    }
		}
		ct = mw_sctran (mwout, "logical", "physical", 2)
	    }

	    # Now do the actual fitting
	    call salloc (model, IM_LEN(in,1), TY_REAL)
	    last = noutaps
	    do i = 1, IM_LEN(in,AAXIS(sh)) {
		call shdr_open (in, mwin, i, 1, INDEFI, SHHDR, sh)

		# Check apertures.
		ap = AP(sh)
		select = is_in_range (aps, ap)
		if ((complement && select) || (!complement && !select))
		    next

		call fp_title (sh, Memc[str], verbose, log)

		call shdr_open (in, mwin, i, 1, INDEFI, SHDATA, sh)
		if (SN(sh) < IM_LEN(in,1))
		    call aclrr (Memr[model], IM_LEN(in,1))
		iferr (call fp_fit (sh, Memr[SX(sh)], Memr[SY(sh)], SN(sh),
		    pos, sig, ng, fitbkg, fitpos, fitsig, components,
		    verbose, log, plot, Memc[str], Memr[model])) {
		    call erract (EA_WARN)
		    #next
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
		    p = nint (mw_c1tranr (ct, real(l)))

		    # Copy and adjust dispersion info
		    switch (FORMAT(sh)) {
		    case MULTISPEC:
			call shdr_gwattrs (mwin, PINDEX1(sh), ap, beam,
			    dtype, w1, dw, nw, z, aplow, aphigh, coeff)
			j = nint ((1. - a) / b)
			k = nint ((SN(sh) - a) / b)
			nw = min (min (j ,k) + IM_LEN(out,1), max (j ,k))
			if (dtype == DCLOG) {
			    dw = log10 (W1(sh) / W0(sh)) / (k - j)
			    w1 = log10 (W0(sh) / (1 - z)) - (j - 1) * dw
			} else {
			    dw = (W1(sh) - W0(sh)) / (k - j) / (1 - z)
			    w1 = W0(sh) / (1 - z) - (j - 1) * dw
			}
			call shdr_swattrs (mwout, p, ap, beam, dtype,
			    w1, dw, nw, z, aplow, aphigh, Memc[coeff])
		    case TWODSPEC:
			j = nint ((1. - a) / b)
			k = nint ((SN(sh) - a) / b)
			nw = min (min (j ,k) + IM_LEN(out,1), max (j ,k))
			if (dtype == DCLOG) {
			    dw = log10 (W1(sh)/ W0(sh)) / (k - j)
			    w1 = log10 (W0(sh)) - (j - 1) * dw
			} else {
			    dw = (W1(sh) - W0(sh)) / (k - j)
			    w1 = W0(sh) - (j - 1) * dw
			}

			call sprintf (Memc[key], SZ_LINE, "spec%d")
			    call pargi (p)
			call sprintf (Memc[str], SZ_LINE,
			    "%d %d %d %g %g %d %g %g %g")
			    call pargi (ap)
			    call pargi (BEAM(sh))
			    call pargd (w1)
			    call pargd (dw)
			    call pargi (nw)
			    call pargr (0.)
			    call pargr (APLOW(sh))
			    call pargr (APHIGH(sh))
			call mw_swattrs (mwout, 2, Memc[key], Memc[str])
		    }

		    # Copy titles
		    if (strne (IM_TITLE(out), TITLE(sh))) {
			call sprintf (Memc[key], SZ_LINE, "APID%d")
			    call pargi (p)
			call imastr (out, Memc[key], TITLE(sh))
		    }

		    # Copy the data
		    switch (option) {
		    case DIFF:
		        call asubr (Memr[SY(sh)], Memr[model],
			    Memr[impl2r(out,l)], SN(sh))
		    case FIT:
		        call amovr (Memr[model], Memr[impl2r(out,l)], SN(sh))
		    }

		    # Verify copy
		    if (verbose) {
			if (FORMAT(sh) == TWODSPEC) {
			    if (DAXIS(sh) == 1)
				call printf (
				    "%s: Lines %d - %d  -->  %s: Ap %d\n")
			    else {
				call printf (
				    "%s: Columns %d - %d  -->  %s: Ap %d\n")
				call pargstr (input)
				call pargi (nint(APLOW(sh)))
				call pargi (nint(APHIGH(sh)))
				call pargstr (output)
				call pargi (ap)
			    }
			} else {
			    call printf ("%s: Ap %d  -->  %s: Ap %d\n")
				call pargstr (input)
				call pargi (ap)
				call pargstr (output)
				call pargi (ap)
			}
			call flush (STDOUT)
		    }
		}
	    }

	    call mw_close (mwin)
	    call imunmap (in)
	    if (out != NULL) {
		call smw_saveim (mwout, out)
		call mw_close (mwout)
		call imunmap (out)
		if (strne (Memc[temp], output)) {
		    call imdelete (output)
		    call imrename (Memc[temp], output)
		}
	    }
	} then {
	    if (mwout != NULL)
		call mw_close (mwout)
	    if (mwin != NULL)
		call mw_close (mwin)
	    if (tmp != NULL)
	        call imunmap (tmp)
	    if (out != NULL)
	        call imunmap (out)
	    if (in != NULL)
	        call imunmap (in)
	    call erract (EA_WARN)
	}
    
	call shdr_close (sh)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


define	SQ2PI	2.5066283

# FP_FIT -- Fit profile functions

procedure fp_fit (sh, x, y, n, pos, sig, ng, fitbkg, fitpos, fitsig,
	components, verbose, log, plot, title, model)

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

int	components[ARB]		# Component list
bool	verbose			# Output to STDOUT?
int	log			# Log file descriptor
int	plot			# Plot file descriptor
char	title[ARB]		# Plot title
real	model[n]		# Model

int	i, j, k, i1, i2, nfit
real	xc, x1, x2, y1, y2, dy, z1, dz, w, z, scale
real	height, flux, cont, sigma, eqw, chisq
pointer	sp, str, xd, yd, xg, yg, sg
pointer	gp, gopen()
bool	is_in_range()
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
	    call aclrr (model, n)
	    call error (1, "Too few data points in fitting region")
	}
	x1 = shdr_lw (sh, double(i1))
	x2 = shdr_lw (sh, double(i2))

	# Allocate memory.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (xd, nfit, TY_REAL)
	call salloc (yd, nfit, TY_REAL)
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
	do i = i1, i2 {
	    Memr[xd+i-i1] = x[i]
	    Memr[yd+i-i1] = y[i] - (y1 + dy * (x[i]-x1))
	    scale = max (scale, abs (Memr[yd+i-i1]))
	}
	call adivkr (Memr[yd], scale, Memr[yd], nfit)

	# Setup initial estimates.
	do i = 1, ng {
	    j = max (1, min (nfit, nint (shdr_wl (sh, double(pos[i])))))
	    Memr[xg+i-1] = pos[i]
	    Memr[yg+i-1] = Memr[yd+j-i1]
	    Memr[sg+i-1] = sig[i]
	}
	z1 = 0.
	dz = 0.
	call dofit (fitbkg, fitpos, fitsig, Memr[xd], Memr[yd], nfit,
	    z1, dz, Memr[xg], Memr[yg], Memr[sg], ng, chisq)
	call amulkr (Memr[yg], scale, Memr[yg], ng)
	z1 = z1 * scale
	dz = dz * scale
	y1 = y1 + z1 + dz * x1
	dy = dy + dz

	# Log computed values
	call sprintf (Memc[str], SZ_LINE,
	    "# Nfit=%d, background=%b, positions=%s, sigmas=%s, RMS=%g\n")
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
	    call pargr (scale * sqrt (chisq / ng))
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
	    if (!is_in_range (components, i))
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
	}

	# Compute model.
	call aclrr (model, n)
	do i = 1, ng {
	    if (!is_in_range (components, i))
		next
	    xc = Memr[xg+i-1]
	    height = Memr[yg+i-1]
	    sigma = Memr[sg+i-1]
	    do j = 1, n {
	       z = 0.5 * ((x[j] - xc) / sigma) ** 2
	       if (z < 5)
		   model[j] = model[j] + height * exp (-z)
	    }
	}

	# Draw graphs
	if (plot != NULL) {
	    gp = gopen ("stdvdm", NEW_FILE, plot)
	    call gascale (gp, y[i1], nfit, 2)
	    call asubr (y[i1], model[i1], Memr[yd], nfit)
	    call grscale (gp, Memr[yd], nfit, 2)
	    do i = i1, i2
		Memr[yd+i-i1] = model[i] + y1 + dy * (x[i] - x1)
	    call grscale (gp, Memr[yd], nfit, 2)
	    call gswind (gp, x1, x2, INDEF, INDEF)
	    call glabax (gp, title, "", "")
	    call gseti (gp, G_PLTYPE, 1)
	    call gpline (gp, Memr[xd], y[i1], nfit)
	    call gseti (gp, G_PLTYPE, 2)
	    call gpline (gp, Memr[xd], Memr[yd], nfit)
	    call gline (gp, x1, y1, x2, y1+dy*(x2-x1))
	    call gseti (gp, G_PLTYPE, 3)
	    call asubr (y[i1], model[i1], Memr[yd], nfit)
	    call gpline (gp, Memr[xd], Memr[yd], nfit)
	    call gseti (gp, G_PLTYPE, 4)
	    do i = 1, ng {
		if (!is_in_range (components, i))
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

pointer	sp, time
long	clktime()

begin
	# Select title format.
	switch (FORMAT(sh)) {
	case MULTISPEC:
	    call sprintf (str, SZ_LINE, "%s - Ap %d: %s")
		call pargstr (SPECTRUM(sh))
		call pargi (AP(sh))
		call pargstr (TITLE(sh))
	case ONEDSPEC:
	    call sprintf (str, SZ_LINE, "%s: %s")
		call pargstr (SPECTRUM(sh))
		call pargstr (TITLE(sh))
	case TWODSPEC:
	    if (DAXIS(sh) == 1)
		call sprintf (str, SZ_LINE, "%s[*,%d:%d]: %s")
	    else
		call sprintf (str, SZ_LINE, "%s[%d:%d,*]: %s")
		call pargstr (SPECTRUM(sh))
		call pargi (nint (APLOW(sh)))
		call pargi (nint (APHIGH(sh)))
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
