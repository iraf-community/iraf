include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	<smw.h>
 
# Output formats.
define	FORMATS		"|multispec|onedspec|"

# Operations.
define	OPS	"|abs|copy|dex|exp|flam|fnu|inv|ln|log|lum|mag|sqrt\
		 |replace|+|-|*|/|^|"
define	ABS	1
define	COPY	2
define	DEX	3
define	EXP	4
define	FLAM	5
define	FNU	6
define	INV	7
define	LN	8
define	LOG	9
define	LUM	10
define	MAG	11
define	SQRT	12

define	REP	13
define	ADD	14
define	SUB	15
define	MUL	16
define	DIV	17
define	POW	18


# T_SARITH -- Arithmetic operations (including copying) on spectra.
 
procedure t_sarith()
 
int	inlist1			# List of input spectra
int	op			# Operation
int	inlist2			# List of input spectra or operands
int	outlist			# List of output spectra
double	w1			# Starting wavelength
double	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
int	format			# Output format
pointer	aps			# Aperture/col/line list
pointer	bands			# Band list
pointer	beams			# Beam list
bool	complement		# Complement aperture/beam selection
int	apmod			# Aperture modulus (used with subapertures)
int	offset			# Add this offset to apertures on output
bool	reverse			# Reverse order of operands
bool	ignoreaps		# Ignore apertures?
bool	clobber			# Clobber existing images?
bool	merge			# Merge with existing images?
bool	renumber		# Renumber apertures?
bool	verbose			# Verbose?
real	errval			# Error value
 
int	list1, list2
pointer	sp, input1, opstr, input2, output, ptr
 
double	clgetd()
int	imtopenp(), imtopen(), imtlen(), imtgetim()
int	clgwrd(), clgeti()
bool	clgetb()
pointer	rng_open()
common	/sarith/ errval
 
begin
	call smark (sp)
	call salloc (input1, SZ_LINE, TY_CHAR)
	call salloc (opstr, SZ_LINE, TY_CHAR)
	call salloc (input2, SZ_LINE, TY_CHAR)
	call salloc (output, SZ_LINE, TY_CHAR)
 
	# Get parameters.
	inlist1 = imtopenp ("input1")
	op = clgwrd ("op", Memc[opstr], SZ_LINE, OPS)
	if (op > SQRT)
	    inlist2 = imtopenp ("input2")
	else
	    inlist2 = imtopen ("")
	outlist = imtopenp ("output")

	w1 = clgetd ("w1")
	w2 = clgetd ("w2")
	if (IS_INDEFD(w1) && IS_INDEFD(w2))
	    rebin = false
	else
	    rebin = clgetb ("rebin")

	format = clgwrd ("format", Memc[input1], SZ_LINE, FORMATS)
	call clgstr ("apertures", Memc[input1], SZ_LINE)
	call clgstr ("bands", Memc[input2], SZ_LINE)
	call clgstr ("beams", Memc[output], SZ_LINE)
	apmod = clgeti ("apmodulus")
	offset = clgeti ("offset")
	reverse = clgetb ("reverse")
	ignoreaps = clgetb ("ignoreaps")
	clobber = clgetb ("clobber")
	merge = clgetb ("merge")
	renumber = clgetb ("renumber")
	verbose = clgetb ("verbose")
	errval = clgetd ("errval")
 
	if (op == 0)
	    call error (1, "Unknown operation")

	# Decode range strings and set complement if needed
	ptr = input1
	complement = false
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	iferr (aps = rng_open (Memc[ptr], INDEF, INDEF, INDEF))
	    call error (0, "Bad aperture/column/line list")

	ptr = input2
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	iferr (bands = rng_open (Memc[ptr], INDEF, INDEF, INDEF))
	    call error (0, "Bad band list")

	ptr = output
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	iferr (beams = rng_open (Memc[ptr], INDEF, INDEF, INDEF))
	    call error (0, "Bad beam list")

	# Check lists.
	if (imtlen (outlist) > 1 && imtlen (outlist) != imtlen (inlist1))
	    call error (1, "Input and output image lists don't make sense")
	if (op > SQRT &&
	    imtlen (inlist2) > 1 && imtlen (inlist2) != imtlen (inlist1))
	    call error (1, "Input operand lists don't make sense")
 
	# Do the operations.
	while (imtgetim (inlist1, Memc[input1], SZ_LINE) != EOF) {
	    if (imtgetim (inlist2, Memc[output], SZ_LINE) == EOF)
		call strcpy (Memc[input2], Memc[output], SZ_LINE)
	    call strcpy (Memc[output], Memc[input2], SZ_LINE)

	    if (imtlen (outlist) > 1) {
		list1 = imtopen (Memc[input1])
		list2 = imtopen (Memc[input2])
	    } else {
		list1 = inlist1
		list2 = inlist2
	    }

	    switch (format) {
	    case 1:
		if (imtgetim (outlist, Memc[output], SZ_LINE) == EOF)
		    call strcpy (Memc[input1], Memc[output], SZ_LINE)
		call imgimage (Memc[output], Memc[output], SZ_LINE)
		call sa_ms (list1, list2, Memc[output], op, Memc[opstr],
		    w1, w2, rebin, aps, bands, beams, complement, apmod,
		    offset, reverse, ignoreaps, clobber, merge, renumber,
		    verbose)
	    case 2:
		call sa_getim (outlist, Memc[input1], Memc[output], SZ_LINE)
		call imgimage (Memc[output], Memc[output], SZ_LINE)
		call sa_1d (list1, list2, Memc[output], op, Memc[opstr],
		    w1, w2, rebin, aps, bands, beams, complement, apmod,
		    offset, reverse, ignoreaps, clobber, renumber, verbose)
	    }

	    if (list1 != inlist1) {
		call imtclose (list1)
		call imtclose (list2)
	    }
	}

	call rng_close (aps)
	call rng_close (bands)
	call rng_close (beams)
	call imtclose (inlist1)
	call imtclose (inlist2)
	call imtclose (outlist)
	call sfree (sp)
end


# SA_MS -- Operate on input list to multispec output
 
procedure sa_ms (list1, list2, output, op, opstr, w1, w2, rebin,
	aps, bands, beams, complement, apmod, offset, reverse, ignoreaps,
	clobber, merge, renumber, verbose)

int	list1			# Input image list
int	list2			# Input image list
char	output[ARB]		# Output image
int	op			# Operation
char	opstr[ARB]		# Operation string
double	w1			# Starting wavelength
double	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
pointer	aps			# Apertures/columns/lines
pointer	bands			# Bands
pointer	beams			# Beams
bool	complement		# Complement aperture/beam selection
int	apmod			# Aperture modulus
int	offset			# Offset to add to output aperture numbers
bool	reverse			# Reverse order of operands
bool	ignoreaps		# Ignore apertures?
bool	clobber			# Clobber existing image?
bool	merge			# Merge with existing image?
bool	renumber		# Renumber apertures?
bool	verbose			# Verbose output?

bool	select, same
real	aplow[2], aphigh[2]
double	l1, dl, a, b, w, wb, dw, z, p1, p2, p3
int	i, j, k, l, nin
int	ap, beam, dtype, nw, err
int	ninaps, noutaps, naps, npts, nbands, mwoutdim
int	last, op1, axis[3]
pointer	ptr, in1, in2, out, outtmp, mwtmp, mwin1, mwin2, mwout
pointer	sh1, sh2, shout, const, coeff, inaps, outaps
pointer	sp, str, str1, key, input1, input2, temp, ltm1, ltv1, ltm2, ltv2

double	shdr_lw()
int	imaccess(), ctod()
int	imtlen(), imtgetim(), imgnfn()
bool	strne(), rng_elementi(), fp_equald()
pointer	immap() , imgl3r(), impl3r(), imofnlu()
pointer	smw_openim(), mw_open()
errchk	immap, smw_openim, mw_open, imunmap, imgstr, imdelete
errchk	imgl3r, impl3r
errchk	shdr_open, sa_sextract
data	axis/1,2,3/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm1, 3*3, TY_DOUBLE)
	call salloc (ltv1, 3, TY_DOUBLE)
	call salloc (ltm2, 3*3, TY_DOUBLE)
	call salloc (ltv2, 3, TY_DOUBLE)
	call malloc (coeff, 1, TY_CHAR)
	const = NULL

	# Initialize.
	Memc[input2] = EOS
	in1 = NULL; in2 = NULL; out = NULL; outtmp=NULL; mwtmp = NULL
	mwin1 = NULL; mwin2 = NULL; mwout = NULL
	sh1 = NULL; sh2 = NULL; shout = NULL
	ninaps = 0; noutaps = 0; nbands = 0
	l1 = 1.; dl = 1.
	err = NO

	iferr {
	# Check for existing output image and abort if clobber is not set.
	if (imaccess (output, READ_ONLY) == YES) {
	    if (!clobber) {
		call sprintf (Memc[str], SZ_LINE,
		    "Output spectrum %s already exists")
		    call pargstr (output)
		call error (1, Memc[str])
	    } else if (merge) {
		# Open the output and check the type.
		ptr = immap (output, READ_ONLY, 0); out = ptr
		ptr = smw_openim (out); mwout = ptr
		if (SMW_FORMAT(mwout) == SMW_ND) {
		    call sprintf (Memc[str], SZ_LINE, "%s - Wrong format")
			call pargstr (output)
		    call error (1, Memc[str])
		}

		# Determine existing apertures and renumber them if needed
		noutaps = SMW_NSPEC(mwout)
		nbands = SMW_NBANDS(mwout)
		call salloc (outaps, noutaps, TY_INT)
		do i = 1, noutaps {
		    call shdr_open (out, mwout, i, 1, INDEFI, SHHDR, sh2)
		    if (renumber)
			Memi[outaps+i-1] = i + offset
		    else
			Memi[outaps+i-1] = AP(sh2)
		}
	    }
	    call mktemp ("temp", Memc[temp], SZ_FNAME)
	} else
	    call strcpy (output, Memc[temp], SZ_FNAME)

	# Open input list.  Determine the number of final output apertures
	# and maximum length in order to set the output dimensions.  Check
	# also that there is data to copy.

	call imtrew (list1)
	nin = imtlen (list1)
	npts = 0
	naps = noutaps
	while (imtgetim (list1, Memc[input1], SZ_FNAME) != EOF) {
	    iferr {
	    in1 = NULL
	    mwin1 = NULL

	    ptr = immap (Memc[input1], READ_ONLY, 0); in1 = ptr
	    ptr = smw_openim (in1); mwin1 = ptr

	    j = 1
	    if (SMW_FORMAT(mwin1) != SMW_ND) {
		j = 0
		do i = 1, SMW_NBANDS(mwin1) {
		    select = rng_elementi (bands, i)
		    if (!select)
			next
		    j = j + 1
		}
		if (j == 0)
		    call error (1, "No bands selected in image")
	    }
	    nbands = max (j, nbands)

	    do i = 1, SMW_NSPEC(mwin1) {
		call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)
		ap = AP(sh1)
		if (SMW_FORMAT(mwin1) == SMW_ND) {
		    call smw_mw (mwin1, i, 1, ptr, j, k)
		    select = rng_elementi (aps, j) && rng_elementi (bands, k)
		} else {
		    j = ap
		    if (apmod > 1)
			j = mod (j, apmod)
		    select = rng_elementi (aps, j)
		}

		select = select && rng_elementi (beams, BEAM(sh1))
		if ((complement && select) || (!complement && !select))
		    next
		if (renumber)
		    ap = naps + 1
		ap = ap + offset
		for (j=0; j<noutaps && Memi[outaps+j]!=ap; j=j+1)
		    ;
		if (j == noutaps)
		    naps = naps + 1
		if (ninaps == 0)
		    call malloc (inaps, 10, TY_INT)
		else if (mod (ninaps, 10) == 0)
		    call realloc (inaps, ninaps+10, TY_INT)
		Memi[inaps+ninaps] = ap

		call sa_sextract (sh1, w1, w2, rebin, dtype, w, dw, nw)
		if (ninaps == 0) {
		    l1 = w
		    dl = dw
		    same = true
		}
		if (same && !(fp_equald (w, l1) && fp_equald (dw, dl))) {
		    l1 = 1.
		    dl = 1.
		    same = false
		}

		npts = max (npts, nw+NP1(sh1)-1)
		ninaps = ninaps + 1
		if (Memc[input2] == EOS)
		    call strcpy (Memc[input1], Memc[input2], SZ_FNAME)
	    }
	    } then
		call erract (EA_WARN)

	    if (nin > 1) {
		call shdr_close (sh1)
		call smw_close (mwin1)
		if (in1 != NULL)
		    call imunmap (in1)
	    }
	}

	# Check the selected apertures.
	if (ninaps == 0)
	    call error (1, "No spectra selected")
	for (i=0; i<ninaps-1; i=i+1) {
	    for (j=i+1; j<ninaps; j=j+1) {
		if (Memi[inaps+i] == Memi[inaps+j]) {
		    call error (1,
			"Output spectra cannot have the same aperture number.\n\tUse renumber parameter.")
		}
	    }
	}

	# Set output image dimensions and WCS.  The WCS preserves the
	# dispersion axis physical coordinates but resets the aperture
	# axis physical coordinates.

	if (out != NULL) {
	    ptr = immap (Memc[temp], NEW_COPY, out); outtmp = ptr
	    if (IM_PIXTYPE(outtmp) != TY_DOUBLE)
		IM_PIXTYPE(outtmp) = TY_REAL

	    IM_LEN(outtmp,1) = max (npts, IM_LEN(out,1))
	    IM_LEN(outtmp,2) = naps
	    IM_LEN(outtmp,3) = max (nbands, IM_LEN(out,3))
	    if (nbands > 1)
		IM_NDIM(outtmp) = 3
	    else if (naps > 1)
		IM_NDIM(outtmp) = 2
	    else
		IM_NDIM(outtmp) = 1

	    l1 = 1.
	    dl = 1.
	    i = SMW_PDIM(MW(sh2))
	    j = SMW_PAXIS(MW(sh2),1)
	    mwoutdim = IM_NDIM(outtmp)

	    mwtmp = mw_open (NULL, mwoutdim)
	    call mw_newsystem (mwtmp, "equispec", mwoutdim)
	    call mw_swattrs (SMW_MW(mwout,0), 0, "sformat", "equispec")
	    call mw_swtype (mwtmp, axis, mwoutdim, "linear", "")
	    if (LABEL(sh2) != EOS)
		call mw_swattrs (mwtmp, 1, "label", LABEL(sh2))
	    if (UNITS(sh2) != EOS)
		call mw_swattrs (mwtmp, 1, "units", UNITS(sh2))
	    ifnoerr (call mw_gwattrs (SMW_MW(MW(sh2),0), SMW_PAXIS(MW(sh2),1),
		"units_display", Memc[str], SZ_LINE))
		call mw_swattrs (mwtmp, 1, "units_display", Memc[str])

	    call mw_gltermd (SMW_MW(mwout,0), Memd[ltm1], Memd[ltv1], i)
	    call mw_gltermd (mwtmp, Memd[ltm2], Memd[ltv2], mwoutdim)
	    Memd[ltm2] = dl * Memd[ltm1+(i+1)*(j-1)]
	    Memd[ltv2] = (Memd[ltv1+(j-1)] - l1) / dl + 1
	    call mw_sltermd (mwtmp, Memd[ltm2], Memd[ltv2], mwoutdim)
	    call smw_open (mwtmp, NULL, outtmp)

	    do i = 1, noutaps {
		call smw_gwattrs (mwout, i, 1, ap, beam, dtype,
		    w, dw, nw, z, aplow, aphigh, coeff)
		call smw_swattrs (mwtmp, i, 1, Memi[outaps+i-1], beam, dtype,
		    w, dw, nw, z, aplow, aphigh, Memc[coeff])
	    }

	    do j = 1, IM_LEN(out,3) {
		do i = 1, IM_LEN(out,2) {
		    ptr = impl3r (outtmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(outtmp,1))
		    call amovr (Memr[imgl3r(out,i,j)], Memr[ptr], IM_LEN(out,1))
		    if (verbose) {
			call shdr_open (out, mwout, i, j, INDEFI, SHHDR, sh2)
			call shdr_open (outtmp, mwtmp, i, j, INDEFI,
			    SHHDR, shout)
			if (AP(sh2) != AP(shout))
			    call sa_verbose (sh2, NULL, shout, output,
				COPY, "copy", const, reverse)
		    }
		}
	    }
	    do j = 1, IM_LEN(out,3)
		do i = IM_LEN(out,2)+1, IM_LEN(outtmp,2) {
		    ptr = impl3r (outtmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(outtmp,1))
		}
	    do j = IM_LEN(out,3)+1, nbands
		do i = 1, IM_LEN(outtmp,2) {
		    ptr = impl3r (outtmp, i, j)
		    call aclrr (Memr[ptr], IM_LEN(outtmp,1))
		}

	    call shdr_close (shout)
	    call shdr_close (sh2)
	    call smw_close (mwout)
	    mwout = mwtmp
	    mwtmp = NULL
	    call imunmap (out)
	    out = outtmp
	    outtmp = NULL

	} else {
	    if (nin > 1) {
		ptr = immap (Memc[input2], READ_ONLY, 0); in1 = ptr
		ptr = smw_openim (in1); mwin1 = ptr
		call shdr_open (in1, mwin1, i, 1, INDEFI, SHDATA, sh1)
	    }
	    ptr = immap (Memc[temp], NEW_COPY, in1); out = ptr
	    if (IM_PIXTYPE(out) != TY_DOUBLE)
		IM_PIXTYPE(out) = TY_REAL
	    ifnoerr (call imgstr (out, "MSTITLE", Memc[str], SZ_LINE)) {
		call strcpy (Memc[str], IM_TITLE(out), SZ_IMTITLE)
		call imdelf (out, "MSTITLE")
	    }

	    # Set header
	    IM_LEN(out,1) = npts
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

	    i = SMW_PDIM(MW(sh1))
	    j = SMW_PAXIS(MW(sh1),1)

	    ptr = mw_open (NULL, mwoutdim); mwout = ptr
	    call mw_newsystem (mwout, "equispec", mwoutdim)
	    call mw_swattrs (mwout, 0, "sformat", "equispec")
	    call mw_swtype (mwout, axis, mwoutdim, "linear", "")
	    if (LABEL(sh1) != EOS)
		call mw_swattrs (mwout, 1, "label", LABEL(sh1))
	    if (UNITS(sh1) != EOS)
		call mw_swattrs (mwout, 1, "units", UNITS(sh1))
	    ifnoerr (call mw_gwattrs (SMW_MW(MW(sh1),0), SMW_PAXIS(MW(sh1),1),
		"units_display", Memc[str], SZ_LINE))
		call mw_swattrs (mwout, 1, "units_display", Memc[str])

	    call mw_gltermd (SMW_MW(mwin1,0), Memd[ltm1], Memd[ltv1], i)
	    call mw_gltermd (mwout, Memd[ltm2], Memd[ltv2], mwoutdim)
	    Memd[ltv2] = (Memd[ltv1+(j-1)] - l1) / dl + 1
	    Memd[ltm2] = dl * Memd[ltm1+(i+1)*(j-1)]
	    call mw_sltermd (mwout, Memd[ltm2], Memd[ltv2], mwoutdim)
	    call smw_open (mwout, NULL, out)

	    if (nin > 1) {
		call shdr_close (sh1)
		call smw_close (mwin1)
		call imunmap (in1)
	    }
	}

	# Now do the actual copy
	last = noutaps
	call imtrew (list1)
	call imtrew (list2)
	while (imtgetim (list1, Memc[input1], SZ_FNAME) != EOF) {
	    i = imtgetim (list2, Memc[input2], SZ_FNAME)
	    iferr {
	    if (nin > 1) {
		in1 = NULL
		mwin1 = NULL

		ptr = immap (Memc[input1], READ_ONLY, 0); in1 = ptr
		ptr = smw_openim (in1); mwin1 = ptr
		call shdr_open (in1, mwin1, 1, 1, INDEFI, SHHDR, sh1)
	    }

	    # Check dispersion function compatibility
	    # Nonlinear functions can't be copied to different physical
	    # coordinate system though the linear dispersion can be
	    # adjusted.

	    call mw_gltermd (SMW_MW(mwout,0), Memd[ltm2], Memd[ltv2],
		mwoutdim)
	    a = Memd[ltv2]
	    b = Memd[ltm2]
	    if (DC(sh1) == DCFUNC && !rebin) {
		i = SMW_PDIM(mwin1)
		j = SMW_PAXIS(mwin1,1)

		call mw_gltermd (SMW_MW(mwin1,0), Memd[ltm1], Memd[ltv1], i)
		Memd[ltv1] = (Memd[ltv1+(j-1)] - l1) / dl + 1
		Memd[ltm1] = dl * Memd[ltm1+(i+1)*(j-1)]
		if (!fp_equald (a, Memd[ltv1]) || !fp_equald (b, Memd[ltm1])) {
		    call error (1,
		"Physical basis for nonlinear dispersion functions don't match")
		}
	    }

	    # Check for second operand
	    if (op > SQRT) {
		ifnoerr (ptr = immap (Memc[input2], READ_ONLY, 0)) {
		    in2 = ptr
		    sh2 = NULL
		    mwin2 = NULL
		    ptr = smw_openim (in2); mwin2 = ptr
		    call shdr_open (in2, mwin2, 1, 1, INDEFI, SHHDR, sh2)
		} else {
		    const = NULL
		    i = 1
		    if (ctod (Memc[input2], i, w) <= 0)
			call error (1, "Error in second operand")
		    call malloc (const, IM_LEN(out,1), TY_REAL)
		    call amovkr (real (w), Memr[const], IM_LEN(out,1))
		}
	    }

	    do i = 1, SMW_NSPEC(mwin1) {
		call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)
		ap = AP(sh1)
		if (SMW_FORMAT(mwin1) == SMW_ND) {
		    call smw_mw (mwin1, i, 1, ptr, j, k)
		    select = rng_elementi (aps, j) && rng_elementi (bands, k)
		} else {
		    j = ap
		    if (apmod > 1)
			j = mod (j, apmod)
		    select = rng_elementi (aps, j)
		}

		select = select && rng_elementi (beams, BEAM(sh1))
		if ((complement && select) || (!complement && !select))
		    next
		if (renumber)
		    ap = last + 1
		ap = ap + offset
		for (j=0; j<noutaps && Memi[outaps+j]!=ap; j=j+1)
		    ;
		if (j < noutaps)
		    l = j + 1
		else {
		    l = last + 1
		    last = l
		}

		call shdr_open (in1, mwin1, i, 1, INDEFI, SHDATA, sh1)
		call sa_sextract (sh1, w1, w2, rebin, dtype, w, dw, nw)

		# Copy and adjust dispersion info
		call smw_gwattrs (mwin1, i, 1, AP(sh1), beam,
		    j, w, dw, nw, z, aplow, aphigh, coeff)

		w = shdr_lw (sh1, 1D0)
		wb = shdr_lw (sh1, double (SN(sh1)))
		if (rebin)
		    Memc[coeff] = EOS

		p1 = (NP1(sh1) - a) / b
		p2 = (NP2(sh1) - a) / b
		p3 = (IM_LEN(out,1) - a) / b
		nw = nint (min (max (p1, p3), max (p1, p2))) + NP1(sh1) - 1

		w = w * (1 + z)
		wb = wb * (1 + z)
		if (dtype == DCLOG) {
		    w = log10 (w)
		    wb = log10 (wb)
		    if (p1 != p2)
			dw = (wb - w) / (p2 - p1)
		    w = w - (p1 - 1) * dw
		    wb = w + (nw - 1) * dw
		    w = 10.**w
		    wb = 10.**wb
		    dw = (wb - w) / (nw - 1)
		} else {
		    if (p1 != p2)
			dw = (wb - w) / (p2 - p1)
		    w = w - (p1 - 1) * dw
		    wb = w + (nw - 1) * dw
		}

		call smw_swattrs (mwout, l, 1, ap, beam, dtype,
		    w, dw, nw, z, aplow, aphigh, Memc[coeff])

		# Copy title
		call smw_sapid (mwout, l, 1, TITLE(sh1))

		# Copy the data
		op1 = op
		k = 0
		do j = 1, SMW_NBANDS(mwin1) {
		    if (SMW_FORMAT(mwin1) != SMW_ND) {
			select = rng_elementi (bands, j)
			if (!select)
			    next
		    }
		    k = k + 1
		    if (j != 1) {
			call shdr_open (in1, mwin1, i, j, INDEFI, SHDATA, sh1)
			call sa_sextract (sh1, w1, w2, rebin, dtype, w, dw,nw)
		    }

		    if (Memc[SID(sh1,1)] != EOS) {
			call sprintf (Memc[key], SZ_LINE, "BANDID%d")
			    call pargi (k)
			iferr (call imgstr (out, Memc[key], Memc[str], SZ_LINE))
			    call imastr (out, Memc[key], Memc[SID(sh1,1)])
			else {
			    if (strne (Memc[SID(sh1,1)], Memc[str]))
				call eprintf (
			    "Warning: Input and output types (BANDID) differ\n")
			}
		    }
			    
		    if (sh2 != NULL) {
			if (ignoreaps)
			    call shdr_open (in2, mwin2, i, j, INDEFI,
				SHDATA, sh2)
			else {
			    call shdr_open (in2, mwin2, i, j, AP(sh1),
				SHDATA, sh2)
			    if (AP(sh2) != AP(sh1))
				op1 = COPY
			}
		    }

		    # For now just copy noise band.
		    if (STYPE(sh1,1) == SHSIG)
			op1 = COPY

		    call sa_arith (op1, sh1, sh2, const, reverse,
			Memr[SY(sh1)], Memr[impl3r(out,l,k)+NP1(sh1)-1],SN(sh1))

		    if (verbose) {
			call shdr_open (out, mwout, l, k, INDEFI, SHHDR, shout)
			call sa_verbose (sh1, sh2, shout, output,
			    op1, opstr, const, reverse)
			call shdr_close (shout)
		    }
		}
		do j = k+1, IM_LEN(out,3)
		    call aclrr (Memr[impl3r(out,l,j)], IM_LEN(out,1))
	    }
	    } then
		call erract (EA_WARN)

	    call shdr_close (shout)
	    call shdr_close (sh1)
	    call shdr_close (sh2)
	    call mfree (const, TY_REAL)
	    call smw_close (mwin2)
	    call smw_close (mwin1)
	    if (in2 != NULL)
		call imunmap (in2)
	    if (in1 != NULL)
		call imunmap (in1)
	}
	} then {
	    err = YES
	    call erract (EA_WARN)
	}

	# Finish up the output image.
	if (mwout != NULL) {
	    call smw_saveim (mwout, out)
	    call smw_close (mwout)
	}
	if (outtmp != NULL)
	    call imunmap (outtmp)
	call smw_close (mwtmp)
	if (out != NULL) {
	    call imunmap (out)
	    if (strne (Memc[temp], output)) {
		if (err == NO) {
		    call imdelete (output)
		    call imrename (Memc[temp], output)
		} else {
		    iferr (call imdelete (Memc[temp]))
			;
		}
	    }
	}

	call mfree (inaps, TY_INT)
	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end

 
# SA_1D -- Operate on input list to onedspec output.

procedure sa_1d (list1, list2, output, op, opstr, w1, w2, rebin,
	aps, bands, beams, complement, apmod, offset, reverse, ignoreaps,
	clobber, renumber, verbose)

int	list1			# Input image list
int	list2			# Input image list
char	output[ARB]		# Output image
int	op			# Operation
char	opstr[ARB]		# Operation string
double	w1			# Starting wavelength
double	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
pointer	aps			# Apertures/columns/lines
pointer	bands			# Bands
pointer	beams			# Beams
bool	complement		# Complement aperture/beam selection
int	apmod			# Aperture modulus
int	offset			# Offset to add to output aperture numbers
bool	reverse			# Reverse order of operands
bool	ignoreaps		# Ignore apertures?
bool	clobber			# Clobber existing image?
bool	renumber		# Renumber apertures?
bool	verbose			# Verbose output?

bool	select
int	i, j, k
int	ap, band, beam, dtype, nw, naps, op1, err
double	w, wb, dw, z, p1, p2, p3
real	aplow[2], aphigh[2]
pointer	ptr, in1, in2, out, mwin1, mwin2, mwout, sh1, sh2, shout
pointer	sp, str, key, input1, input2, output1, temp
pointer	ltm1, ltv1, ltm2, ltv2, coeff, const

double	shdr_lw()
int	imaccess(), ctod(), patmake(), patmatch()
int	imtgetim(), imgnfn()
bool	rng_elementi(), streq()
pointer	immap(), impl1r(), imofnlu(), smw_openim(), mw_open()
errchk	immap, smw_openim, mw_open, imunmap, impl1r, imdelete
errchk	shdr_open, sa_sextract

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (output1, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm1, 3*3, TY_DOUBLE)
	call salloc (ltv1, 3, TY_DOUBLE)
	call salloc (ltm2, 3*3, TY_DOUBLE)
	call salloc (ltv2, 3, TY_DOUBLE)
	call malloc (coeff, 1, TY_CHAR)

	# Loop through each spectrum in each input image.
	call imtrew (list1)
	call imtrew (list2)
	sh1 = NULL; sh2 = NULL; shout = NULL
	naps = 0
	while (imtgetim (list1, Memc[input1], SZ_FNAME) != EOF) {
	    i = imtgetim (list2, Memc[input2], SZ_FNAME)

	    iferr {
	    in1 = NULL; in2 = NULL; mwin1 = NULL; mwin2 = NULL
	    ptr = immap (Memc[input1], READ_ONLY, 0); in1 = ptr
	    ptr = smw_openim (in1); mwin1 = ptr

	    # Check for second operand
	    if (op > SQRT) {
		ifnoerr (ptr = immap (Memc[input2], READ_ONLY, 0)) {
		    in2 = ptr
		    sh2 = NULL
		    mwin2 = NULL
		    ptr = smw_openim (in2); mwin2 = ptr
		    call shdr_open (in2, mwin2,1,1,INDEFI,SHHDR,sh2)
		} else {
		    const = NULL
		    i = 1
		    if (ctod (Memc[input2], i, w) <= 0)
			call error (1, "Error in second operand")
		    call malloc (const, IM_LEN(in1,1), TY_REAL)
		    call amovkr (real (w), Memr[const], IM_LEN(in1,1))
		}
	    }

	    do band = 1, SMW_NBANDS(mwin1) {
		if (SMW_FORMAT(mwin1) != SMW_ND) {
		    select = rng_elementi (bands, band)
		    if (!select)
			next
		}
		do i = 1, SMW_NSPEC(mwin1) {
		    call shdr_open (in1, mwin1, i, band, INDEFI, SHHDR, sh1)

		    # Check aperture and beam numbers.
		    ap = AP(sh1)
		    if (SMW_FORMAT(mwin1) == SMW_ND) {
			call smw_mw (mwin1, i, band, ptr, j, k)
			select = rng_elementi (aps,j) && rng_elementi (bands,k)
		    } else {
			j = ap
			if (apmod > 1)
			    j = mod (j, apmod)
			select = rng_elementi (aps, j)
		    }

		    select = select && rng_elementi (beams, BEAM(sh1))
		    if ((complement && select) || (!complement && !select))
			next
		    if (renumber) {
			naps = naps + 1
			ap = naps
		    }
		    ap = ap + offset

		    iferr {
		    out = NULL
		    mwout = NULL
		    err = NO

		    # Open output spectrum
		    call strcpy (output, Memc[str], SZ_LINE)
		    j = patmake (".[0-9][0-9][0-9][0-9]$", Memc[key],
			SZ_LINE)
		    j = patmatch (Memc[str], Memc[key])
		    if (j > 0)
			Memc[str+j-6] = EOS
		    if (SMW_FORMAT(mwin1) != SMW_ND) {
			call sprintf (Memc[output1], SZ_FNAME,
			    "%s.%d%03d")
			    call pargstr (Memc[str])
			    call pargi (PINDEX(sh1,2)-1)
			    call pargi (ap)
		    } else {
			call sprintf (Memc[output1], SZ_FNAME,
			    "%s.%04d")
			    call pargstr (Memc[str])
			    call pargi (ap)
		    }
		    if (imaccess (Memc[output1], READ_ONLY) == YES) {
			if (clobber)
			    call mktemp ("temp", Memc[temp], SZ_FNAME)
			else {
			    call sprintf (Memc[str], SZ_LINE,
				"Output spectrum %s already exists")
				call pargstr (output)
			    call error (1, Memc[str])
			}
		    } else
			call strcpy (Memc[output1], Memc[temp],
			    SZ_FNAME)

		    # Get data
		    call shdr_open (in1, mwin1, i, band, INDEFI, SHDATA,
			sh1)

		    # Set header
		    ptr = immap (Memc[temp], NEW_COPY, in1); out = ptr
		    if (IM_PIXTYPE(out) != TY_DOUBLE)
			IM_PIXTYPE(out) = TY_REAL
		    IM_NDIM(out) = 1
		    if (!streq (TITLE(sh1), IM_TITLE(out))) {
			call imastr (out, "MSTITLE", IM_TITLE(out))
			call strcpy (TITLE(sh1), IM_TITLE(out),
			    SZ_IMTITLE)
		    }
		    j = imofnlu (out, "DISPAXIS,APID*,BANDID*")
		    while (imgnfn (j, Memc[key], SZ_LINE) != EOF)
			call imdelf (out, Memc[key])
		    call imcfnl (j)

		    if (Memc[SID(sh1,1)] != EOS)
			call imastr (out, "BANDID1", Memc[SID(sh1,1)])

		    # Set WCS
		    j = SMW_PDIM(MW(sh1))
		    k = SMW_PAXIS(MW(sh1),1)
		    ptr = mw_open (NULL, 1); mwout = ptr
		    call mw_newsystem (mwout, "equispec", 1)
		    call mw_swattrs (mwout, 0, "sformat", "equispec")
		    call mw_swtype (mwout, 1, 1, "linear", "")
		    if (LABEL(sh1) != EOS)
			call mw_swattrs (mwout, 1, "label", LABEL(sh1))
		    if (UNITS(sh1) != EOS)
			call mw_swattrs (mwout, 1, "units", UNITS(sh1))
		    ifnoerr (call mw_gwattrs (SMW_MW(MW(sh1),0),
			SMW_PAXIS(MW(sh1),1), "units_display",
			Memc[str], SZ_LINE))
			call mw_swattrs (mwout, 1, "units_display", Memc[str])

		    call mw_gltermd (SMW_MW(mwin1,0), Memd[ltm1],
			Memd[ltv1], j)
		    call mw_gltermd (mwout, Memd[ltm2], Memd[ltv2], 1)
		    Memd[ltv2] = Memd[ltv1+(k-1)]
		    Memd[ltm2] = Memd[ltm1+(j+1)*(k-1)]
		    call sa_sextract (sh1, w1, w2, rebin, dtype, w, dw, nw)
		    IM_LEN(out,1) = nw + NP1(sh1) - 1
		    Memd[ltv2] = (Memd[ltv1] - w) / dw + 1
		    Memd[ltm2] = dw * Memd[ltm1]
		    call mw_sltermd (mwout, Memd[ltm2], Memd[ltv2], 1)
		    call smw_open (mwout, NULL, out)

		    # Copy and adjust dispersion info
		    call smw_gwattrs (mwin1, i, band, AP(sh1),
			beam, j, w, dw, nw, z, aplow, aphigh, coeff)
		    w = shdr_lw (sh1, 1D0)
		    wb = shdr_lw (sh1, double(SN(sh1)))
		    if (rebin)
			Memc[coeff] = EOS

		    p1 = (NP1(sh1) - Memd[ltv2]) / Memd[ltm2]
		    p2 = (NP2(sh1) - Memd[ltv2]) / Memd[ltm2]
		    p3 = (IM_LEN(out,1) - Memd[ltv2]) / Memd[ltm2]
		    nw = nint (min (max (p1, p3), max (p1, p2))) + NP1(sh1) - 1

		    w = w * (1 + z)
		    wb = wb * (1 + z)
		    if (dtype == DCLOG) {
			w = log10 (w)
			wb = log10 (wb)
			if (p1 != p2)
			    dw = (wb - w) / (p2 - p1)
			w = w - (p1 - 1) * dw
			wb = w + (nw - 1) * dw
			w = 10.**w
			wb = 10.**wb
			dw = (wb - w) / (nw - 1)
		    } else {
			if (p1 != p2)
			    dw = (wb - w) / (p2 - p1)
			w = w - (p1 - 1) * dw
			wb = w + (nw - 1) * dw
		    }

		    call smw_swattrs (mwout, 1, 1, ap, beam, dtype,
			w, dw, nw, z, aplow, aphigh, Memc[coeff])

		    # Copy data
		    op1 = op
		    if (sh2 != NULL) {
			if (ignoreaps)
			    call shdr_open (in2, mwin2, i, band, INDEFI,
				SHDATA, sh2)
			else {
			    call shdr_open (in2, mwin2, i, band, AP(sh1),
				SHDATA, sh2)
			    if (AP(sh2) != AP(sh1))
				op1 = COPY
			}
		    }

		    # For now just copy noise band.
		    if (STYPE(sh1,1) == SHSIG)
			op1 = COPY

		    call sa_arith (op1, sh1, sh2, const, reverse,
			Memr[SY(sh1)], Memr[impl1r(out)+NP1(sh1)-1], SN(sh1))


		    if (verbose) {
			call shdr_open (out, mwout, 1, 1, INDEFI, SHHDR, shout)
			call sa_verbose (sh1, sh2, shout, Memc[output1],
			    op1, opstr, const, reverse)
		    }
		    } then {
			err = YES
			call erract (EA_WARN)
		    }

		    call shdr_close (shout)
		    if (mwout != NULL) {
			if (err == NO)
			    call smw_saveim (mwout, out)
			call smw_close (mwout)
		    }
		    if (out != NULL) {
			call imunmap (out)
			if (!streq (Memc[output1], Memc[temp])) {
			    if (err == NO) {
				call imgimage (Memc[input1], Memc[str], SZ_LINE)
				if (streq (Memc[output1], Memc[str]))
				    call imunmap (in1)
				call imgimage (Memc[input2], Memc[str], SZ_LINE)
				if (streq (Memc[output1], Memc[str]))
				    call imunmap (in2)
				call imdelete (Memc[output1])
				call imrename (Memc[temp], Memc[output1])
			    } else
				call imdelete (Memc[temp])
			} else if (err == YES)
			    call imdelete (Memc[output1])
		    }
		}
	    }
	    } then
		call erract (EA_WARN)

	    call shdr_close (sh2)
	    call shdr_close (sh1)
	    call smw_close (mwin1)
	    call smw_close (mwin2)
	    if (in2 != NULL)
		call imunmap (in2)
	    if (in1 != NULL)
		call imunmap (in1)
	}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SA_ARITH -- Do arithmetic operation

procedure sa_arith (op, sh, sh2, const, reverse, in, out, n)

int	op			# Operation
pointer	sh			# Input SHDR pointer
pointer	sh2			# Second operand spectrum (NULL if none)
pointer	const			# Second operand constant (NULL if none)
bool	reverse			# Reverse order of operands
real	in[n]			# Input data
real	out[n]			# Output data
int	n			# Number of data points

int	i
pointer	buf
real	sa_errfcn()
extern	sa_errfcn()

begin
	if (op > SQRT) {
	    if (sh2 != NULL) {
		call shdr_rebin (sh2, sh)
		buf = SY(sh2)
	    } else
		buf = const
	}

	switch (op) {
	case ABS:
	    call aabsr (in, out, n)
	case COPY:
	    call amovr (in, out, n)
	case DEX:
	    do i = 1, n
		out[i] = 10. ** in[i]
	case EXP:
	    do i = 1, n
		out[i] = exp (in[i])
	case FLAM:
	    buf = SX(sh)
	    do i = 1, n {
		out[i] = in[i] / (Memr[buf] ** 2 / 2.997925e18)
		buf = buf + 1
	    }
	case FNU:
	    buf = SX(sh)
	    do i = 1, n {
		out[i] = in[i] * (Memr[buf] ** 2 / 2.997925e18)
		buf = buf + 1
	    }
	case INV:
	    call arczr (1., in, out, n, sa_errfcn)
	case LN:
	    call allnr (in, out, n, sa_errfcn)
	case LOG:
	    call alogr (in, out, n, sa_errfcn)
	case LUM:
	    do i = 1, n
		out[i] = 10. ** (-0.4 * in[i])
	case MAG:
	    do i = 1, n {
		if (in[i] <= 0.)
		    out[i] = sa_errfcn (0.)
		else
		    out[i] = -2.5 * log10 (in[i])
	    }
	case SQRT:
	    call asqrr (in, out, n, sa_errfcn)

	case REP:
	    call amovr (Memr[buf], out, n)
	case ADD:
	    call aaddr (in, Memr[buf], out, n)
	case SUB:
	    if (reverse)
		call asubr (Memr[buf], in, out, n)
	    else
		call asubr (in, Memr[buf], out, n)
	case MUL:
	    call amulr (in, Memr[buf], out, n)
	case DIV:
	    if (reverse)
		call advzr (Memr[buf], in, out, n, sa_errfcn)
	    else
		call advzr (in, Memr[buf], out, n, sa_errfcn)
	case POW:
	    if (reverse) {
		do i = 1, n
		    out[i] = Memr[buf+i-1] ** in[i]
	    } else {
		do i = 1, n
		    out[i] = in[i] ** Memr[buf+i-1]
	    }
	}
end


# SA_ERRFCN -- SARITH Error Function

real procedure sa_errfcn (x)

real	x, errval
common	/sarith/ errval

begin
	return (errval)
end


# SA_VERBOSE -- Print verbose output.

procedure sa_verbose1 (input1, input2, output, ap1, ap2, apout, op, opstr,
	const, reverse)

char	input1[ARB], input2[ARB]	# Input spectra
char	output[ARB]			# Output spectrum
int	ap1, ap2			# Input apertures
int	apout				# Output apertures
int	op				# Opcode
char	opstr[ARB]			# Operation string
pointer	const				# Pointer to constant if used
bool	reverse				# Reverse operands?

begin
	if (op <= SQRT) {
	    if (op == COPY) {
		call printf ("%s[%d]  -->  %s")
		    call pargstr (input1)
		    call pargi (ap1)
		    call pargstr (output)
		    call pargi (apout)
	    } else {
		call printf ("%s[%d]  -- %s -->  %s")
		    call pargstr (input1)
		    call pargi (ap1)
		    call pargstr (opstr)
		    call pargstr (output)
		    call pargi (apout)
	    }
	} else if (const == NULL) {
	    call printf ("%s[%d]  %s  %s[%d]  -->  %s")
		if (reverse) {
		    call pargstr (input2)
		    call pargi (ap2)
		    call pargstr (opstr)
		    call pargstr (input1)
		    call pargi (ap1)
		} else {
		    call pargstr (input1)
		    call pargi (ap1)
		    call pargstr (opstr)
		    call pargstr (input2)
		    call pargi (ap2)
		}
		call pargstr (output)
		call pargi (apout)
	} else {
	    if (reverse) {
		call printf ("%g  %s  %s[%d]  -->  %s")
		    call pargr (Memr[const])
		    call pargstr (opstr)
		    call pargstr (input1)
		    call pargi (ap1)
	    } else {
		call printf ("%s[%d]  %s  %g  -->  %s")
		    call pargstr (input1)
		    call pargi (ap1)
		    call pargstr (opstr)
		    call pargr (Memr[const])
	    }
	}
	call pargstr (output)
	if (!IS_INDEFI(apout)) { 
	    call printf ("[%d]")
		call pargi (apout)
	}
	call printf ("\n")
	call flush (STDOUT)
end


# SA_VERBOSE -- Print verbose output.

procedure sa_verbose (sh1, sh2, shout, output, op, opstr, const, reverse)

pointer	sh1, sh2			# Input spectra
pointer	shout				# Output spectrum
char	output[ARB]			# Output image name
int	op				# Opcode
char	opstr[ARB]			# Operation string
pointer	const				# Pointer to constant if used
bool	reverse				# Reverse operands?

begin
	if (op <= SQRT) {
	    if (op == COPY) {
		call printf ("%s%s(%d)  -->  %s%s(%d)")
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
	    } else {
		call printf ("%s%s(%d) -- %s  -->  %s%s(%d)")
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
		    call pargstr (opstr)
	    }
	} else if (const == NULL) {
	    call printf ("%s%s(%d)  %s  %s%s(%d)  -->  %s%s(%d)")
		if (reverse) {
		    call pargstr (IMNAME(sh2))
		    call pargstr (IMSEC(sh2))
		    call pargi (AP(sh2))
		    call pargstr (opstr)
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
		} else {
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
		    call pargstr (opstr)
		    call pargstr (IMNAME(sh2))
		    call pargstr (IMSEC(sh2))
		    call pargi (AP(sh2))
		}
	} else {
	    if (reverse) {
		call printf ("%g  %s  %s%s(%d)  -->  %s%s(%d)")
		    call pargr (Memr[const])
		    call pargstr (opstr)
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
	    } else {
		call printf ("%s%s(%d)  %s  %g  -->  %s%s(%d)")
		    call pargstr (IMNAME(sh1))
		    call pargstr (IMSEC(sh1))
		    call pargi (AP(sh1))
		    call pargstr (opstr)
		    call pargr (Memr[const])
	    }
	}
	call pargstr (output)
	call pargstr (IMSEC(shout))
	call pargi (AP(shout))
	call printf ("\n")
	call flush (STDOUT)
end


# SA_GETIM -- Get image from a list with the image kernal extension removed.

procedure sa_getim (list, defname, image, maxchar)

int	list		# Image list
char	defname[ARB]	# Default image name
char	image[maxchar]	# Image name
int	maxchar		# Image name maximum character length

int	i, stat, imtgetim(), strmatch()
pointer	sp, str, section

begin
	call smark (sp)
	call salloc (str, maxchar, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	stat = imtgetim (list, Memc[str], maxchar)
	if (stat == EOF)
	    call strcpy (defname, Memc[str], maxchar)

	call imgsection (Memc[str], Memc[section], SZ_FNAME)
	call imgimage (Memc[str], image, maxchar)
	i = strmatch (image, ".??h$")
	if (i > 0)
	    image[i-4] = EOS
	call strcat (Memc[section], image, maxchar)

	call sfree (sp)
end


# SA_SEXTRACT -- Extract a specific wavelength region

procedure sa_sextract (sh, w1, w2, rebin, dtype, l1, dl, n)

pointer	sh			#U SHDR structure
double	w1			#I Starting wavelength
double	w2			#I Ending wavelength
bool	rebin			#I Rebin wavelength region?
int	dtype			#O Dispersion type
double	l1			#O Starting logical pixel
double	dl			#O Logical pixel increment
int	n			#O Number of logical pixels

int	i1, i2
double	a, b
bool	fp_equald()
double	shdr_lw(), shdr_wl()
errchk	shdr_wl, shdr_linear, shdr_extract

begin
	if (IS_INDEFD(w1) && IS_INDEFD(w2)) {
	    l1 = 1.
	    dl = 1.
	    n = SN(sh)
	    dtype = DC(sh)
	    return
	}

	a = w1
	b = w2
	if (IS_INDEFD(a))
	    a = shdr_lw (sh, 1.0D0)
	if (IS_INDEFD(b))
	    b = shdr_lw (sh, double (SN(sh)))

	l1 = shdr_wl (sh, a)
	dl = shdr_wl (sh, b)
	if (fp_equald(l1,dl) || max(l1,dl) < 1. || min (l1,dl) > SN(sh))
	    call error (1, "No pixels to extract")
	l1 = max (1D0, min (double (SN(sh)), l1))
	dl = max (1D0, min (double (SN(sh)), dl))
	i1 = nint (l1)
	i2 = nint (dl)
	n = abs (i2 - i1) + 1
	if (!rebin) {
	    l1 = i1
	    dl = i2
	}
	if (n == 1)
	    dl = 1
	else
	    dl = (dl - l1) / (n - 1)

	if (SY(sh) != NULL)
	    call shdr_extract (sh, real(a), real(b), rebin)
	dtype = DC(sh)
end
