include	<error.h>
include	<imhdr.h>
include	<mach.h>
include	"shdr.h"
 
# Output formats
define	FORMATS		"|multispec|onedspec|"

# Operations
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


# T_SARITH -- Arithmetic operations on spectra
 
procedure t_sarith()
 
int	inlist1			# List of input spectra
int	op			# Operation
int	inlist2			# List of input spectra or operands
int	outlist			# List of output spectra
real	w1			# Starting wavelength
real	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
int	format			# Output format
pointer	aps			# Aperture list
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
 
int	i, list1, list2
pointer	sp, input1, opstr, input2, output, ptr
 
real	clgetr()
int	imtopenp(), imtopen(), sa_getim(), imtlen()
int	clgwrd(), clgeti(), decode_ranges()
bool	clgetb()
common	/sarith/ errval
 
begin
	call smark (sp)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (opstr, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (aps, 300, TY_INT)
	call salloc (beams, 300, TY_INT)
 
	# Get parameters
	inlist1 = imtopenp ("input1")
	op = clgwrd ("op", Memc[opstr], SZ_FNAME, OPS)
	if (op > SQRT)
	    inlist2 = imtopenp ("input2")
	else
	    inlist2 = imtopen ("")
	outlist = imtopenp ("output")

	w1 = clgetr ("w1")
	w2 = clgetr ("w2")
	rebin = clgetb ("rebin")
	if (IS_INDEF(w1) || IS_INDEF(w2)) {
	    w1 = INDEF
	    w2 = INDEF
	    rebin = false
	}

	format = clgwrd ("format", Memc[input1], SZ_FNAME, FORMATS)
	call clgstr ("apertures", Memc[input1], SZ_FNAME)
	call clgstr ("beams", Memc[output], SZ_FNAME)
	apmod = clgeti ("apmodulus")
	offset = clgeti ("offset")
	reverse = clgetb ("reverse")
	ignoreaps = clgetb ("ignoreaps")
	clobber = clgetb ("clobber")
	merge = clgetb ("merge")
	renumber = clgetb ("renumber")
	verbose = clgetb ("verbose")
	errval = clgetr ("errval")
 
	if (op == 0)
	    call error (1, "Unknown operation")

	# Decode range strings and set complement if needed
	ptr = input1
	complement = false
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	if (decode_ranges (Memc[ptr], Memi[aps], 100, i) == ERR)
	    call error (0, "Bad aperture/record list")

	ptr = output
	if (Memc[ptr] == '!') {
	    complement = true
	    ptr = ptr + 1
	}
	if (decode_ranges (Memc[ptr], Memi[beams], 100, i) == ERR)
	    call error (0, "Bad beam list")

	# Check lists.
	if (imtlen (outlist) > 1 && imtlen (outlist) != imtlen (inlist1))
	    call error (1, "Input and output image lists don't make sense")
	if (op > SQRT &&
	    imtlen (inlist2) > 1 && imtlen (inlist2) != imtlen (inlist1))
	    call error (1, "Input operand lists don't make sense")
 
	# Do the operations.
	while (sa_getim (inlist1, Memc[input1], SZ_FNAME) != EOF) {
	    if (sa_getim (inlist2, Memc[input2], SZ_FNAME) == EOF)
		;
	    if (sa_getim (outlist, Memc[output], SZ_FNAME) == EOF)
		call strcpy (Memc[input1], Memc[output], SZ_FNAME)
	    call imgimage (Memc[output], Memc[output], SZ_FNAME)

	    if (imtlen (outlist) > 1) {
		list1 = imtopen (Memc[input1])
		list2 = imtopen (Memc[input2])
	    } else {
		list1 = inlist1
		list2 = inlist2
	    }

	    switch (format) {
	    case 1:
		call sa_ms (list1, list2, Memc[output], op, Memc[opstr],
		    w1, w2, rebin, Memi[aps], Memi[beams], complement,
		    apmod, offset, reverse, ignoreaps, clobber, merge,
		    renumber, verbose)
	    case 2:
		call sa_1d (list1, list2, Memc[output], op, Memc[opstr],
		    w1, w2, rebin, Memi[aps], Memi[beams], complement,
		    apmod, offset, reverse, ignoreaps, clobber,
		    renumber, verbose)
	    }

	    if (list1 != inlist1) {
		call imtclose (list1)
		call imtclose (list2)
	    }
	}

	call imtclose (inlist1)
	call imtclose (inlist2)
	call imtclose (outlist)
	call sfree (sp)
end


# SA_MS -- Operate on input list to multispec output
 
procedure sa_ms (list1, list2, output, op, opstr, w1, w2, rebin,
	aps, beams, complement, apmod, offset, reverse, ignoreaps, clobber,
	merge, renumber, verbose)

int	list1			# Input image list
int	list2			# Input image list
char	output[ARB]		# Output image
int	op			# Operation
char	opstr[ARB]		# Operation string
real	w1			# Starting wavelength
real	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
int	aps[ARB]		# Apertures
int	beams[ARB]		# Beams
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
double	l1, dl, a, b, w, dw, z, aplow, aphigh
int	i, j, k, l, nin, ap, beam, dtype, nw, apin1, apin2, err
int	ninaps, noutaps, naps, npts, nbands
int	last, op1, axis[2]
pointer	ptr, in1, in2, out, tmp, mwin1, mwin2, mwout, sh1, sh2, ct, const
pointer	sp, str, key, input1, input2, temp, ltm, ltv, coeff, inaps, outaps

real	mw_c1tranr()
int	imaccess(), ctod()
int	imtlen(), sa_getim(), imgnfn()
bool	strne(), is_in_range(), fp_equald()
pointer	imofnlu()
pointer	immap(), smw_openim(), mw_open(), mw_sctran(), imgl3r(), impl3r()
errchk	immap, smw_openim, mw_open, imunmap, imgstr, imdelete
errchk	imgl3r, impl3r
errchk	shdr_open, sa_sextract
data	axis/1,2/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm, 3*3, TY_DOUBLE)
	call salloc (ltv, 3, TY_DOUBLE)
	call malloc (coeff, 1, TY_CHAR)

	Memc[input2] = EOS
	in1 = NULL
	in2 = NULL
	out = NULL
	tmp = NULL
	mwin1 = NULL
	mwin2 = NULL
	mwout = NULL
	sh1 = NULL
	sh2 = NULL
	const = NULL
	ninaps = 0
	noutaps = 0
	l1 = 1.
	dl = 1.
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
		    call shdr_open (out, mwout, 1, 1, INDEFI, SHHDR, sh2)
		    if (FORMAT(sh2) != MULTISPEC) {
			call sprintf (Memc[str], SZ_LINE, "%s - Wrong format")
			    call pargstr (output)
			call error (1, Memc[str])
		    }

		    # Determine existing apertures and renumber them if needed
		    noutaps = IM_LEN(out,2)
		    call salloc (outaps, noutaps, TY_INT)
		    do i = 1, IM_LEN(out,2) {
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

	    # Open input list.  Determine the number of final output
	    # apertures and maximum length in order to set the output
	    # dimensions.  Check also that there is data to copy.

	    call imtrew (list1)
	    nin = imtlen (list1)
	    npts = 0
	    naps = noutaps
	    nbands = 0
	    while (sa_getim (list1, Memc[input1], SZ_FNAME) != EOF) {
		iferr {
		    in1 = NULL
		    mwin1 = NULL

		    ptr = immap (Memc[input1], READ_ONLY, 0); in1 = ptr
		    ptr = smw_openim (in1); mwin1 = ptr
		    call shdr_open (in1, mwin1, 1, 1, INDEFI, SHHDR, sh1)

		    nbands = max (nbands, IM_LEN(in1,3))
		    do i = 1, IM_LEN(in1, AAXIS(sh1)) {
			call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)
			ap = AP(sh1)
			j = ap
			if (apmod > 1)
			    j = mod (j, apmod)
			select = is_in_range (aps, j) &&
			    is_in_range (beams, BEAM(sh1))
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
			call sa_sextract (sh1, w1, w2, rebin, w, dw, nw)
			if (ninaps == 0) {
			    l1 = w
			    dl = dw
			    same = true
			}
			if (same &&
			    (!fp_equald (w, l1) || !fp_equald (dw, dl))) {
			    l1 = 1.
			    dl = 1.
			    same = false
			}
			npts = max (npts, nw)
			ninaps = ninaps + 1
			if (Memc[input2] == EOS)
			    call strcpy (Memc[input1], Memc[input2], SZ_FNAME)
		    }
		} then
		    call erract (EA_WARN)

		if (nin > 1) {
		    call shdr_close (sh1)
		    if (mwin1 != NULL)
			call mw_close (mwin1)
		    if (in1 != NULL)
			call imunmap (in1)
		}
	    }

	    if (ninaps == 0)
		call error (1, "No apertures selected")
	    for (i=0; i<ninaps-1; i=i+1) {
		for (j=i+1; j<ninaps; j=j+1) {
		    if (Memi[inaps+i] == Memi[inaps+j]) {
			call error (1,
"Output spectra cannot have the same aperture number.\n\tUse renumber parameter.")
		    }
		}
	    }

	    # Set output image dimensions and WCS
	    # The WCS preserves the dispersion axis physical coordinates but
	    # resets the aperture axis physical coordinates

	    if (out != NULL) {
		ptr = immap (Memc[temp], NEW_COPY, out); tmp = ptr
		IM_PIXTYPE(tmp) = TY_REAL

		IM_LEN(tmp,1) = max (npts, IM_LEN(out,1))
		IM_LEN(tmp,2) = naps
		IM_LEN(tmp,3) = max (nbands, IM_LEN(out,3))
		if (nbands > 1)
		    IM_NDIM(out) = 3
		else if (naps > 1)
		    IM_NDIM(out) = 2
		else
		    IM_NDIM(out) = 1
		do j = 1, IM_LEN(out,3)
		    do i = 1, IM_LEN(out,2)
		        call amovr (Memr[imgl3r(out,i,j)],
			    Memr[impl3r(tmp,i,j)], IM_LEN(out,1))
		do j = IM_LEN(out,3)+1, nbands
		    do i = 1, IM_LEN(out,2)
		        call aclrr (Memr[impl3r(tmp,i,j)], IM_LEN(tmp,1))

		call imunmap (out)
		out = tmp
		tmp = NULL

		i = PNDIM(sh2)
		j = DAXIS(sh2)

		tmp = mw_open (NULL, 2)
		call mw_newsystem (tmp, "multispec", 2)
		call mw_swtype (tmp, axis, 2, "multispec", "")
		if (LABEL(sh2) != EOS)
		    call mw_swattrs (tmp, 1, "label", LABEL(sh2))
		if (UNITS(sh2) != EOS)
		    call mw_swattrs (tmp, 1, "units", UNITS(sh2))
		call aclrd (Memd[ltv], 3)
		call aclrd (Memd[ltm], 3*3)

		l1 = 1.
		dl = 1.
		call mw_gltermd (mwout, Memd[ltm], Memd[ltv], i)
		if (j == 2) {
		    Memd[ltv] = Memd[ltv+1]
		    Memd[ltm] = Memd[ltm+i+1]
		}
		Memd[ltv] = (Memd[ltv] - l1) / dl + 1
		Memd[ltm] = dl * Memd[ltm]
		Memd[ltv+1] = 0.
		Memd[ltm+1] = 0.
		Memd[ltm+3] = 1.
		call mw_sltermd (tmp, Memd[ltm], Memd[ltv], 2)

		ct = mw_sctran (mwout, "logical", "physical", 2)
		do i = 1, noutaps {
		    j = nint (mw_c1tranr (ct, real(i)))
		    call shdr_gwattrs (mwout, j, ap, beam, dtype,
			w, dw, nw, z, aplow, aphigh, coeff)
		    call shdr_swattrs (tmp, i, Memi[outaps+i-1], beam, dtype,
			w, dw, nw, z, aplow, aphigh, Memc[coeff])
		    if (verbose && ap != Memi[outaps+i-1])
			call sa_verbose (output, output, output, ap, ap,
			    Memi[outaps+i-1], COPY, "copy", const, reverse)
		}

		call shdr_close (sh2)
		call mw_close (mwout)
		mwout = tmp
		tmp = NULL

	    } else {
		if (nin > 1) {
		    ptr = immap (Memc[input2], READ_ONLY, 0); in1 = ptr
		    ptr = smw_openim (in1); mwin1 = ptr
		    call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)
		}
		ptr = immap (Memc[temp], NEW_COPY, in1); out = ptr
		IM_PIXTYPE(out) = TY_REAL

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
		j = imofnlu (out, "DISPAXIS,APID*")
		while (imgnfn (j, Memc[key], SZ_LINE) != EOF)
		    call imdelf (out, Memc[key])
		call imcfnl (j)

		i = PNDIM(sh1)
		j = DAXIS(sh1)

		ptr = mw_open (NULL, i); mwout = ptr
		call mw_newsystem (mwout, "multispec", 2)
		call mw_swtype (mwout, axis, 2, "multispec", "")
		if (LABEL(sh1) != EOS)
		    call mw_swattrs (mwout, 1, "label", LABEL(sh1))
		if (UNITS(sh1) != EOS)
		    call mw_swattrs (mwout, 1, "units", UNITS(sh1))
		call aclrd (Memd[ltv], 3)
		call aclrd (Memd[ltm], 3*3)
		call mw_gltermd (mwin1, Memd[ltm], Memd[ltv], i)
		if (j == 2) {
		    Memd[ltv] = Memd[ltv+1]
		    Memd[ltm] = Memd[ltm+i+1]
		}
		Memd[ltv] = (Memd[ltv] - l1) / dl + 1
		Memd[ltm] = dl * Memd[ltm]
		Memd[ltv+1] = 0.
		Memd[ltm+1] = 0.
		Memd[ltm+i+1] = 1.
		call mw_sltermd (mwout, Memd[ltm], Memd[ltv], 2)

		if (nin > 1) {
		    call shdr_close (sh1)
		    call mw_close (mwin1)
		    call imunmap (in1)
		}
	    }

	    # Now do the actual copy
	    last = noutaps
	    call imtrew (list1)
	    call imtrew (list2)
	    while (sa_getim (list1, Memc[input1], SZ_FNAME) != EOF) {
		i = sa_getim (list2, Memc[input2], SZ_FNAME)
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

		    call mw_gltermd (mwout, Memd[ltm], Memd[ltv], 2)
		    a = Memd[ltv]
		    b = Memd[ltm]
		    if (DC(sh1) == DCFUNC && !rebin) {
			call mw_gltermd (mwin1, Memd[ltm], Memd[ltv],
			    PNDIM(sh1))
			if (DAXIS(sh1) == 2) {
			    Memd[ltv] = Memd[ltv+1]
			    Memd[ltm] = Memd[ltm+PNDIM(sh1)+1]
			}
			Memd[ltv] = (Memd[ltv] - l1) / dl + 1
			Memd[ltm] = dl * Memd[ltm]
			if (!fp_equald(a,Memd[ltv])||!fp_equald(b,Memd[ltm])) {
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
			    call shdr_open (in2, mwin2,1,1,INDEFI,SHHDR,sh2)
			} else {
			    const = NULL
			    i = 1
			    if (ctod (Memc[input2], i, w) <= 0)
				call error (1, "Error in second operand")
			    call malloc (const, IM_LEN(out,1), TY_REAL)
			    call amovkr (real (w), Memr[const], IM_LEN(out,1))
			}
		    }

		    do i = 1, IM_LEN(in1, AAXIS(sh1)) {
			call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)
			ap = AP(sh1)
			apin1 = ap
			j = ap
			if (apmod > 1)
			    j = mod (j, apmod)
			select = is_in_range (aps, j) &&
			    is_in_range (beams, BEAM(sh1))
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
			call sa_sextract (sh1, w1, w2, rebin, w, dw, nw)

			# Copy and adjust dispersion info
			switch (FORMAT(sh1)) {
			case MULTISPEC:
			    call shdr_gwattrs (mwin1, PINDEX1(sh1), apin1, beam,
				dtype, w, dw, nw, z, aplow, aphigh, coeff)
			    if (!IS_INDEF(w1) && rebin)
				Memc[coeff] = EOS
			case TWODSPEC:
			    beam = BEAM(sh1)
			    dtype = DC(sh1)
			    z = 0.
			    aplow = APLOW(sh1)
			    aphigh = APHIGH(sh1)
			    Memc[coeff] = EOS
			}

			j = nint ((1 - a) / b)
			k = nint ((SN(sh1) - a) / b)
			nw = min (min (j ,k) + IM_LEN(out,1), max (j ,k))
			if (dtype == DCLOG) {
			    if (j != k)
				dw = log10 (W1(sh1)/ W0(sh1)) / (k - j)
			    w = log10 (W0(sh1) / (1 - z)) - (j - 1) * dw
			} else {
			    if (j != k)
				dw = (W1(sh1) - W0(sh1)) / (k - j) / (1 - z)
			    w = W0(sh1) / (1 - z) - (j - 1) * dw
			}

			call shdr_swattrs (mwout, l, ap, beam, dtype,
			    w, dw, nw, z, aplow, aphigh, Memc[coeff])

			# Copy titles
			if (strne (IM_TITLE(out), TITLE(sh1))) {
			    call sprintf (Memc[key], SZ_LINE, "APID%d")
				call pargi (l)
			    call imastr (out, Memc[key], TITLE(sh1))
			}

			# Copy the data
			op1 = op
			k = min (nbands, IM_LEN(in1,3))
			do j = 1, k  {
			    if (j != 1) {
				call shdr_open (in1, mwin1, i, j, INDEFI,
				    SHDATA, sh1)
				call sa_sextract (sh1, w1, w2, rebin, w, dw,nw)
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
				apin2 = AP(sh2)
			    }
			    call sa_arith (op1, sh1, sh2, const, reverse,
				Memr[SY(sh1)], Memr[impl3r(out,l,j)], SN(sh1))
			}
			do j = k+1, IM_LEN(out,3)
			    call aclrr (Memr[impl3r(out,l,j)], IM_LEN(out,1))

			if (verbose)
			    call sa_verbose (Memc[input1], Memc[input2],
				output, apin1, apin2, ap, op1, opstr,
				const, reverse)
		    }
		} then
		    call erract (EA_WARN)

		call shdr_close (sh1)
		call shdr_close (sh2)
		call mfree (const, TY_REAL)
		if (mwin2 != NULL)
		    call mw_close (mwin2)
		if (mwin1 != NULL)
		    call mw_close (mwin1)
		if (in2 != NULL)
		    call imunmap (in2)
		if (in1 != NULL)
		    call imunmap (in1)
	    }
	} then {
	    err = YES
	    call erract (EA_WARN)
	}

	if (mwout != NULL) {
	    call smw_saveim (mwout, out)
	    call mw_close (mwout)
	}
	if (tmp != NULL)
	    call imunmap (tmp)
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

 
# SA_1D -- Operate on input list to onedspec output

procedure sa_1d (list1, list2, output, op, opstr, w1, w2, rebin,
	aps, beams, complement, apmod, offset, reverse, ignoreaps, clobber,
	renumber, verbose)

int	list1			# Input image list
int	list2			# Input image list
char	output[ARB]		# Output image
int	op			# Operation
char	opstr[ARB]		# Operation string
real	w1			# Starting wavelength
real	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?
int	aps[ARB]		# Apertures
int	beams[ARB]		# Beams
bool	complement		# Complement aperture/beam selection
int	apmod			# Aperture modulus
int	offset			# Offset to add to output aperture numbers
bool	reverse			# Reverse order of operands
bool	ignoreaps		# Ignore apertures?
bool	clobber			# Clobber existing image?
bool	renumber		# Renumber apertures?
bool	verbose			# Verbose output?

bool	select
int	i, j, k, ap, beam, dtype, nw, apin1, apin2, naps, op1, axis[2]
double	w, dw, z, aplow, aphigh
pointer	ptr, in1, in2, out, mwin1, mwin2, mwout, sh1, sh2
pointer	sp, str, key, input1, input2, output1, temp, ltm, ltv, coeff, const

int	imaccess(), ctod(), patmake(), patmatch()
int	sa_getim(), imgnfn()
bool	is_in_range(), streq()
pointer	immap(), smw_openim(), mw_open(), impl1r(), imofnlu()
errchk	immap, smw_openim, mw_open, imunmap, impl1r, imdelete
errchk	shdr_open, sa_sextract
data	axis/1,2/

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (input1, SZ_FNAME, TY_CHAR)
	call salloc (input2, SZ_FNAME, TY_CHAR)
	call salloc (output1, SZ_FNAME, TY_CHAR)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (ltm, 3*3, TY_DOUBLE)
	call salloc (ltv, 3, TY_DOUBLE)
	call malloc (coeff, 1, TY_CHAR)

	# Loop through each spectrum in each input image.
	call imtrew (list1)
	call imtrew (list2)
	sh1 = NULL
	sh2 = NULL
	naps = 0
	while (sa_getim (list1, Memc[input1], SZ_FNAME) != EOF) {
	    i = sa_getim (list2, Memc[input2], SZ_FNAME)
	    iferr {
		in1 = NULL
		mwin1 = NULL
		in2 = NULL
		mwin2 = NULL

		ptr = immap (Memc[input1], READ_ONLY, 0); in1 = ptr
		ptr = smw_openim (in1); mwin1 = ptr
		call shdr_open (in1, mwin1, 1, 1, INDEFI, SHHDR, sh1)

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
			call amovkr (double (w), Memr[const], IM_LEN(in1,1))
		    }
		}

		do i = 1, IM_LEN(in1, AAXIS(sh1)) {
		    call shdr_open (in1, mwin1, i, 1, INDEFI, SHHDR, sh1)

		    # Check aperture and beam numbers.
		    ap = AP(sh1)
		    apin1 = ap
		    j = ap
		    if (apmod > 1)
			j = mod (j, apmod)
		    select = is_in_range (aps, j) &&
			is_in_range (beams, BEAM(sh1))
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

			# Open output spectrum
			call strcpy (output, Memc[str], SZ_LINE)
			j = patmake (".[0-9][0-9][0-9][0-9]$", Memc[key],
			    SZ_LINE)
			j = patmatch (Memc[str], Memc[key])
			if (j > 0)
			    Memc[str+j-6] = EOS
			call sprintf (Memc[output1], SZ_FNAME, "%s.%04d")
			    call pargstr (Memc[str])
			    call pargi (ap)
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
			    call strcpy (Memc[output1], Memc[temp], SZ_FNAME)

			ptr = immap (Memc[temp], NEW_COPY, in1); out = ptr
			IM_PIXTYPE(out) = TY_REAL
			ptr = mw_open (NULL, 2); mwout = ptr

			# Get data
			call shdr_open (in1, mwin1, i, 1, INDEFI, SHDATA, sh1)

			# Set header
			IM_NDIM(out) = 1
			call strcpy (TITLE(sh1), IM_TITLE(out), SZ_IMTITLE)
			j = imofnlu (out, "DISPAXIS,APID*")
			while (imgnfn (j, Memc[key], SZ_LINE) != EOF)
			    call imdelf (out, Memc[key])
			call imcfnl (j)

			# Set WCS
			j = PNDIM(sh1)
			k = DAXIS(sh1)
			call mw_newsystem (mwout, "multispec", 2)
			call mw_swtype (mwout, axis, 2, "multispec", "")
			if (LABEL(sh1) != EOS)
			    call mw_swattrs (mwout, 1, "label", LABEL(sh1))
			if (UNITS(sh1) != EOS)
			    call mw_swattrs (mwout, 1, "units", UNITS(sh1))
			call mw_gltermd (mwin1, Memd[ltm], Memd[ltv], j)
			if (k == 2) {
			    Memd[ltv] = Memd[ltv+1]
			    Memd[ltm] = Memd[ltm+j+1]
			}
			call sa_sextract (sh1, w1, w2, rebin, w, dw, nw)
			IM_LEN(out,1) = nw
			Memd[ltv] = (Memd[ltv] - w) / dw + 1
			Memd[ltm] = dw * Memd[ltm]
			Memd[ltv+1] = 0.
			Memd[ltm+1] = 0.
			Memd[ltm+2] = 0.
			Memd[ltm+3] = 1.
			call mw_sltermd (mwout, Memd[ltm], Memd[ltv], 2)

			# Copy and adjust dispersion info
			switch (FORMAT(sh1)) {
			case MULTISPEC:
			    call shdr_gwattrs (mwin1, PINDEX1(sh1), apin1, beam,
				dtype, w, dw, nw, z, aplow, aphigh, coeff)
			    if (!IS_INDEF(w1) && rebin)
				Memc[coeff] = EOS
			case TWODSPEC:
			    beam = BEAM(sh1)
			    dtype = DC(sh1)
			    z = 0.
			    aplow = APLOW(sh1)
			    aphigh = APHIGH(sh1)
			    Memc[coeff] = EOS
			}

			j = nint ((1 - Memd[ltv]) / Memd[ltm])
			k = nint ((SN(sh1) - Memd[ltv]) / Memd[ltm])
			nw = min (min (j ,k) + IM_LEN(out,1), max (j ,k))
			if (dtype == DCLOG) {
			    if (j != k)
				dw = log10 (W1(sh1)/ W0(sh1)) / (k - j)
			    w = log10 (W0(sh1) / (1 - z)) - (j - 1) * dw
			} else {
			    if (j != k)
				dw = (W1(sh1) - W0(sh1)) / (k - j) / (1 - z)
			    w = W0(sh1) / (1 - z) - (j - 1) * dw
			}

			call shdr_swattrs (mwout, 1, ap, beam, dtype,
			    w, dw, nw, z, aplow, aphigh, Memc[coeff])

			# Copy data
			op1 = op
			if (sh2 != NULL) {
			    if (ignoreaps)
				call shdr_open (in2, mwin2, i, 1, INDEFI,
				    SHDATA, sh2)
			    else {
				call shdr_open (in2, mwin2, i, 1, AP(sh1),
				    SHDATA, sh2)
				if (AP(sh2) != AP(sh1))
				    op1 = COPY
			    }
			    apin2 = AP(sh2)
			}

			call sa_arith (op1, sh1, sh2, const, reverse,
			    Memr[SY(sh1)], Memr[impl1r(out)], SN(sh1))

			if (verbose)
			    call sa_verbose (Memc[input1], Memc[input2],
				Memc[output1], apin1, apin2, INDEFI, op1,
				opstr, const, reverse)
		    } then
			call erract (EA_WARN)

		    if (mwout != NULL) {
			call smw_saveim (mwout, out)
			call mw_close (mwout)
		    }
		    if (out != NULL) {
			call imunmap (out)
			if (!streq (Memc[output1], Memc[temp])) {
			    call imgimage (Memc[input1], Memc[str], SZ_LINE)
			    if (streq (Memc[output1], Memc[str]))
				call imunmap (in1)
			    call imgimage (Memc[input2], Memc[str], SZ_LINE)
			    if (streq (Memc[output1], Memc[str]))
				call imunmap (in2)
			    call imdelete (Memc[output1])
			    call imrename (Memc[temp], Memc[output1])
			}
		    }
		}
	    } then
		call erract (EA_WARN)

	    if (mwin2 != NULL)
		call mw_close (mwin2)
	    if (mwin1 != NULL)
		call mw_close (mwin1)
	    if (in2 != NULL)
		call imunmap (in2)
	    if (in1 != NULL)
		call imunmap (in1)
	}

	call shdr_close (sh2)
	call shdr_close (sh1)
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
	    if (reverse)
		call apowr (Memr[buf], in, out, n)
	    else
		call apowr (in, Memr[buf], out, n)
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

procedure sa_verbose (input1, input2, output, ap1, ap2, apout, op, opstr,
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


# SA_GETIM -- Get image from a list with the image kernal extension removed.

int procedure sa_getim (list, image, maxchar)

int	list		# Image list
char	image[maxchar]	# Image name
int	maxchar		# Image name maximum character length

int	i, stat, imtgetim(), strmatch()
pointer	section

begin
	stat = imtgetim (list, image, maxchar)
	if (stat != EOF) {
	    call malloc (section, SZ_FNAME, TY_CHAR)
	    call imgsection (image, Memc[section], SZ_FNAME)
	    call imgimage (image, image, maxchar)
	    i = strmatch (image, ".??h$")
	    if (i > 0)
		image[i-4] = EOS
	    call strcat (Memc[section], image, maxchar)
	    call mfree (section, TY_CHAR)
	}
	return (stat)
end


# SA_SEXTRACT -- Extract a specific wavelength region

procedure sa_sextract (sh, w1, w2, rebin, l1, dl, n)

pointer	sh			#U SHDR structure
real	w1			#I Starting wavelength
real	w2			#I Ending wavelength
bool	rebin			#I Rebin wavelength region?
double	l1			#O Starting logical pixel
double	dl			#O Logical pixel increment
int	n			#O Number of logical pixels

int	i1, i2
bool	fp_equald()
double	shdr_wl()
errchk	shdr_wl, shdr_linear, shdr_extract

begin
	if (IS_INDEF(w1) || IS_INDEF(w2)) {
	    l1 = 1.
	    dl = 1.
	    n = SN(sh)
	    return
	}

	l1 = shdr_wl (sh, double (w1))
	dl = shdr_wl (sh, double (w2))
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
	    call shdr_extract (sh, w1, w2, rebin)
end
