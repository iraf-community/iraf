include	<error.h>
include <imhdr.h>
include	<imset.h>
include	<mwset.h>
include	"shdr.h"


# SHDR_OPEN    -- Open spectrum header structure.
# SHDR_CLOSE   -- Close and free spectrum header structure.
# SHDR_2D      -- Set/get physical dispersion axis and number of lines to sum.
# SHDR_COPY    -- Make a copy of an SHDR structure.
# SHDR_SYSTEM  -- Set or change the system.
# SHDR_LW      -- Logical to world coordinate transformation
# SHDR_WL      -- World to logical coordinate transformation
# SHDR_REBIN   -- Rebin spectrum to dispersion of reference spectrum
# SHDR_LINEAR  -- Rebin spectrum to linear dispersion
# SHDR_EXTRACT -- Extract a specific wavelength region
# SHDR_GI      -- Load an integer value from the header
# SHDR_GR      -- Load a real value from the header
# SHDR_GWATTRS -- Get spectrum attribute parameters
# SHDR_SWATTRS -- Set spectrum attribute parameters


# SHDR_OPEN -- Open spectrum header structure.
# This routine sets header information, WCS transformations, and extracts the
# spectrum from MULTISPEC and TWODSPEC format images.  The
# spectrum from a 2D/3D format is specified by a logical line and band
# number.  Optionally a MULTISPEC spectrum may be selected by it's aperture
# number.  The physical dispersion axis and summing parameter in TWODSPEC
# images are obtained by a call to SHDR_2D.  The access modes are header only
# or header and data.  Special checks are made to avoid repeated setting of
# the header and WCS information common to all spectra in a 2D format
# provided the previously set structure is input.  Note that the logical to
# world and world to logical transformations require that the MWCS pointer
# not be closed.

procedure shdr_open (im, mw, index1, index2, ap, mode, sh)

pointer	im			# IMIO pointer
pointer	mw			# MWCS pointer
int	index1			# Image index desired
int	index2			# Image index desired
int	ap			# Aperture number desired
int	mode			# Access mode
pointer	sh			# SHDR pointer

int	format, daxisl, daxisp, np, nsum
int	i, j, k, l, aaxis, pndim, np1, np2, axno[3], axval[3]
real	r[3], w[3], mw_c1tranr(), asumr()
double	dval, aplow, aphigh, shdr_lw()
bool	newim, streq()
int	mw_stati()
pointer	sp, key, str, coeff, ct, mw_sctran(), imgs3r(), un_open()
errchk	mw_sctran, imgstr, imgeti, imgetr, shdr_2d, un_open, shdr_gwattrs

define	data_	90

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	coeff = NULL

	# Allocate basic structure or check if the same spectrum is requested
	if (sh == NULL) {
	    call calloc (sh, LEN_SHDR, TY_STRUCT)
	    newim = true
	} else {
	    call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	    newim = !streq (Memc[str], SPECTRUM(sh))
	    if (!newim) {
		if (INDEX1(sh)==index1 && max(1,INDEX2(sh))==index2) {
		    if (IS_INDEFI(ap) || AP(sh)==ap) {
			if (CTLW(sh) != NULL && CTWL(sh) != NULL &&
			    ((mode==SHDATA && SY(sh)!=NULL) ||
			    (mode==SHHDR && SY(sh)==NULL))) {
			    call sfree (sp)
			    return
			} else {
			    np1 = NP1(sh)
			    np2 = NP2(sh)
			    np = np2 - np1 + 1
			    goto data_
			}
		    }
		}
	    }
	}

	# Set parameters common to an entire image
	if (newim) {
	    call imstats (im, IM_IMAGENAME, SPECTRUM(sh), LEN_SHDRS)
	    call strcpy (IM_TITLE(im), TITLE(sh), LEN_SHDRS)
	    IM(sh) = im
	    MW(sh) = mw

	    # Get standard parameters
	    call shdr_gi (im, "OFLAG", OBJECT, TYPE(sh))
	    call shdr_gr (im, "EXPOSURE", INDEF, IT(sh))
	    call shdr_gr (im, "ITIME", IT(sh), IT(sh))
	    call shdr_gr (im, "EXPTIME", IT(sh), IT(sh))
	    call shdr_gr (im, "RA", INDEF, RA(sh))
	    call shdr_gr (im, "DEC", INDEF, DEC(sh))
	    call shdr_gr (im, "UT", INDEF, UT(sh))
	    call shdr_gr (im, "ST", INDEF, ST(sh))
	    call shdr_gr (im, "HA", INDEF, HA(sh))
	    call shdr_gr (im, "AIRMASS", INDEF, AM(sh))
	    call shdr_gi (im, "DC-FLAG", DCNO, DC(sh))
	    call shdr_gi (im, "EX-FLAG", ECNO, EC(sh))
	    call shdr_gi (im, "CA-FLAG", FCNO, FC(sh))

	    # Flag bad airmass value; i.e. 0
	    if (!IS_INDEF (AM(sh)) && AM(sh) < 1.)
		AM(sh) = INDEF

	    # Determine the format and dispersion axis
	    call mw_gwattrs (mw, 0, "system", Memc[key], SZ_FNAME)
	    pndim = mw_stati (mw, MW_NDIM)
	    call mw_gaxmap (mw, axno, axval, pndim)

	    if (streq (Memc[key], "multispec"))
		format = MULTISPEC
	    else
		format = TWODSPEC

	    switch (format) {
	    case MULTISPEC:
		daxisp = 1
		daxisl = axno[daxisp]
		nsum = 1

		if (daxisl == 0) {
		    if (axval[daxisp] == 0)
			daxisl = daxisp
		    else
			call error (1, "No dispersion axis")
		}

		CTLW1(sh) = mw_sctran (MW(sh), "logical", "multispec", 3)
		CTWL1(sh) = mw_sctran (MW(sh), "multispec", "logical", 3)
	    case TWODSPEC:
		call shdr_2d (im, daxisp, nsum)
		daxisl = max (1, axno[daxisp])
		if (IM_LEN(im,daxisl) == 1)
		    daxisl = mod (daxisl, 2) + 1

		i = daxisp
		do daxisp = 1, pndim
		    if (axno[daxisp] == daxisl)
			break
		if (i != daxisp) {
		    call eprintf (
		      "WARNING: Dispersion axis %d not found. Using axis %d.\n")
		    call pargi (i)
		    call pargi (daxisp)
		}

		CTLW1(sh) = mw_sctran (MW(sh), "logical", "world", daxisp)
		CTWL1(sh) = mw_sctran (MW(sh), "world", "logical", daxisp)

		# Check that the dispersion type makes sense.
		if (DC(sh) == DCLOG) {
		    w[1] = mw_c1tranr (CTLW1(sh), 1.)
		    w[2] = mw_c1tranr (CTLW1(sh), real (IM_LEN[im,daxisl]))
		    if (abs(w[1]) > 20. || abs(w[2]) > 20.)
			DC(sh) = DCLINEAR
		}
	    }

	    # Convert physical dispersion axis to logical dispersion axis
	    daxisl = axno[daxisp]
	    if (daxisl == 0) {
		if (axval[daxisp] == 0)
		    daxisl = daxisp
		else
		    call error (1, "No dispersion axis")
	    }
	    aaxis = 3 - daxisl

	    # Set labels
	    iferr (call mw_gwattrs (mw, daxisp, "label", LABEL(sh), LEN_SHDRS))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    if (streq (LABEL(sh), "multispe"))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    iferr (call mw_gwattrs (mw, daxisp, "units", UNITS(sh), LEN_SHDRS))
		call strcpy ("", UNITS(sh), LEN_SHDRS)

	    # Set units
	    UN(sh) = un_open (UNITS(sh))
	    MWUN(sh) = un_open (UNITS(sh))

	    FORMAT(sh) = format
	    NSUM(sh) = nsum
	    AAXIS(sh) = aaxis
	    DAXISP(sh) = daxisp
	    DAXIS(sh) = daxisl
	    NDIM(sh) = IM_NDIM(im)
	    PNDIM(sh) = pndim
	    if (NDIM(sh) < 3)
		IM_LEN(im,3) = 1
	    if (NDIM(sh) < 2)
		IM_LEN(im,2) = 1
	} else {
	    format = FORMAT(sh)
	    aaxis = AAXIS(sh)
	    daxisp = DAXISP(sh)
	    daxisl = DAXIS(sh)
	}

	# Set WCS parameters for spectrum type
	INDEX1(sh) = max (1, min (IM_LEN(im,aaxis), index1))
	INDEX2(sh) = max (1, min (IM_LEN(im,3), index2))

	switch (format) {
	case MULTISPEC:
	    # If an aperture is specified first try and find it.
	    # If not specified or not found then use the index.

	    np = IM_LEN(im,1)
	    np1 = 1
	    ct = mw_sctran (mw, "logical", "physical", 2)
	    AP(sh) = 0
	    if (!IS_INDEFI(ap)) {
		do i = 1, IM_LEN(im,2) {
		    j = mw_c1tranr (ct, real(i))
		    call shdr_gwattrs (mw, j, AP(sh), BEAM(sh), DC(sh), dval,
			dval, np2, dval, aplow, aphigh, coeff) 
		    APLOW(sh) = aplow
		    APHIGH(sh) = aphigh
		    if (AP(sh) == ap) {
			INDEX1(sh) = i
			break
		    }
		}
	    }
	    if (AP(sh) != ap) {
		i = INDEX1(sh)
		j = mw_c1tranr (ct, real(i))
		call shdr_gwattrs (mw, j, AP(sh), BEAM(sh), DC(sh), dval,
		    dval, np2, dval, aplow, aphigh, coeff) 
		APLOW(sh) = aplow
		APHIGH(sh) = aphigh
	    }

	    PINDEX1(sh) = j
	    call sprintf (Memc[key], SZ_LINE, "APID%d")
		call pargi (j)
	    iferr (call imgstr (im, Memc[key], TITLE(sh), LEN_SHDRS))
		call strcpy (IM_TITLE(im), TITLE(sh), LEN_SHDRS)

	    call mw_ctfree (ct)
	case TWODSPEC:
	    np = IM_LEN(im,daxisl)

	    ct = mw_sctran (mw, "logical", "physical", 3B)
	    r[daxisp] = 1.
	    r[aaxis] = INDEX1(sh)
	    call mw_ctranr (ct, r, w, 2)
	    i = w[daxisp]
	    r[daxisp] = np
	    call mw_ctranr (ct, r, w, 2)
	    j = w[daxisp]
	    call mw_ctfree (ct)

	    np1 = min (i, j)
	    np2 = max (i, j)
	    #AP(sh) = w[aaxis]
	    #BEAM(sh) = w[aaxis]
	    AP(sh) = INDEX1(sh)
	    BEAM(sh) = INDEX1(sh)
	    APLOW(sh) = max (1, AP(sh) - NSUM(sh) / 2)
	    APHIGH(sh) = min (IM_LEN(im,aaxis), nint(APLOW(sh)) + NSUM(sh) - 1)
	    APLOW(sh) = max (1, nint(APHIGH(sh)) - NSUM(sh) + 1)
	    NSUM(sh) = nint(APHIGH(sh)) - nint(APLOW(sh)) + 1
	    PINDEX1(sh) = w[aaxis]
	    call strcpy (IM_TITLE(im), TITLE(sh), LEN_SHDRS)
	}
	
	# Set NP1 and NP2 in logical coordinates.
	ct = mw_sctran (mw, "physical", "logical", daxisp)
	i = max (1, min (int (mw_c1tranr (ct, real (np1))), np))
	j = max (1, min (int (mw_c1tranr (ct, real (np2))), np))
	call mw_ctfree (ct)
	np1 = min (i, j)
	np2 = max (i, j)
	np = np2 - np1 + 1

	NP1(sh) = np1
	NP2(sh) = np2
	SN(sh) = np

data_	CTLW(sh) = CTLW1(sh)
	CTWL(sh) = CTWL1(sh)

	# Set linear approximation.
	W0(sh) = shdr_lw (sh, double(np1))
	W1(sh) = shdr_lw (sh, double(np2))
	WP(sh) = (W1(sh) - W0(sh)) / (np2 - np1)
	SN(sh) = np2 - np1 + 1

	if (mode == SHDATA) {
	    # Set WCS array
	    if (SX(sh) == NULL)
		call malloc (SX(sh), np, TY_REAL)
	    else
		call realloc (SX(sh), np, TY_REAL)
	    do i = np1, np2
		Memr[SX(sh)+i-np1] = shdr_lw (sh, double(i))

	    # Set spectrum array
	    if (SY(sh) == NULL)
		call malloc (SY(sh), np, TY_REAL)
	    else
		call realloc (SY(sh), np, TY_REAL)

	    i = max (1, INDEX1(sh))
	    j = max (1, INDEX2(sh))
	    switch (FORMAT(sh)) {
	    case MULTISPEC:
		call amovr (Memr[imgs3r(im,np1,np2,i,i,j,j)], Memr[SY(sh)], np)
	    case TWODSPEC:
	        APLOW(sh) = max (1, AP(sh) - NSUM(sh) / 2)
	        APHIGH(sh) = min (IM_LEN(im,aaxis),
		    nint(APLOW(sh)) + NSUM(sh) - 1)
		APLOW(sh) = max (1, nint(APHIGH(sh)) - NSUM(sh) + 1)
	        NSUM(sh) = nint(APHIGH(sh)) - nint(APLOW(sh)) + 1
		k = nint (APLOW(sh))
		l = nint (APHIGH(sh))
		nsum = l - k + 1
		if (daxisl == 1) {
		    do i = k, l {
			if (i == k)
		    	    call amovr (Memr[imgs3r(im,np1,np2,i,i,j,j)],
				Memr[SY(sh)], np)
			else
			     call aaddr (Memr[imgs3r(im,np1,np2,i,i,j,j)],
				 Memr[SY(sh)], Memr[SY(sh)], np)
		    }
		} else if (daxisl == 2) {
		    do i = np1, np2
			Memr[SY(sh)+i-np1] =
			    asumr (Memr[imgs3r(im,k,l,i,i,j,j)], nsum)
		}
	    }
	} else {
	    call mfree (SX(sh), TY_REAL)
	    call mfree (SY(sh), TY_REAL)
	}

	#if (PNDIM(sh) < 2) {
	#    INDEX1(sh) = 0
	#    PINDEX1(sh) = 0
	#}
	#if (IM_NDIM(im) < 3) {
	#    INDEX2(sh) = 0
	#    PINDEX2(sh) = 0
	#}

	call mfree (coeff, TY_CHAR)
	call sfree (sp)
end


# SHDR_CLOSE -- Close and free spectrum header structure.

procedure shdr_close (sh)

pointer	sh			# SHDR structure

begin
	if (sh == NULL)
	    return
	call mfree (SX(sh), TY_REAL)
	call mfree (SY(sh), TY_REAL)
	call un_close (UN(sh))
	call un_close (MWUN(sh))
	call mfree (sh, TY_STRUCT)
end


# SHDR_2D -- Set/get physical dispersion axis and number of lines to sum.
# If the IMIO pointer is NULL then the values are set otherwise
# the values are returned.  If the default values are zero (the initial
# values) and they are not in the image header then they are queried
# from the CL.

procedure shdr_2d (im, daxisp, nsum)

pointer	im			# IMIO pointer (get/set flag)
int	daxisp			# Physical dispersion axis
int	nsum			# Number of lines to sum

int	da, ns, imgeti(), clgeti()
data	da/0/, ns/0/
errchk	clgeti

begin
	if (im == NULL) {
	    if (!IS_INDEFI (daxisp))
		da = daxisp
	    if (!IS_INDEFI (nsum))
		ns = nsum
	    return
	}

	daxisp = da
	if (daxisp == 0) {
	    iferr (daxisp = imgeti (im, "DISPAXIS"))
		daxisp = clgeti ("dispaxis")
	}
	nsum = ns
	if (nsum == 0)
	    nsum = clgeti ("nsum")
end


# SHDR_COPY -- Make a copy of an SHDR structure.
# The image pointer is not copied.  The  MWCS pointer and transform pointers
# may be copied if desired.

procedure shdr_copy (sh1, sh2, wcs)

pointer	sh1		# SHDR structure to copy
pointer	sh2		# SHDR structure copy
int	wcs		# Make copy of wcs?

pointer	un, mwun, sx, sy, mw_newcopy()
errchk	shdr_system

begin
	if (sh2 == NULL)
	    call calloc (sh2, LEN_SHDR, TY_STRUCT)

	un = UN(sh2)
	mwun = MWUN(sh2)
	sx = SX(sh2)
	sy = SY(sh2)
	call amovi (Memi[sh1], Memi[sh2], LEN_SHDR)
	UN(sh2) = un
	MWUN(sh2) = mwun
	SX(sh2) = sx
	SY(sh2) = sy
	call un_copy (UN(sh1), UN(sh2))
	call un_copy (MWUN(sh1), MWUN(sh2))
	if (SX(sh1) != NULL) {
	    if (SX(sh2) == NULL)
		call malloc (SX(sh2), SN(sh1), TY_REAL)
	    else
		call realloc (SX(sh2), SN(sh1), TY_REAL)
	    call amovr (Memr[SX(sh1)], Memr[SX(sh2)], SN(sh1))
	}
	if (SY(sh1) != NULL) {
	    if (SY(sh2) == NULL)
		call malloc (SY(sh2), SN(sh1), TY_REAL)
	    else
		call realloc (SY(sh2), SN(sh1), TY_REAL)
	    call amovr (Memr[SY(sh1)], Memr[SY(sh2)], SN(sh1))
	}
	if (wcs == YES && IM(sh1) != NULL && MW(sh1) != NULL) {
	    MW(sh2) = mw_newcopy (MW(sh1))
	    CTLW1(sh2) = NULL
	    CTWL1(sh2) = NULL
	    if (FORMAT(sh2) == MULTISPEC)
		call shdr_system (sh2, "multispec")
	    else
		call shdr_system (sh2, "world")
	}
end


# SHDR_SYSTEM -- Set or change the system.

procedure shdr_system (sh, system)

pointer	sh			# SHDR pointer
char	system[ARB]		# System

int	i, np1, np2
bool	streq()
double	shdr_lw()
pointer	mw_sctran(), un_open()
errchk	mw_sctran, un_open

begin
	if (CTLW1(sh) != NULL)
	    call mw_ctfree (CTLW1(sh))
	if (CTWL1(sh) != NULL)
	    call mw_ctfree (CTWL1(sh))

	switch (FORMAT(sh)) {
	case MULTISPEC:
	    CTLW1(sh) = mw_sctran (MW(sh), "logical", system, 3)
	    CTWL1(sh) = mw_sctran (MW(sh), system, "logical", 3)
	case TWODSPEC:
	    CTLW1(sh) = mw_sctran (MW(sh), "logical", system, DAXISP(sh))
	    CTWL1(sh) = mw_sctran (MW(sh), system, "logical", DAXISP(sh))
	}
	CTLW(sh) = CTLW1(sh)
	CTWL(sh) = CTWL1(sh)

	# Set labels and units
	if (streq (system, "physical")) {
	    call strcpy ("Pixel", LABEL(sh), LEN_SHDRS)
	    call strcpy ("", UNITS(sh), LEN_SHDRS)
	} else {
	    iferr (call mw_gwattrs (MW(sh), DAXISP(sh), "label", LABEL(sh),
		LEN_SHDRS))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    if (streq (LABEL(sh), "multispe"))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    iferr (call mw_gwattrs (MW(sh), DAXISP(sh), "units", UNITS(sh),
		LEN_SHDRS))
		call strcpy ("", UNITS(sh), LEN_SHDRS)
	}

	call un_close (MWUN(sh))
	MWUN(sh) = un_open (UNITS(sh))

	np1 = NP1(sh)
	np2 = NP2(sh)
	W0(sh) = shdr_lw (sh, double(np1))
	W1(sh) = shdr_lw (sh, double(np2))
	WP(sh) = (W1(sh) - W0(sh)) / (np2 - np1)
	if (SX(sh) != NULL)
	    do i = np1, np2
		Memr[SX(sh)+i-np1] = shdr_lw (sh, double(i))
end


# SHDR_LW -- Logical to world coordinate transformation
# The transformation pointer is generally NULL only after SHDR_LINEAR

double procedure shdr_lw (sh, l)

pointer	sh			# SHDR pointer
double	l			# Logical coordinate
double	w			# World coordinate

double	l1, l2, w1, mw_c1trand()

begin
	if (CTLW(sh) != NULL) {
	    switch (FORMAT(sh)) {
	    case MULTISPEC:
		call mw_c2trand (CTLW(sh), l, double (INDEX1(sh)), w, w1)
	    case TWODSPEC:
		w = mw_c1trand (CTLW(sh), l)
		if (DC(sh) == DCLOG)
		    w = 10. ** max (-20D0, min (20D0, w))
	    }
	} else {
	    switch (DC(sh)) {
	    case DCLINEAR:
		w = W0(sh) + (l - 1) * WP(sh)
	    case DCLOG:
		w = W0(sh) * 10. ** (log10(W1(sh)/W0(sh)) * (l-1) / (SN(sh)-1))
	    case DCFUNC:
		w = W0(sh)
		call mw_c2trand (CTWL1(sh), w, double (INDEX1(sh)), l1, w1)
		w = W1(sh)
		call mw_c2trand (CTWL1(sh), w, double (INDEX1(sh)), l2, w1)
		if (SN(sh) > 1)
		    l1 = (l2 - l1) / (SN(sh) - 1) * (l - 1) + l1
		else
		    l1 = l - 1 + l1
		call mw_c2trand (CTLW1(sh), l1, double (INDEX1(sh)), w, w1)
	    }
	}

	iferr (call un_ctrand (MWUN(sh), UN(sh), w, w, 1))
	    ;
	return (w)
end


# SHDR_WL -- World to logical coordinate transformation
# The transformation pointer is generally NULL only after SHDR_LINEAR

double procedure shdr_wl (sh, w)

pointer	sh			# SHDR pointer
double	w			# World coordinate
double	l			# Logical coordinate

double	w1, l1, l2, mw_c1trand()

begin
	iferr (call un_ctrand (UN(sh), MWUN(sh), w, w1, 1))
	    w1 = w

	if (CTWL(sh) != NULL) {
	    switch (FORMAT(sh)) {
	    case MULTISPEC:
		call mw_c2trand (CTWL(sh), w1, double (INDEX1(sh)), l, l1)
	    case TWODSPEC:
		if (DC(sh) == DCLOG)
		    w1 = log10 (w1)
		l = mw_c1trand (CTWL(sh), w1)
	    }
	} else {
	    switch (DC(sh)) {
	    case DCLINEAR:
		l = (w1 - W0(sh)) / WP(sh) + 1
	    case DCLOG:
		l = log10(w1/W0(sh)) / log10(W1(sh)/W0(sh)) * (SN(sh)-1) + 1
	    case DCFUNC:
		call mw_c2trand (CTWL1(sh), w1, double (INDEX1(sh)), l, l1)

		w1 = W0(sh)
		call mw_c2trand (CTWL1(sh), w1, double (INDEX1(sh)), l1, w1)
		w1 = W1(sh)
		call mw_c2trand (CTWL1(sh), w1, double (INDEX1(sh)), l2, w1)
		if (l1 != l2)
		    l = (SN(sh) - 1) / (l2 - l1) * (l - l1) + 1
		else
		    l = l - l1 + 1
	    }
	}

	return (l)
end


# SHDR_REBIN -- Rebin spectrum to dispersion of reference spectrum
# The interpolation function is set by ONEDINTERP.

procedure shdr_rebin (sh, shref)

pointer	sh		# Spectrum to be rebinned
pointer	shref		# Reference spectrum

int	i, j, ia, ib, n, onedinterp()
real	a, b, sum, asieval(), asigrl()
double	x, w, xmin, xmax, shdr_lw(), shdr_wl()
pointer	unsave, asi, spec
bool	fp_equalr()
errchk	onedinterp

begin
	# Check if rebinning is needed
	if (DC(sh) == DC(shref) && DC(sh) != DCFUNC &&
	    fp_equalr (W0(sh), W0(shref)) && fp_equalr(WP(sh), WP(shref)) &&
	    SN(sh) == SN(shref))
	    return

	# Do everything in units of MWCS.
	unsave = UN(sh)
	UN(SH) = MWUN(sh)

	# Fit the interpolation function to the spectrum.
	# Extend the interpolation by one pixel at each end.

	n = SN(sh)
	call malloc (spec, n+2, TY_REAL)
	call amovr (Memr[SY(sh)], Memr[spec+1], n)
	Memr[spec] = Memr[SY(sh)]
	Memr[spec+n+1] = Memr[SY(sh)+n-1]
	call asiinit (asi, onedinterp())
	call asifit (asi, Memr[spec], n+2)
	call mfree (spec, TY_REAL)

	xmin = 0.5
	xmax = n + 0.5

	# Reallocate spectrum data array
	if (n != SN(shref)) {
	    n = SN(shref)
	    call realloc (SX(sh), n, TY_REAL)
	    call realloc (SY(sh), n, TY_REAL)
	    call aclrr (Memr[SX(sh)], n)
	    call aclrr (Memr[SY(sh)], n)
	}
	spec = SY(sh)

	# Integrate across pixels using ASIGRL.

	x = 0.5
	w = shdr_lw (shref, x)
	x = shdr_wl (sh, w)
	b = max (xmin, min (xmax, x)) + 1
	do i = 1, n {
	    x = i + 0.5
	    w = shdr_lw (shref, x)
	    x = shdr_wl (sh, w)
	    a = b
	    b = max (xmin, min (xmax, x)) + 1
	    if (a <= b) {
		ia = nint (a + 0.5)
		ib = nint (b - 0.5)
		if (abs (a+0.5-ia) < .001 && abs (b-0.5-ib) < .001) {
		    sum = 0.
		    do j = ia, ib
			sum = sum + asieval (asi, real(j))
		    if (ib - ia > 0)
			sum = sum / (ib - ia)
		} else {
		    sum = asigrl (asi, a, b)
		    if (b - a > 0.)
			sum = sum / (b - a)
		}
	    } else {
		ib = nint (b + 0.5)
		ia = nint (a - 0.5)
		if (abs (a-0.5-ia) < .001 && abs (b+0.5-ib) < .001) {
		    sum = 0.
		    do j = ib, ia
			sum = sum + asieval (asi, real(j))
		    if (ia - ib > 0)
			sum = sum / (ia - ib)
		} else {
		    sum = asigrl (asi, b, a)
		    if (a - b > 0.)
			sum = sum / (a - b)
		}
	    }
	    Memr[spec] = sum
	    spec = spec + 1
	}

	call asifree (asi)

	# Set the rest of the header.  The coordinate transformations are
	# canceled to indicate they are not valid for the data.  They
	# are not freed because the same pointer may be used in other
	# spectra from the same image.

	call amovr (Memr[SX(shref)], Memr[SX(sh)], n)
	DC(sh) = DC(shref)
	W0(sh) = W0(shref)
	W1(sh) = W1(shref)
	WP(sh) = WP(shref)
	SN(sh) = SN(shref)

	CTLW(sh) = NULL
	CTWL(sh) = NULL

	# Restore original units
	UN(sh) = unsave
	iferr (call un_ctranr (MWUN(sh), UN(sh), Memr[SX(sh)], Memr[SX(sh)],
	    SN(sh)))
	    ;
end


# SHDR_LINEAR -- Rebin spectrum to linear dispersion
# The interpolation function is set by ONEDINTERP

procedure shdr_linear (sh, w0, w1, sn, dc)

pointer	sh		# Spectrum to be rebinned
real	w0		# Wavelength of first logical pixel
real	w1		# Wavelength of last logical pixel
int	sn		# Number of pixels
int	dc		# Dispersion type (DCLINEAR | DCLOG)

int	i, j, ia, ib, n, onedinterp()
real	a, b, sum, asieval(), asigrl()
double	x, w, w0l, wp, xmin, xmax, shdr_wl()
pointer	unsave, asi, spec
bool	fp_equalr()
errchk	onedinterp

begin
	# Check if rebinning is needed
	if (DC(sh) == dc && fp_equalr (W0(sh), w0) &&
	    fp_equalr (W1(sh), w1) && SN(sh) == sn)
	    return

	# Do everything in units of MWCS.
	unsave = UN(sh)
	UN(SH) = MWUN(sh)

	# Fit the interpolation function to the spectrum.
	# Extend the interpolation by one pixel at each end.

	n = SN(sh)
	call malloc (spec, n+2, TY_REAL)
	call amovr (Memr[SY(sh)], Memr[spec+1], n)
	Memr[spec] = Memr[SY(sh)]
	Memr[spec+n+1] = Memr[SY(sh)+n-1]
	call asiinit (asi, onedinterp())
	call asifit (asi, Memr[spec], n+2)
	call mfree (spec, TY_REAL)

	xmin = 0.5
	xmax = n + 0.5

	# Reallocate spectrum data array
	if (n != sn) {
	    n = sn
	    call realloc (SX(sh), n, TY_REAL)
	    call realloc (SY(sh), n, TY_REAL)
	}
	spec = SY(sh)

	# Integrate across pixels using ASIGRL.

	x = 0.5
	if (dc == DCLOG) {
	    w0l = log10 (w0)
	    wp = (log10 (w1) - log10(w0)) / (n - 1)
	    w = 10. ** (w0l+(x-1)*wp)
	} else {
	    wp = (w1 - w0) / (n - 1)
	    w = w0 + (x - 1) * wp
	}
	x = shdr_wl (sh, w)
	b = max (xmin, min (xmax, x)) + 1
	do i = 1, n {
	    x = i + 0.5
	    if (dc == DCLOG)
		w = 10. ** (w0l + (x - 1) * wp)
	    else
		w = w0 + (x - 1) * wp
	    x = shdr_wl (sh, w)
	    a = b
	    b = max (xmin, min (xmax, x)) + 1
	    if (a <= b) {
		ia = nint (a + 0.5)
		ib = nint (b - 0.5)
		if (abs (a+0.5-ia) < .001 && abs (b-0.5-ib) < .001) {
		    sum = 0.
		    do j = ia, ib
			sum = sum + asieval (asi, real(j))
		    if (ib - ia > 0)
			sum = sum / (ib - ia)
		} else {
		    sum = asigrl (asi, a, b)
		    if (b - a > 0.)
			sum = sum / (b - a)
		}
	    } else {
		ib = nint (b + 0.5)
		ia = nint (a - 0.5)
		if (abs (a-0.5-ia) < .001 && abs (b+0.5-ib) < .001) {
		    sum = 0.
		    do j = ib, ia
			sum = sum + asieval (asi, real(j))
		    if (ia - ib > 0)
			sum = sum / (ia - ib)
		} else {
		    sum = asigrl (asi, b, a)
		    if (a - b > 0.)
			sum = sum / (a - b)
		}
	    }
	    Memr[spec] = sum
	    spec = spec + 1
	}

	call asifree (asi)

	# Set the rest of the header.  The coordinate transformations are
	# canceled to indicate they are not valid for the data.  They
	# are not freed because the same pointer may be used in other
	# spectra from the same image.

	do i = 0, n-1 {
	    if (dc == DCLOG)
		w = 10. ** (w0l + i * wp)
	    else
		w = w0 + i * wp
	    Memr[SX(sh)+i] = w
	}
	W0(sh) = w0
	W1(sh) = w1
	WP(sh) = (w1 - w0) / (sn - 1)
	SN(sh) = sn
	NP1(sh) = 1
	NP2(sh) = sn
	DC(sh) = dc

	CTLW(sh) = NULL
	CTWL(sh) = NULL

	# Restore original units
	UN(sh) = unsave
	iferr (call un_ctranr (MWUN(sh), UN(sh), Memr[SX(sh)], Memr[SX(sh)],
	    sn))
	    ;
end


# SHDR_EXTRACT -- Extract a specific wavelength region

procedure shdr_extract (sh, w1, w2, rebin)

pointer	sh			# SHDR structure
real	w1			# Starting wavelength
real	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?

int	i, i1, i2, n
double	l1, l2
pointer	buf
bool	fp_equald()
double	shdr_wl(), shdr_lw()
errchk	shdr_linear, shdr_lw, shdr_wl

begin
	l1 = shdr_wl (sh, double (w1))
	l2 = shdr_wl (sh, double (w2))
	if (fp_equald(l1,l2) || max(l1,l2) < 1 || min (l1,l2) > SN(sh))
	    call error (1, "No pixels to extract")
	l1 = max (1D0, min (double (SN(sh)), l1))
	l2 = max (1D0, min (double (SN(sh)), l2))
	i1 = nint (l1)
	i2 = nint (l2)
	n = abs (i2 - i1) + 1

	if (rebin) {
	    l1 = shdr_lw (sh, l1)
	    l2 = shdr_lw (sh, l2)
	    if (DC(sh) == DCFUNC)
		call shdr_linear (sh, real (l1), real (l2), n, DCLINEAR)
	    else
		call shdr_linear (sh, real (l1), real (l2), n, DC(sh))
	} else {
	    if (i1 == 1 && i2 == SN(sh))
		return

	    if (i1 <= i2) {
		call amovr (Memr[SX(sh)+i1-1], Memr[SX(sh)], n)
		call amovr (Memr[SY(sh)+i1-1], Memr[SY(sh)], n)
	    } else {
		call malloc (buf, n, TY_REAL)
		do i = i1, i2, -1
		    Memr[buf+i1-i] = Memr[SX(sh)+i-1]
		call amovr (Memr[buf], Memr[SX(sh)], n)
		do i = i1, i2, -1
		    Memr[buf+i1-i] = Memr[SY(sh)+i-1]
		call amovr (Memr[buf], Memr[SY(sh)], n)
		call mfree (buf, TY_REAL)
	    }
	    W0(sh) = Memr[SX(sh)]
	    W1(sh) = Memr[SX(sh)+n-1]
	    SN(sh) = n
	    if (n > 1)
		WP(sh) = (W1(sh) - W0(sh)) / (SN(sh) - 1)
	    CTLW(sh) = NULL
	    CTWL(sh) = NULL
	}
end


# SHDR_GI -- Load an integer value from the header

procedure shdr_gi (im, field, default, ival)

pointer	im
char	field[ARB]
int	default
int	ival

int	dummy, imaccf(), imgeti()

begin
	ival = default
	if (imaccf (im, field) == YES) {
	    iferr (dummy = imgeti (im, field))
		call erract (EA_WARN)
	    else
		ival = dummy
	}
end


# SHDR_GR -- Load a real value from the header

procedure shdr_gr (im, field, default, rval)

pointer	im
char	field[ARB]
real	default
real	rval

int	imaccf()
real	dummy, imgetr()

begin
	rval = default
	if (imaccf (im, field) == YES) {
	    iferr (dummy = imgetr (im, field))
		call erract (EA_WARN)
	    else
		rval = dummy
	}
end


# SHDR_GWATTRS -- Get spectrum attribute parameters

procedure shdr_gwattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
	coeff)

pointer	mw				# MWCS pointer
int	line				# Physical line number
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
double	aplow, aphigh			# Aperture limits
pointer	coeff				# Nonlinear coeff string (input/output)

int	i, j, sz_coeff, strlen(), ctoi(), ctod()
pointer	sp, key
errchk	mw_gwattrs

data	sz_coeff /SZ_LINE/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	if (coeff == NULL)
	    call malloc (coeff, sz_coeff, TY_CHAR)
	else
	    call realloc (coeff, sz_coeff, TY_CHAR)

	call sprintf (Memc[key], SZ_FNAME, "spec%d")
	    call pargi (line)

	call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	while (strlen (Memc[coeff]) == sz_coeff) {
	    sz_coeff = 2 * sz_coeff
	    call realloc (coeff, sz_coeff, TY_CHAR)
	    call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	}

	i = 1
	j = ctoi (Memc[coeff], i, ap)
	j = ctoi (Memc[coeff], i, beam)
	j = ctoi (Memc[coeff], i, dtype)
	j = ctod (Memc[coeff], i, w1)
	j = ctod (Memc[coeff], i, dw)
	j = ctoi (Memc[coeff], i, nw)
	j = ctod (Memc[coeff], i, z)
	j = ctod (Memc[coeff], i, aplow)
	j = ctod (Memc[coeff], i, aphigh)
	if (Memc[coeff+i-1] != EOS)
	    call strcpy (Memc[coeff+i], Memc[coeff], sz_coeff)
	else
	    Memc[coeff] = EOS

	if (j == 0)
	    call error (1, "Syntax error in spectrum attribute parameter")

	call sfree (sp)
end


# SHDR_SWATTRS -- Set spectrum attribute parameters

procedure shdr_swattrs (mw, line, ap, beam, dtype, w1, dw, nw, z, aplow, aphigh,
	coeff)

pointer	mw				# MWCS pointer
int	line				# Physical line number
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
double	aplow, aphigh			# Aperture limits
char	coeff[ARB]			# Nonlinear coeff string

int	sz_val, strlen()
pointer	sp, key, val

begin
	sz_val = strlen (coeff) + SZ_LINE

	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, sz_val, TY_CHAR)

	# We can't use SPRINTF for the whole string because it can only
	# handle a limited length and trucates long coefficient strings.
	# Use STRCAT instead.

	call sprintf (Memc[key], SZ_FNAME, "spec%d")
	    call pargi (line)
	call sprintf (Memc[val], sz_val, "%d %d %d %g %g %d %g %.2f %.2f")
	    call pargi (ap)
	    call pargi (beam)
	    call pargi (dtype)
	    call pargd (w1)
	    call pargd (dw)
	    call pargi (nw)
	    call pargd (z)
	    call pargd (aplow)
	    call pargd (aphigh)
	if (coeff[1] != EOS) {
	    call strcat (" ", Memc[val], sz_val)
	    call strcat (coeff, Memc[val], sz_val)
	}
	call mw_swattrs (mw, 2, Memc[key], Memc[val])

	call sfree (sp)
end
