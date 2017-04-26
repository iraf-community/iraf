include	<error.h>
include <imhdr.h>
include	<imset.h>
include	<smw.h>
include	<units.h>
include	<funits.h>
include	<math/iminterp.h>


# SHDR_OPEN    -- Open the SHDR spectrum header structure.
# SHDR_TYPE    -- Determine spectrum type.
# SHDR_GTYPE   -- Get the selected spectrum type.
# SHDR_CLOSE   -- Close and free the SHDR structure.
# SHDR_COPY    -- Make a copy of an SHDR structure.
# SHDR_SYSTEM  -- Set or change the WCS system.
# SHDR_UNITS   -- Set or change user units.
# SHDR_LW      -- Logical to world coordinate transformation.
# SHDR_WL      -- World to logical coordinate transformation.
# SHDR_REBIN   -- Rebin spectrum to dispersion of reference spectrum.
# SHDR_LINEAR  -- Rebin spectrum to linear dispersion.
# SHDR_EXTRACT -- Extract a specific wavelength region.
# SHDR_GI      -- Load an integer value from the header.
# SHDR_GR      -- Load a real value from the header.


# SHDR_OPEN -- Open SHDR spectrum header structure.
#
# This routine sets header information, WCS transformations, and extracts the
# spectrum from EQUISPEC, MULTISPEC, and NDSPEC format images.  The spectrum
# from a 2D/3D format is specified by a logical line and band number.
# Optionally an EQUISPEC or MULTISPEC spectrum may be selected by it's
# aperture number.  The access modes are header only or header and data.
# Special checks are made to avoid repeated setting of the header and WCS
# information common to all spectra in an image provided the previously set
# structure is input.  Note that the logical to world and world to logical
# transformations require that the MWCS pointer not be closed.

procedure shdr_open (im, smw, index1, index2, ap, mode, sh)

pointer	im			# IMIO pointer
pointer	smw			# SMW pointer
int	index1			# Image index desired
int	index2			# Image index desired
int	ap			# Aperture number desired
int	mode			# Access mode
pointer	sh			# SHDR pointer

int	i, j, k, l, n, np, np1, np2, aplow[2], aphigh[2], strncmp()
real	smw_c1tranr(), asumr()
double	dval, shdr_lw()
bool	newim, streq()
pointer	sp, key, str, coeff, mw, ct, buf
pointer	smw_sctran(), imgs3r(), un_open(), fun_open()
errchk	smw_sctran, imgstr, imgeti, imgetr, smw_gwattrs
errchk	un_open, fun_open, fun_ctranr, imgs3r, shdr_gtype

define	data_	90

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate basic structure or check if the same spectrum is requested.
	if (sh == NULL) {
	    call calloc (sh, LEN_SHDR, TY_STRUCT)
	    call calloc (SID(sh,1), LEN_SHDRS, TY_CHAR)
	    newim = true
	} else {
	    call imstats (im, IM_IMAGENAME, Memc[str], SZ_LINE)
	    newim = !streq (Memc[str], IMNAME(sh))
	    if (!newim) {
		if (LINDEX(sh,1)==index1 && LINDEX(sh,2)==index2) {
		    if (IS_INDEFI(ap) || AP(sh)==ap) {
			np1 = NP1(sh)
			np2 = NP2(sh)
			np = np2 - np1 + 1
			if (CTLW(sh) == NULL || CTWL(sh) == NULL)
			    goto data_
			if (mode == SHHDR) {
			    do i = 1, SH_NTYPES
				call mfree (SPEC(sh,i), TY_REAL)
			} else {
			    switch (SMW_FORMAT(smw)) {
			    case SMW_ND:
				if (mode == SHDATA && SPEC(sh,mode) == NULL)
				    goto data_
			    case SMW_ES, SMW_MS:
				if (SPEC(sh,mode) == NULL)
				    goto data_
			    }
			}
			call sfree (sp)
			return
		    }
		}
	    }
	}

	# Set parameters common to an entire image.
	if (newim) {
	    call imstats (im, IM_IMAGENAME, IMNAME(sh), LEN_SHDRS)
	    IM(sh) = im
	    MW(sh) = smw

	    # Get standard parameters.
	    call shdr_gi (im, "OFLAG", OBJECT, OFLAG(sh))
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
	    iferr (call imgstr (im, "DEREDDEN", RC(sh), LEN_SHDRS))
		RC(sh) = EOS

	    # Flag bad airmass value; i.e. 0.
	    if (!IS_INDEF (AM(sh)) && AM(sh) < 1.)
		AM(sh) = INDEF

	    # Set the SMW information.
	    if (SMW_FORMAT(smw) == SMW_MS)
		i = 3B
	    else
		i = 2 ** (SMW_PAXIS(smw,1) - 1)
	    CTLW1(sh) = smw_sctran (smw, "logical", "world", i)
	    CTWL1(sh) = smw_sctran (smw, "world", "logical", i)

	    # Set the units.
	    mw = SMW_MW(smw,0)
	    i = SMW_PAXIS(smw,1)
	    iferr (call mw_gwattrs (mw, i, "label", LABEL(sh),LEN_SHDRS))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    if (streq (LABEL(sh), "equispec") || streq (LABEL(sh), "multispe"))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    iferr (call mw_gwattrs (mw, i, "units", UNITS(sh),LEN_SHDRS)) {
		call sprintf (Memc[key], SZ_FNAME, "cunit%d")
		    call pargi (i)
		iferr (call imgstr (im, Memc[key], UNITS(sh), LEN_SHDRS)) {
		    call strlwr (LABEL(sh))
		    if (LABEL(sh) == EOS)
			call strcpy ("", UNITS(sh), LEN_SHDRS)
		    else if (streq (LABEL(sh), "lambda"))
			call strcpy ("angstroms", UNITS(sh), LEN_SHDRS)
		    else if (streq (LABEL(sh), "freq"))
			call strcpy ("hertz", UNITS(sh), LEN_SHDRS)
		    else if (strncmp (LABEL(sh), "velo", 4) == 0)
			call strcpy ("m/s", UNITS(sh), LEN_SHDRS)
		    else if (streq (LABEL(sh), "waveleng"))
			call strcpy ("angstroms", UNITS(sh), LEN_SHDRS)
		    else
			call strcpy ("", UNITS(sh), LEN_SHDRS)
		}
		if (strncmp (LABEL(sh), "velo", 4) == 0)
		    call strcat (" 21 centimeters", UNITS(sh), LEN_SHDRS)
	    }
	    if (UNITS(sh) == EOS && DC(sh) != DCNO)
		call strcpy ("Angstroms", UNITS(sh), LEN_SHDRS)
	    MWUN(sh) = un_open (UNITS(sh))
	    call un_copy (MWUN(sh), UN(sh))

	    iferr (call imgstr (im, "bunit", Memc[str], SZ_LINE))
		call strcpy ("", Memc[str], SZ_LINE)
	    FUNIM(sh) = fun_open (Memc[str])
	    if (FUN_CLASS(FUNIM(sh)) != FUN_UNKNOWN)
		FC(sh) = FCYES

	    call fun_copy (FUNIM(sh), FUN(sh))
	    call strcpy (FUN_LABEL(FUN(sh)), FLABEL(sh), LEN_SHDRS)
	    call strcpy (FUN_UNITS(FUN(sh)), FUNITS(sh), LEN_SHDRS)
	}

	# Set WCS parameters for spectrum type.
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    # Set physical and logical indices.
	    if (!IS_INDEFI (ap)) {
		i = max (1, min (SMW_NSPEC(smw), ap))
		j = 1
	    } else {
		i = max (1, index1)
		j = max (1, index2)
	    }
	    call smw_mw (smw, i, j, mw, k, l)

	    LINDEX(sh,1) = max (1, min (SMW_LLEN(smw,2), k))
	    LINDEX(sh,2) = max (1, min (SMW_LLEN(smw,3), l))
	    PINDEX(sh,1) = LINDEX(sh,1)
	    PINDEX(sh,2) = LINDEX(sh,2)
	    APINDEX(sh) = LINDEX(sh,1)

	    # Set aperture information.  Note the use of the logical index.
	    np1 = 1
	    call smw_gwattrs (smw, i, j, AP(sh), BEAM(sh), DC(sh),
		dval, dval, np2, dval, APLOW(sh,1), APHIGH(sh,1), coeff) 

	    call smw_gapid (smw, i, j, TITLE(sh), LEN_SHDRS)
	    Memc[SID(sh,1)] = EOS

	    switch (SMW_LDIM(smw)) {
	    case 1:
		IMSEC(sh) = EOS
	    case 2:
		if (APLOW(sh,1) == APHIGH(sh,1)) {
		    if (SMW_LAXIS(smw,1) == 1)
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d]")
		    else
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d,*]")
		    call pargi (nint (APLOW(sh,1)))
		} else {
		    if (SMW_LAXIS(smw,1) == 1)
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d:%d]")
		    else
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d:%d,*]")
		    call pargi (nint (APLOW(sh,1)))
		    call pargi (nint (APHIGH(sh,1)))
		}
	    case 3:
		if (APLOW(sh,1)==APHIGH(sh,1) && APLOW(sh,2)==APHIGH(sh,2)) {
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d,%d]")
		    case 2:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d,*,%d]")
		    case 3:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d,%d,*]")
		    }
		    call pargi (nint (APLOW(sh,1)))
		    call pargi (nint (APLOW(sh,2)))
		} else if (APLOW(sh,1) == APHIGH(sh,1)) {
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d,%d:%d]")
		    case 2:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d,*,%d:%d]")
		    case 3:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d,%d:%d,*]")
		    }
		    call pargi (nint (APLOW(sh,1)))
		    call pargi (nint (APLOW(sh,2)))
		    call pargi (nint (APHIGH(sh,2)))
		} else if (APLOW(sh,2) == APHIGH(sh,2)) {
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d:%d,%d]")
		    case 2:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d:%d,*,%d]")
		    case 3:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d:%d,%d,*]")
		    }
		    call pargi (nint (APLOW(sh,1)))
		    call pargi (nint (APHIGH(sh,1)))
		    call pargi (nint (APLOW(sh,2)))
		} else {
		    switch (SMW_LAXIS(smw,1)) {
		    case 1:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d:%d,%d:%d]")
		    case 2:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d:%d,*,%d:%d]")
		    case 3:
			call sprintf (IMSEC(sh), LEN_SHDRS, "[%d:%d,%d:%d,*]")
		    }
		    call pargi (nint (APLOW(sh,1)))
		    call pargi (nint (APHIGH(sh,1)))
		    call pargi (nint (APLOW(sh,2)))
		    call pargi (nint (APHIGH(sh,2)))
		}
	    }

	case SMW_ES, SMW_MS:
	    # Set the image and aperture indices.
	    if (SMW_PAXIS(smw,2) != 3) {
		PINDEX(sh,1) = max (1, min (SMW_LLEN(smw,2), index1))
		PINDEX(sh,2) = max (1, min (SMW_LLEN(smw,3), index2))
		LINDEX(sh,1) = PINDEX(sh,1)
		LINDEX(sh,2) = PINDEX(sh,2)
		APINDEX(sh) = LINDEX(sh,1)
	    } else {
		PINDEX(sh,1) = 1
		PINDEX(sh,2) = max (1, min (SMW_LLEN(smw,2), index2))
		LINDEX(sh,1) = PINDEX(sh,2)
		LINDEX(sh,2) = 1
		APINDEX(sh) = 1
	    }

	    # If an aperture is specified first try and find it.
	    # If it is not specified or found then use the physical index.

	    coeff = NULL
	    AP(sh) = 0
	    if (!IS_INDEFI(ap)) {
		do i = 1, SMW_NSPEC(smw) {
		    call smw_gwattrs (smw, i, 1, AP(sh), BEAM(sh), DC(sh),
			dval, dval, np2, dval, APLOW(sh,1), APHIGH(sh,1), coeff)
		    if (AP(sh) == ap && SMW_PAXIS(smw,2) != 3) {
			PINDEX(sh,1) = i
			LINDEX(sh,1) = i
			APINDEX(sh) = i
			break
		    }
		}
	    }
	    if (AP(sh) != ap)
		call smw_gwattrs (smw, APINDEX(sh), 1, AP(sh), BEAM(sh), DC(sh),
		    dval, dval, np2, dval, APLOW(sh,1), APHIGH(sh,1), coeff) 
	    call mfree (coeff, TY_CHAR)

	    np1 = 1
	    if (SMW_PDIM(smw) > 1) {
		ct = smw_sctran (smw, "logical", "physical", 2)
		PINDEX(sh,1) = nint (smw_c1tranr (ct, real(PINDEX(sh,1))))
		call smw_ctfree (ct)
	    }
	    if (SMW_PDIM(smw) > 2) {
		ct = smw_sctran (smw, "logical", "physical", 4)
		PINDEX(sh,2) = nint (smw_c1tranr (ct, real(PINDEX(sh,2))))
		call smw_ctfree (ct)
	    }

	    call smw_gapid (smw, APINDEX(sh), 1, TITLE(sh), LEN_SHDRS)
	    call shdr_type (sh, 1, PINDEX(sh,2))

	    switch (SMW_LDIM(smw)) {
	    case 1:
		IMSEC(sh) = EOS
	    case 2:
		call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d]")
		call pargi (APINDEX(sh))
	    case 3:
		call sprintf (IMSEC(sh), LEN_SHDRS, "[*,%d,%d]")
		call pargi (APINDEX(sh))
		call pargi (LINDEX(sh,2))
	    }
	}
	
	# Set NP1 and NP2 in logical coordinates.
	i = 2 ** (SMW_PAXIS(smw,1) - 1)
	ct = smw_sctran (smw, "physical", "logical", i)
	i = max (1, min (int (smw_c1tranr (ct, real (np1))), SMW_LLEN(smw,1)))
	j = max (1, min (int (smw_c1tranr (ct, real (np2))), SMW_LLEN(smw,1)))
	call smw_ctfree (ct)
	np1 = min (i, j)
	np2 = max (i, j)
	np = np2 - np1 + 1

	NP1(sh) = np1
	NP2(sh) = np2
	SN(sh) = np


data_	# Set the coordinate and data arrays if desired otherwise free them.
	CTLW(sh) = CTLW1(sh)
	CTWL(sh) = CTWL1(sh)

	# Set linear dispersion terms.
	W0(sh) = shdr_lw (sh, double(1))
	W1(sh) = shdr_lw (sh, double(np))
	WP(sh) = (W1(sh) - W0(sh)) / (np - 1)
	SN(sh) = np

	if (mode == SHHDR) {
	    do i = 1, SH_NTYPES
		call mfree (SPEC(sh,i), TY_REAL)
	    call sfree (sp)
	    return
	}

	# Set WCS array
	if (SX(sh) == NULL)
	    call malloc (SX(sh), np, TY_REAL)
	else
	    call realloc (SX(sh), np, TY_REAL)
	do i = 1, np
	    Memr[SX(sh)+i-1] = shdr_lw (sh, double(i))

	# Set spectrum array in most efficient way.
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    if (mode == SHDATA || SY(sh) == NULL) {
		if (SY(sh) == NULL)
		    call malloc (SY(sh), np, TY_REAL)
		else
		    call realloc (SY(sh), np, TY_REAL)
		call aclrr (Memr[SY(sh)], np)
		if (IS_INDEF(APLOW(sh,1)))
		    aplow[1] = 1
		else
		    aplow[1] = nint (APLOW(sh,1))
		if (IS_INDEF(APHIGH(sh,1)))
		    aphigh[1] = 1
		else
		    aphigh[1] = nint (APHIGH(sh,1))
		if (IS_INDEF(APLOW(sh,2)))
		    aplow[2] = 1
		else
		    aplow[2] = nint (APLOW(sh,2))
		if (IS_INDEF(APHIGH(sh,2)))
		    aphigh[2] = 1
		else
		    aphigh[2] = nint (APHIGH(sh,2))
		k = aplow[1]
		l = aphigh[1]
		n = aphigh[1] - aplow[1] + 1
		if (SMW_LAXIS(smw,1) == 1) {
		    do j = aplow[2], aphigh[2] {
			do i = aplow[1], aphigh[1] {
			    buf = imgs3r (im, np1, np2, i, i, j, j)
			    call aaddr (Memr[buf], Memr[SY(sh)],
				 Memr[SY(sh)], np)
			}
		    }
		} else if (SMW_LAXIS(smw,1) == 2) {
		    do j = aplow[2], aphigh[2] {
			do i = np1, np2 {
			    buf = imgs3r (im, k, l, i, i, j, j)
			    Memr[SY(sh)+i-np1] = Memr[SY(sh)+i-np1] +
				asumr (Memr[buf], n)
			}
		    }
		} else {
		    do i = np1, np2 {
			do j = aplow[2], aphigh[2] {
			    buf = imgs3r (im, k, l, j, j, i, i)
			    Memr[SY(sh)+i-np1] = Memr[SY(sh)+i-np1] +
				asumr (Memr[buf], n)
			}
		    }
		}
	    }
	case SMW_ES, SMW_MS:
	    if (mode == SHDATA || SY(sh) == NULL) {
		if (SY(sh) == NULL)
		    call malloc (SY(sh), np, TY_REAL)
		else
		    call realloc (SY(sh), np, TY_REAL)
		i = LINDEX(sh,1)
		j = LINDEX(sh,2)
		buf = imgs3r (im, np1, np2, i, i, j, j)
		call amovr (Memr[buf], Memr[SY(sh)], np)
	    }

	    if (mode > SHDATA)
		call shdr_gtype (sh, mode)
	}

	# Guess flux scale if necessary.
	if (FC(sh) == FCYES && FUN_CLASS(FUNIM(sh)) == FUN_UNKNOWN) {
	    if (Memr[SY(sh)+np/2] < 1e-18)
		call strcpy ("erg/cm2/s/Hz", Memc[str], SZ_LINE)
	    else if (Memr[SY(sh)+np/2] < 1e-5)
		call strcpy ("erg/cm2/s/A", Memc[str], SZ_LINE)
	    call fun_close (FUNIM(sh))
	    FUNIM(sh) = fun_open (Memc[str])
	    if (FUN_CLASS(FUN(sh)) == FUN_UNKNOWN) {
		call fun_copy (FUNIM(sh), FUN(sh))
		call strcpy (FUN_LABEL(FUN(sh)), FLABEL(sh), LEN_SHDRS)
		call strcpy (FUN_UNITS(FUN(sh)), FUNITS(sh), LEN_SHDRS)
	    }
	}
	if (SPEC(sh,mode) != 0)
	    iferr (call fun_ctranr (FUNIM(sh), FUN(sh), UN(sh), Memr[SX(sh)],
		Memr[SPEC(sh,mode)], Memr[SPEC(sh,mode)], SN(sh)))
		;

	call sfree (sp)
end


# SHDR_GTYPE -- Get the selected spectrum type.
# Currently this only works for multispec data.

procedure shdr_gtype (sh, type)

pointer	sh			#I SHDR pointer
int	type			#I Spectrum type

int	i, j, ctowrd(), strdic()
pointer	sp, key, str, im, smw, ct, buf, smw_sctran(), imgs3r()
real	smw_c1tranr()

begin
	im = IM(sh)
	smw = MW(sh)

	if (SMW_FORMAT(smw) == SMW_ND)
	    return
	if (SMW_PDIM(smw) < 3) {
	    if (type != SHDATA && type != SHRAW) {
		if (SID(sh,type) != NULL)
		    call mfree (SID(sh,type), TY_CHAR)
		if (SPEC(sh,type) != NULL)
		    call mfree (SPEC(sh,type), TY_REAL)
	    }
	    return
	}

	# Find the band.
	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	do i = 1, 5 {
	    call sprintf (Memc[key], SZ_LINE, "BANDID%d")
		call pargi (i)
	    ifnoerr (call imgstr (im, Memc[key], Memc[str], SZ_LINE)) {
		j = 1
		if (ctowrd (Memc[str], j, Memc[key], SZ_LINE) == 0)
		    next
		if (strdic (Memc[key], Memc[key], SZ_LINE, STYPES) != type)
		    next
		if (SID(sh,type) == NULL)
		    call malloc (SID(sh,type), LEN_SHDRS, TY_CHAR)
		call strcpy (Memc[str], Memc[SID(sh,type)], LEN_SHDRS)
		STYPE(sh,type) = type
		break
	    }
	}
	call sfree (sp)
	if (i == 6) {
	    if (SID(sh,type) != NULL)
		call mfree (SID(sh,type), TY_CHAR)
	    if (SPEC(sh,type) != NULL)
		call mfree (SPEC(sh,type), TY_REAL)
	    return
	}

	# Map the physical band to logical vector.
	ct = smw_sctran (smw, "physical", "logical", 4)
	i = nint (smw_c1tranr (ct, real(i)))
	call smw_ctfree (ct)
	if (SMW_PAXIS(smw,2) != 3) {
	    if (i > SMW_LLEN(smw,3))
		return
	    j = i
	    i = LINDEX(sh,1)
	} else {
	    if (i > SMW_LLEN(smw,2))
		return
	    j = 1
	}

	# Get the spectrum.
	if (SPEC(sh,type) == NULL)
	    call malloc (SPEC(sh,type), SN(sh), TY_REAL)
	else
	    call realloc (SPEC(sh,type), SN(sh), TY_REAL)
	buf = imgs3r (im, NP1(sh), NP2(sh), i, i, j, j)
	call amovr (Memr[buf], Memr[SPEC(sh,type)], SN(sh))
end


# SHDR_TYPE -- Determine the spectrum type.
# Currently this only works for multispec data.

procedure shdr_type (sh, index, band)

pointer	sh			#I SHDR pointer
int	index			#I Index
int	band			#I Physical band

int	i, ctowrd(), strdic()
pointer	sp, key

begin
	if (SMW_FORMAT(MW(sh)) == SMW_ND)
	    return

	call smark (sp)
	call salloc (key, SZ_LINE, TY_CHAR)

	if (SID(sh,index) == NULL)
	    call malloc (SID(sh,index), LEN_SHDRS, TY_CHAR)

	call sprintf (Memc[key], SZ_FNAME, "BANDID%d")
	    call pargi (band)
	 iferr (call imgstr (IM(sh), Memc[key], Memc[SID(sh,index)], LEN_SHDRS))
	    Memc[SID(sh,index)] = EOS

	i = 1
	if (ctowrd (Memc[SID(sh,index)], i, Memc[key], SZ_LINE) > 0)
	    STYPE(sh,index) = strdic (Memc[key], Memc[key], SZ_LINE, STYPES)
	else
	    STYPE(sh,index) = 0

	call sfree (sp)
end


# SHDR_CLOSE -- Close and free the SHDR structure.

procedure shdr_close (sh)

pointer	sh			# SHDR structure
int	i

begin
	if (sh == NULL)
	    return
	do i = 1, SH_NTYPES {
	    call mfree (SPEC(sh,i), TY_REAL)
	    call mfree (SID(sh,i), TY_CHAR)
	}
	call un_close (UN(sh))
	call un_close (MWUN(sh))
	call fun_close (FUN(sh))
	call fun_close (FUNIM(sh))
	if (MW(sh) != NULL) {
	    call smw_ctfree (CTLW1(sh))
	    call smw_ctfree (CTWL1(sh))
	}
	call mfree (sh, TY_STRUCT)
end


# SHDR_COPY -- Make a copy of an SHDR structure.
# The image pointer is not copied and the MWCS pointer and transform pointers
# may or may not be copied .  The uncopied pointers mean that they will be
# shared by multiple spectrum structures but it also means that when they are
# closed the structures will have invalid pointers.  The advantage of not
# copying is that many spectra may come from the same image and the overhead
# of having copies of the IMIO and MWCS pointers can be avoided.

procedure shdr_copy (sh1, sh2, wcs)

pointer	sh1		# SHDR structure to copy
pointer	sh2		# SHDR structure copy
int	wcs		# Make copy of wcs?

int	i
pointer	un, mwun, fun, funim, spec[SH_NTYPES], sid[SH_NTYPES], smw_newcopy()
errchk	shdr_system

begin
	if (sh2 == NULL) {
	    call calloc (sh2, LEN_SHDR, TY_STRUCT)
	    call calloc (SID(sh2,1), LEN_SHDRS, TY_CHAR)
	}

	un = UN(sh2)
	mwun = MWUN(sh2)
	fun = FUN(sh2)
	funim = FUNIM(sh2)
	call amovi (SPEC(sh2,1), spec, SH_NTYPES)
	call amovi (SID(sh2,1), sid, SH_NTYPES)
	call amovi (Memi[sh1], Memi[sh2], LEN_SHDR)
	call amovi (spec, SPEC(sh2,1), SH_NTYPES)
	call amovi (sid, SID(sh2,1), SH_NTYPES)
	UN(sh2) = un
	MWUN(sh2) = mwun
	FUN(sh2) = fun
	FUNIM(sh2) = funim
	call un_copy (UN(sh1), UN(sh2))
	call un_copy (MWUN(sh1), MWUN(sh2))
	call fun_copy (FUN(sh1), FUN(sh2))
	call fun_copy (FUNIM(sh1), FUNIM(sh2))
	do i = 1, SH_NTYPES {
	    if (SPEC(sh1,i) != NULL) {
		if (SPEC(sh2,i) == NULL)
		    call malloc (SPEC(sh2,i), SN(sh1), TY_REAL)
		else
		    call realloc (SPEC(sh2,i), SN(sh1), TY_REAL)
		call amovr (Memr[SPEC(sh1,i)], Memr[SPEC(sh2,i)], SN(sh1))
	    }
	}

	if (wcs == YES && MW(sh1) != NULL) {
	    MW(sh2) = smw_newcopy (MW(sh1))
	    CTLW1(sh2) = NULL
	    CTWL1(sh2) = NULL
	    call shdr_system (sh2, "world")
	}
end


# SHDR_SYSTEM -- Set or change the WCS system.

procedure shdr_system (sh, system)

pointer	sh			# SHDR pointer
char	system[ARB]		# System

int	i, sn
bool	streq()
double	shdr_lw()
pointer	smw, mw, smw_sctran(), un_open()
errchk	smw_sctran, un_open

begin
	smw = MW(sh)
	if (smw == NULL)
	    call error (1, "shdr_system: MWCS not defined")

	call smw_ctfree (CTLW1(sh))
	call smw_ctfree (CTWL1(sh))

	switch (SMW_FORMAT(smw)) {
	case SMW_ND, SMW_ES:
	    i = 2 ** (SMW_PAXIS(smw,1) - 1)
	    CTLW1(sh) = smw_sctran (smw, "logical", system, i)
	    CTWL1(sh) = smw_sctran (smw, system, "logical", i)
	case SMW_MS:
	    CTLW1(sh) = smw_sctran (smw, "logical", system, 3B)
	    CTWL1(sh) = smw_sctran (smw, system, "logical", 3B)
	}
	CTLW(sh) = CTLW1(sh)
	CTWL(sh) = CTWL1(sh)

	# Set labels and units
	call un_close (MWUN(sh))
	if (streq (system, "physical")) {
	    call strcpy ("Pixel", LABEL(sh), LEN_SHDRS)
	    call strcpy ("", UNITS(sh), LEN_SHDRS)
	    MWUN(sh) = un_open (UNITS(sh))
	} else {
	    call smw_mw (smw, 1, 1, mw, i, i)
	    iferr (call mw_gwattrs (mw, SMW_PAXIS(smw,1), "label", LABEL(sh),
		LEN_SHDRS))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    if (streq (LABEL(sh), "equispec") || streq (LABEL(sh), "multispe"))
		call strcpy ("", LABEL(sh), LEN_SHDRS)
	    iferr (call mw_gwattrs (mw, SMW_PAXIS(smw,1), "units", UNITS(sh),
		LEN_SHDRS))
		call strcpy ("", UNITS(sh), LEN_SHDRS)
	    MWUN(sh) = un_open (UNITS(sh))
	    call strcpy (UN_LABEL(UN(sh)), LABEL(sh), LEN_SHDRS)
	    call strcpy (UN_UNITS(UN(sh)), UNITS(sh), LEN_SHDRS)
	}

	sn = SN(sh)
	W0(sh) = shdr_lw (sh, double(1))
	W1(sh) = shdr_lw (sh, double(sn))
	WP(sh) = (W1(sh) - W0(sh)) / (sn - 1)
	if (SX(sh) != NULL)
	    do i = 1, sn
		Memr[SX(sh)+i-1] = shdr_lw (sh, double(i))
end


# SHDR_UNITS -- Set or change the WCS system.
# This changes W0, W1, WP, and SX.

procedure shdr_units (sh, units)

pointer	sh			# SHDR pointer
char	units[ARB]		# Units

int	i, sn
bool	streq()
double	shdr_lw()
pointer	str, un, un_open()
errchk	un_open

begin
	# Check for unknown units.
	if (streq (units, "display")) {
	    call malloc (str, SZ_LINE, TY_CHAR)
	    iferr (call mw_gwattrs (SMW_MW(MW(sh),0), SMW_PAXIS(MW(sh),1),
		"units_display", Memc[str], SZ_LINE)) {
		un = NULL
		call un_copy (MWUN(sh), un)
	    } else
		un = un_open (Memc[str])
	    call mfree (str, TY_CHAR)
	} else if (streq (units, "default")) {
	    un = NULL
	    call un_copy (MWUN(sh), un)
	} else
	    un = un_open (units)
	if (UN_CLASS(un) == UN_UNKNOWN || UN_CLASS(MWUN(sh)) == UN_UNKNOWN) {
	    call un_close (un)
	    call error (1, "Cannot convert to specified units")
	}

	# Update the coordinates.
	call un_close (UN(sh))
	UN(sh) = un

	call strcpy (UN_LABEL(UN(sh)), LABEL(sh), LEN_SHDRS)
	call strcpy (UN_UNITS(UN(sh)), UNITS(sh), LEN_SHDRS)

	sn = SN(sh)
	W0(sh) = shdr_lw (sh, double(1))
	W1(sh) = shdr_lw (sh, double(sn))
	WP(sh) = (W1(sh) - W0(sh)) / (sn - 1)
	if (SX(sh) != NULL)
	    do i = 1, sn
		Memr[SX(sh)+i-1] = shdr_lw (sh, double(i))
end


# SHDR_LW -- Logical to world coordinate transformation.
# The transformation pointer is generally NULL only after SHDR_LINEAR

double procedure shdr_lw (sh, l)

pointer	sh			# SHDR pointer
double	l			# Logical coordinate
double	w			# World coordinate

double	l0, l1, l2, w1, smw_c1trand()

begin
	l0 = l + NP1(sh) - 1
	if (CTLW(sh) != NULL) {
	    switch (SMW_FORMAT(MW(sh))) {
	    case SMW_ND, SMW_ES:
		w = smw_c1trand (CTLW(sh), l0)
	    case SMW_MS:
		call smw_c2trand (CTLW(sh), l0, double (APINDEX(sh)), w, w1)
	    }
	} else {
	    switch (DC(sh)) {
	    case DCLOG:
		w = W0(sh) * 10. ** (log10(W1(sh)/W0(sh)) * (l0-1) / (SN(sh)-1))
	    case DCFUNC:
		w = W0(sh)
		call smw_c2trand (CTWL1(sh), w, double (AP(sh)), l1, w1)
		w = W1(sh)
		call smw_c2trand (CTWL1(sh), w, double (AP(sh)), l2, w1)
		if (SN(sh) > 1)
		    l1 = (l2 - l1) / (SN(sh) - 1) * (l0 - 1) + l1
		else
		    l1 = l0 - 1 + l1
		call smw_c2trand (CTLW1(sh), l1, double (APINDEX(sh)), w, w1)
	    default:
		w = W0(sh) + (l0 - 1) * WP(sh)
	    }
	}

	iferr (call un_ctrand (MWUN(sh), UN(sh), w, w, 1))
	    ;
	return (w)
end


# SHDR_WL -- World to logical coordinate transformation.
# The transformation pointer is generally NULL only after SHDR_LINEAR

double procedure shdr_wl (sh, w)

pointer	sh			# SHDR pointer
double	w			# World coordinate
double	l			# Logical coordinate

double	w1, l1, l2, smw_c1trand()

begin
	iferr (call un_ctrand (UN(sh), MWUN(sh), w, w1, 1))
	    w1 = w

	if (CTWL(sh) != NULL) {
	    switch (SMW_FORMAT(MW(sh))) {
	    case SMW_ND, SMW_ES:
		l = smw_c1trand (CTWL(sh), w1)
	    case SMW_MS:
		call smw_c2trand (CTWL(sh), w1, double (AP(sh)),l,l1)
	    }
	} else {
	    switch (DC(sh)) {
	    case DCLOG:
		l = log10(w1/W0(sh)) / log10(W1(sh)/W0(sh)) * (SN(sh)-1) + 1
	    case DCFUNC:
		call smw_c2trand (CTWL1(sh), w1, double (AP(sh)), l, l1)

		w1 = W0(sh)
		call smw_c2trand (CTWL1(sh), w1, double (AP(sh)), l1, w1)
		w1 = W1(sh)
		call smw_c2trand (CTWL1(sh), w1, double (AP(sh)), l2, w1)
		if (l1 != l2)
		    l = (SN(sh) - 1) / (l2 - l1) * (l - l1) + 1
		else
		    l = l - l1 + 1
	    default:
		l = (w1 - W0(sh)) / WP(sh) + 1
	    }
	}

	return (l-NP1(sh)+1)
end


# SHDR_REBIN -- Rebin spectrum to dispersion of reference spectrum.
# The interpolation function is set by ONEDINTERP.

procedure shdr_rebin (sh, shref)

pointer	sh		# Spectrum to be rebinned
pointer	shref		# Reference spectrum

char	interp[10]
int	i, j, type, ia, ib, n, clgwrd()
real	a, b, sum, asieval(), asigrl()
double	x, w, xmin, xmax, shdr_lw(), shdr_wl()
pointer	unref, unsave, asi, spec
bool	fp_equalr()

begin
	# Check if rebinning is needed
	if (DC(sh) == DC(shref) && DC(sh) != DCFUNC &&
	    fp_equalr (W0(sh), W0(shref)) && fp_equalr(WP(sh), WP(shref)) &&
	    SN(sh) == SN(shref))
	    return

	# Do everything in units of reference WCS.
	unref = UN(shref)
	unsave = UN(sh)
	UN(SH) = unref

	call asiinit (asi, clgwrd ("interp", interp, 10, II_FUNCTIONS))
	do type = 1, SH_NTYPES {
	    if (SPEC(sh,type) == NULL)
		next

	    # Fit the interpolation function to the spectrum.
	    # Extend the interpolation by one pixel at each end.

	    n = SN(sh)
	    call malloc (spec, n+2, TY_REAL)
	    call amovr (Memr[SPEC(sh,type)], Memr[spec+1], n)
	    Memr[spec] = Memr[SPEC(sh,type)]
	    Memr[spec+n+1] = Memr[SPEC(sh,type)+n-1]
	    call asifit (asi, Memr[spec], n+2)
	    call mfree (spec, TY_REAL)

	    xmin = 0.5
	    xmax = n + 0.5

	    # Reallocate data array
	    if (n != SN(shref)) {
		n = SN(shref)
		call realloc (SPEC(sh,type), n, TY_REAL)
		call aclrr (Memr[SPEC(sh,type)], n)
	    }
	    spec = SPEC(sh,type)

	    # Compute the average flux in each output pixel.

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
		    if (abs (a+0.5-ia) < .00001 && abs (b-0.5-ib) < .00001) {
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
		    if (abs (a-0.5-ia) < .00001 && abs (b+0.5-ib) < .00001) {
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
	}
	call asifree (asi)

	# Set the rest of the header.  The coordinate transformations are
	# canceled to indicate they are not valid for the data.  They
	# are not freed because the same pointer may be used in other
	# spectra from the same image.

	if (SN(sh) != n)
	    call realloc (SX(sh), n, TY_REAL)
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
	iferr (call un_ctranr (unref, UN(sh), Memr[SX(sh)], Memr[SX(sh)],
	    SN(sh)))
	    ;
end


# SHDR_LINEAR -- Rebin spectrum to linear dispersion.
# The interpolation function is set by ONEDINTERP

procedure shdr_linear (sh, w0, w1, sn, dc)

pointer	sh		# Spectrum to be rebinned
real	w0		# Wavelength of first logical pixel
real	w1		# Wavelength of last logical pixel
int	sn		# Number of pixels
int	dc		# Dispersion type (DCLINEAR | DCLOG)

char	interp[10]
int	i, j, type, ia, ib, n, clgwrd()
real	w0mw, w1mw, a, b, sum, asieval(), asigrl()
double	x, w, w0l, wp, xmin, xmax, shdr_wl()
pointer	unsave, asi, spec
bool	fp_equalr()

begin
	# Check if rebinning is needed
	if (DC(sh) == dc && fp_equalr (W0(sh), w0) &&
	    fp_equalr (W1(sh), w1) && SN(sh) == sn)
	    return

	# Do everything in units of MWCS.
	call un_ctranr (UN(sh), MWUN(sh), w0, w0mw, 1)
	call un_ctranr (UN(sh), MWUN(sh), w1, w1mw, 1)
	unsave = UN(sh)
	UN(SH) = MWUN(sh)

	call asiinit (asi, clgwrd ("interp", interp, 10, II_FUNCTIONS))
	do type = 1, SH_NTYPES {
	    if (SPEC(sh,type) == NULL)
		next

	    # Fit the interpolation function to the spectrum.
	    # Extend the interpolation by one pixel at each end.

	    n = SN(sh)
	    call malloc (spec, n+2, TY_REAL)
	    call amovr (Memr[SPEC(sh,type)], Memr[spec+1], n)
	    Memr[spec] = Memr[SPEC(sh,type)]
	    Memr[spec+n+1] = Memr[SPEC(sh,type)+n-1]
	    call asifit (asi, Memr[spec], n+2)
	    call mfree (spec, TY_REAL)

	    xmin = 0.5
	    xmax = n + 0.5

	    # Reallocate spectrum data array
	    if (n != sn) {
		n = sn
		call realloc (SPEC(sh,type), n, TY_REAL)
	    }
	    spec = SPEC(sh,type)

	    # Integrate across pixels using ASIGRL.

	    x = 0.5
	    if (dc == DCLOG) {
		w0l = log10 (w0mw)
		wp = (log10 (w1mw) - log10(w0mw)) / (n - 1)
		w = 10. ** (w0l+(x-1)*wp)
	    } else {
		wp = (w1mw - w0mw) / (n - 1)
		w = w0mw + (x - 1) * wp
	    }
	    x = shdr_wl (sh, w)
	    b = max (xmin, min (xmax, x)) + 1
	    do i = 1, n {
		x = i + 0.5
		if (dc == DCLOG)
		    w = 10. ** (w0l + (x - 1) * wp)
		else
		    w = w0mw + (x - 1) * wp
		x = shdr_wl (sh, w)
		a = b
		b = max (xmin, min (xmax, x)) + 1
		if (a <= b) {
		    ia = nint (a + 0.5)
		    ib = nint (b - 0.5)
		    if (abs (a+0.5-ia) < .00001 && abs (b-0.5-ib) < .00001) {
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
		    if (abs (a-0.5-ia) < .00001 && abs (b+0.5-ib) < .00001) {
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
	}
	call asifree (asi)

	# Set the rest of the header.  The coordinate transformations are
	# canceled to indicate they are not valid for the data.  They
	# are not freed because the same pointer may be used in other
	# spectra from the same image.

	if (SN(sh) != n)
	    call realloc (SX(sh), n, TY_REAL)
	do i = 0, n-1 {
	    if (dc == DCLOG)
		w = 10. ** (w0l + i * wp)
	    else
		w = w0mw + i * wp
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


# SHDR_EXTRACT -- Extract a specific wavelength region.

procedure shdr_extract (sh, w1, w2, rebin)

pointer	sh			# SHDR structure
real	w1			# Starting wavelength
real	w2			# Ending wavelength
bool	rebin			# Rebin wavelength region?

int	i, j, i1, i2, n
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
		do j = 1, SH_NTYPES
		    if (SPEC(sh,j) != NULL)
			call amovr (Memr[SPEC(sh,j)+i1-1], Memr[SPEC(sh,j)], n)
	    } else {
		call malloc (buf, n, TY_REAL)
		do j = 1, SH_NTYPES {
		    if (SPEC(sh,j) != NULL) {
			do i = i1, i2, -1
			    Memr[buf+i1-i] = Memr[SPEC(sh,j)+i-1]
			call amovr (Memr[buf], Memr[SPEC(sh,j)], n)
		    }
		}
		call mfree (buf, TY_REAL)
	    }
	    W0(sh) = Memr[SX(sh)]
	    W1(sh) = Memr[SX(sh)+n-1]
	    SN(sh) = n
	    NP1(sh) = 1
	    NP2(sh) = n
	    if (n > 1)
		WP(sh) = (W1(sh) - W0(sh)) / (SN(sh) - 1)
	    CTLW(sh) = NULL
	    CTWL(sh) = NULL
	}
end


# SHDR_GI -- Load an integer value from the header.

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


# SHDR_GR -- Load a real value from the header.

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
