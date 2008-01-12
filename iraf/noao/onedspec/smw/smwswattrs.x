include	<error.h>
include	<smw.h>


# SMW_SWATTRS -- Set spectrum attribute parameters
# This routine has the feature that if the coordinate system of a single
# spectrum in an EQUISPEC WCS is changed then the image WCS is changed
# to a MULTISPEC WCS.

procedure smw_swattrs (smw, index1, index2, ap, beam, dtype, w1, dw, nw, z,
	aplow, aphigh, coeff)

pointer	smw				# SMW pointer
int	index1				# Spectrum index
int	index2				# Spectrum index
int	ap				# Aperture number
int	beam				# Beam number
int	dtype				# Dispersion type
double	w1				# Starting coordinate
double	dw				# Coordinate interval
int	nw				# Number of valid pixels
double	z				# Redshift factor
real	aplow[2], aphigh[2]		# Aperture limits
char	coeff[ARB]			# Nonlinear coeff string

bool	fp_equald()
int	i, j, sz_val, strlen()
double	a, b
pointer	sp, str, val, mw
errchk	smw_mw

define	start_	10

begin

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

start_
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    if (!IS_INDEFI(SMW_DTYPE(smw)) && (!fp_equald(w1,SMW_W1(smw)) ||
		!fp_equald(dw,SMW_DW(smw)) || !fp_equald(z,SMW_Z(smw)))) {
		call malloc (val, 15, TY_DOUBLE)
		mw = SMW_MW(smw,0)
		i = SMW_PDIM(smw)
		j = SMW_PAXIS(smw,1)
		call mw_gwtermd (mw, Memd[val], Memd[val+i], Memd[val+2*i], i)
		Memd[val+j-1] = 1.
		switch (dtype) {
		case DCNO, DCLINEAR:
		    a = w1 / (1 + z)
		    b = dw / (1 + z)
		case DCLOG:
		    a = log10 (w1 / (1 + z))
		    b = log10 ((w1 + (nw - 1) * dw) / w1) / (nw - 1)
		case DCFUNC:
		    call error (1,
		       "Nonlinear functions not allowed for NSPEC format")
		}
		Memd[val+i+j-1] = a
		Memd[val+2*i+(i+1)*(j-1)] = b
		call mw_swtermd (mw, Memd[val], Memd[val+i], Memd[val+2*i], i)
		call mfree (val, TY_DOUBLE)
	    }
	    SMW_DTYPE(smw) = dtype
	    SMW_NW(smw) = nw
	    SMW_W1(smw) = w1
	    SMW_DW(smw) = dw
	    SMW_Z(smw) = z

	case SMW_ES:
	    # Check for any changes to the dispersion system.
	    if (dtype == DCFUNC) {
		call smw_esms(smw)
		goto start_
	    }
	    if (!IS_INDEFI(SMW_DTYPE(smw)) && (dtype != SMW_DTYPE(smw) ||
		nw != SMW_NW(smw) || !fp_equald(w1,SMW_W1(smw)) ||
		!fp_equald(dw,SMW_DW(smw)) || !fp_equald(z,SMW_Z(smw)))) {
		if (SMW_NSPEC(smw) > 1 && index1 > 0) {
		    call smw_esms(smw)
		    goto start_
		}
		if (!fp_equald(w1,SMW_W1(smw)) || !fp_equald(dw,SMW_DW(smw)) ||
		    !fp_equald(z,SMW_Z(smw))) {
		    call malloc (val, 15, TY_DOUBLE)
		    mw = SMW_MW(smw,0)
		    i = SMW_PDIM(smw)
		    j = SMW_PAXIS(smw,1)
		    call mw_gwtermd (mw, Memd[val], Memd[val+i],
			Memd[val+2*i], i)
		    Memd[val+j-1] = 1.
		    switch (dtype) {
		    case DCNO, DCLINEAR:
			a = w1 / (1 + z)
			b = dw / (1 + z)
		    case DCLOG:
			a = log10 (w1 / (1 + z))
			b = log10 ((w1 + (nw - 1) * dw) / w1) / (nw - 1)
		    }
		    Memd[val+i+j-1] = a
		    Memd[val+2*i+(i+1)*(j-1)] = b
		    call mw_swtermd (mw, Memd[val], Memd[val+i],
			Memd[val+2*i], i)
		    call mfree (val, TY_DOUBLE)
		}
	    }

	    SMW_DTYPE(smw) = dtype
	    SMW_NW(smw) = nw
	    SMW_W1(smw) = w1
	    SMW_DW(smw) = dw
	    SMW_Z(smw) = z

	    if (index1 > 0) {
		Memi[SMW_APS(smw)+index1-1] = ap
		Memi[SMW_BEAMS(smw)+index1-1] = beam
		Memr[SMW_APLOW(smw)+2*index1-2] = aplow[1]
		Memr[SMW_APHIGH(smw)+2*index1-2] = aphigh[1]
		Memr[SMW_APLOW(smw)+2*index1-1] = aplow[2]
		Memr[SMW_APHIGH(smw)+2*index1-1] = aphigh[2]
	    }

	case SMW_MS:
	    # We can't use SPRINTF for the whole string because it can only
	    # handle a limited length and trucates long coefficient strings.
	    # Use STRCAT instead.

	    call smw_mw (smw, index1, index2, mw, i, j)
	    sz_val = strlen (coeff) + SZ_LINE
	    call salloc (val, sz_val, TY_CHAR)
	    call sprintf (Memc[str], SZ_LINE, "spec%d")
		call pargi (i)
	    call sprintf (Memc[val], sz_val,
		"%d %d %d %.14g %.14g %d %.14g %.2f %.2f")
		call pargi (ap)
		call pargi (beam)
		call pargi (dtype)
		if (dtype == DCLOG) {
		    call pargd (log10 (w1))
		    call pargd (log10 ((w1+(nw-1)*dw)/w1)/(nw-1))
		} else {
		    call pargd (w1)
		    call pargd (dw)
		}
		call pargi (nw)
		call pargd (z)
		call pargr (aplow[1])
		call pargr (aphigh[1])
	    if (coeff[1] != EOS) {
		call strcat (" ", Memc[val], sz_val)
		call strcat (coeff, Memc[val], sz_val)
	    }
	    call mw_swattrs (mw, 2, Memc[str], Memc[val])

	    if (SMW_APS(smw) != NULL)
		Memi[SMW_APS(smw)+index1-1] = ap
	}

	call sfree (sp)
end
