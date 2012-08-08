include	<error.h>
include	<smw.h>


# SMW_GWATTRS -- Get spectrum attribute parameters.
# BE CAREFUL OF OUTPUT VARIABLES BEING THE SAME MEMORY ADDRESS!

procedure smw_gwattrs (smw, index1, index2, ap, beam, dtype, w1, dw, nw, z,
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
pointer	coeff				# Nonlinear coeff string (input/output)

int	i, j, n, ip, sz_coeff, strlen(), ctoi(), ctor(), ctod()
double	a, b
pointer	sp, key, mw
errchk	smw_mw, mw_gwattrs

data	sz_coeff /SZ_LINE/

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)

	if (coeff == NULL)
	    call malloc (coeff, sz_coeff, TY_CHAR)
	else
	    call realloc (coeff, sz_coeff, TY_CHAR)

	# Determine parameters based on the SMW format.
	switch (SMW_FORMAT(smw)) {
	case SMW_ND:
	    call smw_mw (smw, index1, index2, mw, i, j)

	    dtype = SMW_DTYPE(smw)
	    nw = SMW_NW(smw)
	    w1 = SMW_W1(smw)
	    dw = SMW_DW(smw)
	    z = SMW_Z(smw)

	    ap = index1
	    beam = 0
	    aplow[1] = 1
	    aphigh[1] = 1
	    aplow[2] = 1
	    aphigh[2] = 1
	    if (SMW_LDIM(smw) > 1) {
		aplow[1] = i - (SMW_NSUM(smw,1)-1) / 2
		aphigh[1] = nint (aplow[1]) + SMW_NSUM(smw,1) - 1
		aplow[1] = max (1, nint (aplow[1]))
		aphigh[1] = min (SMW_LLEN(smw,2), nint (aphigh[1]))
	    }
	    if (SMW_LDIM(smw) > 2) {
		aplow[2] = j - (SMW_NSUM(smw,2)-1) / 2
		aphigh[2] = nint (aplow[2]) + SMW_NSUM(smw,2) - 1
		aplow[2] = max (1, nint (aplow[2]))
		aphigh[2] = min (SMW_LLEN(smw,3), nint (aphigh[2]))
	    }

	    Memc[coeff] = EOS
	case SMW_ES:
	    call smw_mw (smw, index1, index2, mw, i, j)

	    dtype = SMW_DTYPE(smw)
	    nw = SMW_NW(smw)
	    w1 = SMW_W1(smw)
	    dw = SMW_DW(smw)
	    z = SMW_Z(smw)

	    ap = Memi[SMW_APS(smw)+index1-1]
	    beam = Memi[SMW_BEAMS(smw)+index1-1]
	    aplow[1] = Memr[SMW_APLOW(smw)+2*index1-2]
	    aphigh[1] = Memr[SMW_APHIGH(smw)+2*index1-2]
	    aplow[2] = Memr[SMW_APLOW(smw)+2*index1-1]
	    aphigh[2] = Memr[SMW_APHIGH(smw)+2*index1-1]

	    Memc[coeff] = EOS
	case SMW_MS:
	    call smw_mw (smw, index1, index2, mw, i, j)

	    call sprintf (Memc[key], SZ_FNAME, "spec%d")
		call pargi (i)

	    call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	    while (strlen (Memc[coeff]) == sz_coeff) {
		sz_coeff = 2 * sz_coeff
		call realloc (coeff, sz_coeff, TY_CHAR)
		call mw_gwattrs (mw, 2, Memc[key], Memc[coeff], sz_coeff)
	    }

	    ip = 1
	    i = ctoi (Memc[coeff], ip, ap)
	    i = ctoi (Memc[coeff], ip, beam)
	    i = ctoi (Memc[coeff], ip, j)
	    i = ctod (Memc[coeff], ip, a)
	    i = ctod (Memc[coeff], ip, b)
	    i = ctoi (Memc[coeff], ip, n)
	    i = ctod (Memc[coeff], ip, z)
	    i = ctor (Memc[coeff], ip, aplow[1])
	    i = ctor (Memc[coeff], ip, aphigh[1])
	    aplow[2] = INDEF
	    aphigh[2] = INDEF
	    if (Memc[coeff+ip-1] != EOS)
		call strcpy (Memc[coeff+ip], Memc[coeff], sz_coeff)
	    else
		Memc[coeff] = EOS

	    if (j==DCLOG) {
		if (abs(a)>20. || abs(a+(n-1)*b)>20.)
		    j = DCLINEAR
		else {
		    a = 10D0 ** a
		    b = a * (10D0 ** ((n-1)*b) - 1) / (n - 1)
		}
	    }

	    dtype = j
	    w1 = a
	    dw = b
	    nw = n
	}

	call sfree (sp)
end
