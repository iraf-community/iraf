include	<smw.h>


# Evaluate SMW coordinate transform.  These procedures call the
# MWCS procedures unless the WCS is a split MULTISPEC WCS.  In that
# case the appropriate piece needs to be determined and the physical
# line numbers manipulated.  For log sampled spectra conversions
# must be made for EQUISPEC/NDSPEC.  The convention is that coordinates
# are always input and output and linear.  Note that the MULTISPEC
# function driver already takes care of this.
#
# SMW_CTRANR -- N dimensional real coordinate transformation.
# SMW_CTRAND -- N dimensional double coordinate transformation.
# SMW_C1TRANR -- One dimensional real coordinate transformation.
# SMW_C1TRAND -- One dimensional double coordinate transformation.
# SMW_C2TRANR -- Two dimensional real coordinate transformation.
# SMW_C2TRAND -- Two dimensional double coordinate transformation.



# SMW_CTRAN -- N dimensional coordinate transformation.

procedure smw_ctranr (ct, p1, p2, ndim)

pointer	ct		#I SMW CT pointer
real	p1[ndim]	#I Input coordinate
real	p2[ndim]	#O Output coordinate
int	ndim		#I Dimensionality

int	i, j, format, daxis, aaxis, dtype, naps
pointer	smw, aps
errchk	mw_ctranr

begin
	if (SMW_NCT(ct) != 1)
	    call error (1, "SMW_CTRAN: Wrong WCS type")

	call amovr (p1, p2, ndim)

	smw = SMW_SMW(ct)
	format = SMW_FORMAT(smw)
	daxis = SMW_DAXIS(ct)
	aaxis = SMW_AAXIS(ct)
	dtype = SMW_DTYPE(smw)
	naps = SMW_NSPEC(smw)
	aps = SMW_APS(smw)
	switch (format) {
	case SMW_ND, SMW_ES:
	    switch (SMW_CTTYPE(ct)) {
	    case SMW_LW, SMW_PW:
		call mw_ctranr (SMW_CT(ct,0), p2, p2, ndim)
		if (daxis > 0 && dtype == DCLOG)
		    p2[daxis] = 10. ** max (-20.0, min (20.0, p2[daxis]))
		if (aaxis > 0 && format == SMW_ES) {
		    i = max (1, min (naps, nint (p2[aaxis])))
		    p2[aaxis] = Memi[aps+i-1]
		}
	    case SMW_WL, SMW_WP:
		if (daxis > 0 && dtype == DCLOG)
		    p2[daxis] = log10 (p2[daxis])
		if (aaxis > 0 && format == SMW_ES) {
		    j = nint (p2[aaxis])
		    p2[aaxis] = 1
		    do i = 1, naps {
			if (j == Memi[aps+i-1]) {
			    p2[aaxis] = i
			    break
			}
		    }
		}
		call mw_ctranr (SMW_CT(ct,0), p2, p2, ndim)
	    default:
		call mw_ctranr (SMW_CT(ct,0), p2, p2, ndim)
	    }
	case SMW_MS:
	    call mw_ctranr (SMW_CT(ct,0), p1, p2, ndim)
	}
end


# SMW_C1TRAN -- One dimensional coordinate transformation.

real procedure smw_c1tranr (ct, x1)

pointer	ct		#I SMW CT pointer
real	x1		#I Input coordinate
real	x2		#O Output coordinate

errchk	mw_ctranr

begin
	call smw_ctranr (ct, x1, x2, 1)
	return (x2)
end


# SMW_C2TRAN -- Two dimensional coordinate transformation.

procedure smw_c2tranr (ct, x1, y1, x2, y2)

pointer	ct		#I SMW CT pointer
real	x1, y1		#I Input coordinates
real	x2, y2		#O Output coordinates

real	p1[2], p2[2]
int	i, j, naps
real	xp, yp
pointer	aps, smw_ct()
errchk	smw_ct, mw_c2tranr

begin
	# Unsplit WCS.
	if (SMW_NCT(ct) == 1) {
	    p1[1] = x1
	    p1[2] = y1
	    call smw_ctranr (ct, p1, p2, 2)
	    x2 = p2[1]
	    y2 = p2[2]
	    return
	}

	# If we get here then we are dealing with a split MULTISPEC WCS.
	# Depending on the systems being transformed there may need to
	# transformation made on the physical coordinate system.

	switch (SMW_CTTYPE(ct)) {
	case SMW_LW:
	    call mw_c2tranr (SMW_CTL(ct), x1, y1, xp, yp)
	    i = nint (yp)
	    yp = mod (i-1, SMW_NSPLIT) + 1
	    call mw_c2tranr (smw_ct(ct,i), xp, yp, x2, y2)
	case SMW_PW:
	    i = nint (y1)
	    yp = mod (i-1, SMW_NSPLIT) + 1
	    call mw_c2tranr (smw_ct(ct,i), x1, yp, x2, y2)
	case SMW_WL:
	    aps = SMW_APS(SMW_SMW(ct))
	    naps = SMW_NSPEC(SMW_SMW(ct))
	    j = nint (y1)
	    do i = 1, naps {
		if (Memi[aps+i-1] == j) {
		    call mw_c2tranr (smw_ct(ct,i), x1, y1, xp, yp)
		    yp = i
		    call mw_c2tranr (SMW_CTL(ct), xp, yp, x2, y2)
		    return
		}
	    }
	    call error (1, "Aperture not found")
	case SMW_WP:
	    aps = SMW_APS(SMW_SMW(ct))
	    naps = SMW_NSPEC(SMW_SMW(ct))
	    j = nint (y1)
	    do i = 1, naps {
		if (Memi[aps+i-1] == j) {
		    call mw_c2tranr (smw_ct(ct,i), x1, y1, x2, y2)
		    y2 = i
		    return
		}
	    }
	    call error (1, "Aperture not found")
	default:
	    x2 = x1
	    y2 = y1
	}
end

# SMW_CTRAN -- N dimensional coordinate transformation.

procedure smw_ctrand (ct, p1, p2, ndim)

pointer	ct		#I SMW CT pointer
double	p1[ndim]	#I Input coordinate
double	p2[ndim]	#O Output coordinate
int	ndim		#I Dimensionality

int	i, j, format, daxis, aaxis, dtype, naps
pointer	smw, aps
errchk	mw_ctrand

begin
	if (SMW_NCT(ct) != 1)
	    call error (1, "SMW_CTRAN: Wrong WCS type")

	call amovd (p1, p2, ndim)

	smw = SMW_SMW(ct)
	format = SMW_FORMAT(smw)
	daxis = SMW_DAXIS(ct)
	aaxis = SMW_AAXIS(ct)
	dtype = SMW_DTYPE(smw)
	naps = SMW_NSPEC(smw)
	aps = SMW_APS(smw)
	switch (format) {
	case SMW_ND, SMW_ES:
	    switch (SMW_CTTYPE(ct)) {
	    case SMW_LW, SMW_PW:
		call mw_ctrand (SMW_CT(ct,0), p2, p2, ndim)
		if (daxis > 0 && dtype == DCLOG)
		    p2[daxis] = 10. ** max (-20.0D0, min (20.0D0, p2[daxis]))
		if (aaxis > 0 && format == SMW_ES) {
		    i = max (1, min (naps, nint (p2[aaxis])))
		    p2[aaxis] = Memi[aps+i-1]
		}
	    case SMW_WL, SMW_WP:
		if (daxis > 0 && dtype == DCLOG)
		    p2[daxis] = log10 (p2[daxis])
		if (aaxis > 0 && format == SMW_ES) {
		    j = nint (p2[aaxis])
		    p2[aaxis] = 1
		    do i = 1, naps {
			if (j == Memi[aps+i-1]) {
			    p2[aaxis] = i
			    break
			}
		    }
		}
		call mw_ctrand (SMW_CT(ct,0), p2, p2, ndim)
	    default:
		call mw_ctrand (SMW_CT(ct,0), p2, p2, ndim)
	    }
	case SMW_MS:
	    call mw_ctrand (SMW_CT(ct,0), p1, p2, ndim)
	}
end


# SMW_C1TRAN -- One dimensional coordinate transformation.

double procedure smw_c1trand (ct, x1)

pointer	ct		#I SMW CT pointer
double	x1		#I Input coordinate
double	x2		#O Output coordinate

errchk	mw_ctrand

begin
	call smw_ctrand (ct, x1, x2, 1)
	return (x2)
end


# SMW_C2TRAN -- Two dimensional coordinate transformation.

procedure smw_c2trand (ct, x1, y1, x2, y2)

pointer	ct		#I SMW CT pointer
double	x1, y1		#I Input coordinates
double	x2, y2		#O Output coordinates

double	p1[2], p2[2]
int	i, j, naps
double	xp, yp
pointer	aps, smw_ct()
errchk	smw_ct, mw_c2trand

begin
	# Unsplit WCS.
	if (SMW_NCT(ct) == 1) {
	    p1[1] = x1
	    p1[2] = y1
	    call smw_ctrand (ct, p1, p2, 2)
	    x2 = p2[1]
	    y2 = p2[2]
	    return
	}

	# If we get here then we are dealing with a split MULTISPEC WCS.
	# Depending on the systems being transformed there may need to
	# transformation made on the physical coordinate system.

	switch (SMW_CTTYPE(ct)) {
	case SMW_LW:
	    call mw_c2trand (SMW_CTL(ct), x1, y1, xp, yp)
	    i = nint (yp)
	    yp = mod (i-1, SMW_NSPLIT) + 1
	    call mw_c2trand (smw_ct(ct,i), xp, yp, x2, y2)
	case SMW_PW:
	    i = nint (y1)
	    yp = mod (i-1, SMW_NSPLIT) + 1
	    call mw_c2trand (smw_ct(ct,i), x1, yp, x2, y2)
	case SMW_WL:
	    aps = SMW_APS(SMW_SMW(ct))
	    naps = SMW_NSPEC(SMW_SMW(ct))
	    j = nint (y1)
	    do i = 1, naps {
		if (Memi[aps+i-1] == j) {
		    call mw_c2trand (smw_ct(ct,i), x1, y1, xp, yp)
		    yp = i
		    call mw_c2trand (SMW_CTL(ct), xp, yp, x2, y2)
		    return
		}
	    }
	    call error (1, "Aperture not found")
	case SMW_WP:
	    aps = SMW_APS(SMW_SMW(ct))
	    naps = SMW_NSPEC(SMW_SMW(ct))
	    j = nint (y1)
	    do i = 1, naps {
		if (Memi[aps+i-1] == j) {
		    call mw_c2trand (smw_ct(ct,i), x1, y1, x2, y2)
		    y2 = i
		    return
		}
	    }
	    call error (1, "Aperture not found")
	default:
	    x2 = x1
	    y2 = y1
	}
end

