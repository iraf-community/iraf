include	<smw.h>


# Evaluate SMW coordinate transform.  These procedures simple call the
# MWCS procedures unless the WCS is a split MULTISPEC WCS.  In that
# case the appropriate piece needs to be determined and the physical
# line numbers manipulated.
#
# SMW_CTRANR -- N dimensional real coordinate transformation.
# SMW_CTRAND -- N dimensional double coordinate transformation.
# SMW_C1TRANR -- One dimensional real coordinate transformation.
# SMW_C1TRAND -- One dimensional double coordinate transformation.
# SMW_C2TRANR -- Two dimensional real coordinate transformation.
# SMW_C2TRAND -- Two dimensional double coordinate transformation.


# SMW_CTRANR -- N dimensional real coordinate transformation.

procedure smw_ctranr (ct, p1, p2, ndim)

pointer	ct		#I SMW CT pointer
real	p1[ndim]	#I Input coordinate
real	p2[ndim]	#O Output coordinate
int	ndim		#I Dimensionality

errchk	mw_ctranr

begin
	if (SMW_NCT(ct) == 1) {
	    call mw_ctranr (SMW_CT(ct,0), p1, p2, ndim)
	    return
	}
	
	call error (1, "SMW_CTRANR: Wrong WCS type")
end


# SMW_CTRAND -- N dimensional double coordinate transformation.

procedure smw_ctrand (ct, p1, p2, ndim)

pointer	ct		#I SMW CT pointer
real	p1[ndim]	#I Input coordinate
real	p2[ndim]	#O Output coordinate
int	ndim		#I Dimensionality

errchk	mw_ctrand

begin
	if (SMW_NCT(ct) == 1) {
	    call mw_ctrand (SMW_CT(ct,0), p1, p2, ndim)
	    return
	}
	
	call error (1, "SMW_CTRAND: Wrong WCS type")
end


# SMW_C1TRANR -- One dimensional real coordinate transformation.

real procedure smw_c1tranr (ct, x)

pointer	ct		#I SMW CT pointer
real	x		#I Input coordinate

real	mw_c1tranr()
errchk	mw_c1tranr

begin
	if (SMW_NCT(ct) == 1)
	    return (mw_c1tranr (SMW_CT(ct,0), x))
	
	call error (1, "SMW_C1TRANR: Wrong WCS type")
end


# SMW_C1TRAND -- One dimensional double coordinate transformation.

double procedure smw_c1trand (ct, x)

pointer	ct		#I SMW CT pointer
double	x		#I Input coordinate

double	mw_c1trand()
errchk	mw_c1trand

begin
	if (SMW_NCT(ct) == 1)
	    return (mw_c1trand (SMW_CT(ct,0), x))
	
	call error (1, "SMW_C1TRAND: Wrong WCS type")
end


# SMW_C2TRANR -- Two dimensional real coordinate transformation.

procedure smw_c2tranr (ct, x1, y1, x2, y2)

pointer	ct		#I SMW CT pointer
real	x1, y1		#I Input coordinates
real	x2, y2		#O Output coordinates

int	i, j, naps
real	xp, yp
pointer	aps, smw_ct()
errchk	smw_ct, mw_c2tranr

begin
	if (SMW_NCT(ct) == 1) {
	    # EQUISPEC requires mapping physical lines to apertures.
	    switch (SMW_FORMAT(SMW_SMW(ct))) {
	    case SMW_ES:
		aps = SMW_APS(SMW_SMW(ct))
		naps = SMW_NSPEC(SMW_SMW(ct))
		switch (SMW_CTTYPE(ct)) {
		case SMW_LW, SMW_PW:
		    call mw_c2tranr (SMW_CT(ct,0), x1, y1, x2, y2)
		    i = max (1, min (naps, nint (y2)))
		    y2 = Memi[aps+i-1]
		case SMW_WL, SMW_WP:
		    j = nint (y1)
		    yp = 1
		    do i = 1, naps
			if (j == Memi[aps+i-1]) {
			    yp = i
			    break
			}
		    call mw_c2tranr (SMW_CT(ct,0), x1, yp, x2, y2)
		default:
		    call mw_c2tranr (SMW_CT(ct,0), x1, y1, x2, y2)
		}
	    default:
		call mw_c2tranr (SMW_CT(ct,0), x1, y1, x2, y2)
	    }
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


# SMW_C2TRAND -- Two dimensional double coordinate transformation

procedure smw_c2trand (ct, x1, y1, x2, y2)

pointer	ct		#I SMW CT pointer
double	x1, y1		#I Input coordinates
double	x2, y2		#O Output coordinates

int	i, j, naps
double	xp, yp
pointer	aps, smw_ct()
errchk	smw_ct, mw_c2trand

begin
	if (SMW_NCT(ct) == 1) {
	    # EQUISPEC requires mapping physical lines to apertures.
	    switch (SMW_FORMAT(SMW_SMW(ct))) {
	    case SMW_ES:
		aps = SMW_APS(SMW_SMW(ct))
		naps = SMW_NSPEC(SMW_SMW(ct))
		switch (SMW_CTTYPE(ct)) {
		case SMW_LW, SMW_PW:
		    call mw_c2trand (SMW_CT(ct,0), x1, y1, x2, y2)
		    i = max (1, min (naps, nint (y2)))
		    y2 = Memi[aps+i-1]
		case SMW_WL, SMW_WP:
		    j = nint (y1)
		    yp = 1
		    do i = 1, naps
			if (j == Memi[aps+i-1]) {
			    yp = i
			    break
			}
		    call mw_c2trand (SMW_CT(ct,0), x1, yp, x2, y2)
		default:
		    call mw_c2trand (SMW_CT(ct,0), x1, y1, x2, y2)
		}
	    default:
		call mw_c2trand (SMW_CT(ct,0), x1, y1, x2, y2)
	    }
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
