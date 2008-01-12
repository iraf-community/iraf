include	<mach.h>
include	<error.h>
include	<smw.h>
include	"../identify.h"
include	"autoid.h"


# AID_TARGET -- Select target lines and the dispersion limits to be searched.
# The dispersion limits may be based on header parameters.

procedure aid_target (aid)

pointer	aid		#I AID pointer

int	i, j, l, nt, n
double	dw, dwmin, dwmax, pix, aid_imgd(), id_center()
pointer	sp, x, y, idt, idr, im, xt, xtl
int	id_upeaks(), stridxs()
errchk	id_upeaks, id_center

begin
        call smark (sp)
        call salloc (x, ID_NPTS(AID_IDT(aid)), TY_REAL)

	idt = AID_IDT(aid)
	idr = AID_IDR(aid)
	im = IM(ID_SH(idt))
	nt = ID_NPTS(idt)

	# Set the approximate coordinate information.
	AID_CRVAL(aid) = aid_imgd (im, AID_CR(aid))
	AID_CDELT(aid) = aid_imgd (im, AID_CD(aid))
	AID_CRPIX(aid) = aid_imgd (im, AID_CP(aid))
	AID_CRQUAD(aid) = aid_imgd (im, AID_CQ(aid))
	AID_CRSEARCH(aid) = aid_imgd (im, AID_CRS(aid))
	AID_CDSEARCH(aid) = aid_imgd (im, AID_CDS(aid))

	if (IS_INDEFD(AID_CRPIX(aid)))
	    AID_CRPIX(aid) = (nt+1) / 2.

	if (IS_INDEFD(AID_CRQUAD(aid)))
	    AID_CRQUAD(aid) = 0D0

	if (!IS_INDEFD(AID_CRVAL(aid)) && !IS_INDEFD(AID_CDELT(aid))) {
	    dw = nt * AID_CDELT(aid)
	    if (IS_INDEFD(AID_CRSEARCH(aid)))
		AID_CRSEARCH(aid) = abs (0.1 * dw)
	    else if (AID_CRSEARCH(aid) < 0.)
		AID_CRSEARCH(aid) = abs (AID_CRSEARCH(aid) * dw)
	    if (IS_INDEFD(AID_CDSEARCH(aid)))
		AID_CDSEARCH(aid) = abs (0.1 * AID_CDELT(aid))
	    else if (AID_CDSEARCH(aid) < 0.)
		AID_CDSEARCH(aid) = abs (AID_CDSEARCH(aid) * AID_CDELT(aid))
	    AID_CRSEARCH(aid) = max (abs (0.0001 * dw),
		AID_CRSEARCH(aid))
	    AID_CDSEARCH(aid) = max (abs (0.0001 * AID_CDELT(aid)),
		AID_CDSEARCH(aid))
	    dwmax = 2 * AID_CRSEARCH(aid) + (nt - 1) *
		(abs (AID_CDELT(aid)) + AID_CDSEARCH(aid))
	    dwmin = (abs (AID_CDELT(aid)) - AID_CDSEARCH(aid)) * (nt - 1)
	    dwmin = max (1.0D-1, dwmin / dwmax)
	    AID_NB(aid) = nint (1. / dwmin)
	}

        # Find the peaks in the target spectrum.
	if (ID_FTYPE(idt) == ABSORPTION) {
	    call anegr (IMDATA(idt,1), IMDATA(idt,1), nt)
	    n = id_upeaks (idt, IMDATA(idt,1), Memr[x], nt, INDEF,
		int (ID_MINSEP(idt)), 0, AID_NTMAX(aid), 5, INDEF, false)
	    call anegr (IMDATA(idt,1), IMDATA(idt,1), nt)
	} else {
	    n = id_upeaks (idt, IMDATA(idt,1), Memr[x], nt, INDEF,
		int (ID_MINSEP(idt)), 0, AID_NTMAX(aid), 5, INDEF, false)
	}
	call salloc (y, n, TY_REAL)
	do i = 1, n
	    Memr[y+i-1] = -IMDATA(idt,nint(Memr[x+i-1]))
	call xt_sort2 (Memr[y], Memr[x], n)

        # Center and sort the lines.
        if (AID_XTF(aid) == NULL)
            call malloc (AID_XTF(aid), n, TY_DOUBLE)
        else
            call realloc (AID_XTF(aid), n, TY_DOUBLE)
        xt = AID_XTF(aid)

        j = 0
        do i = 1, n {
	    pix = Memr[x+i-1]
            pix = id_center (idt, pix, ID_FWIDTH(idt), ID_FTYPE(idt))
            if (IS_INDEFD(pix))
                next
	    if (IS_INDEFD(pix))
		next
	    do l = 1, j {
	        if (abs (pix-Memd[xt+l-1]) < 1.)
		    break
	    }
	    if (l <= j)
		next
	    Memd[xt+j] = pix
            j = j + 1
        }
        AID_NTF(aid) = j

        # Sort the lines.
        if (AID_XT(aid) == NULL)
            call malloc (AID_XT(aid), j, TY_DOUBLE)
        else
            call realloc (AID_XT(aid), j, TY_DOUBLE)
        xt = AID_XT(aid)
        if (j > 0)
            call asrtd (Memd[AID_XTF(aid)], Memd[xt], j)
        else {
            call salloc (x, SZ_LINE, TY_CHAR)
            call sprintf (Memc[x], SZ_LINE, "No target lines found in `%s'")
                call pargstr (ID_IMAGE(idt))
            call error (1, Memc[x])
        }

	# Linearize the lines.
        if (AID_XTL(aid) == NULL)
            call malloc (AID_XTL(aid), j, TY_DOUBLE)
        else
            call realloc (AID_XTL(aid), j, TY_DOUBLE)
	xt = AID_XT(aid)
	xtl = AID_XTL(aid)
	do i = 1, j
	    Memd[xtl+i-1] = Memd[xt+i-1] +
	         AID_CRQUAD(aid) * (Memd[xt+i-1]-AID_CRPIX(aid))**2

        # Debug t: Print list of target lines.
	if (stridxs ("t", AID_DEBUG(aid,1)) != 0) {
	    call eprintf ("# Selected target lines:\n")
	    call eprintf ("#%10s %11s\n")
	        call pargstr ("Measured")
		call pargstr ("Undistorted")
            do i = 1, j {
                call eprintf ("%11.6g %11.6g\n")
                    call pargd (Memd[xt+i-1])
                    call pargd (Memd[xtl+i-1])
            }
	    call eprintf ("\n")
        }

	call sfree (sp)
end


# AID_REFERENCE -- Set reference lines from spectrum or line list.  

procedure aid_reference (aid, ev, flip)

pointer	aid		#I AID pointer
pointer	ev		#I EV pointer
int	flip		#I Flip dispersion?

int	i, j, i1, i2, npts, nr, nt, nll, id_peaks(), stridxs()
double	w, w0, w1, wp, cdelt, wa, wb
real	sig, wt, center1d()
pointer	sp, x, idt, idr, specr, xr, sh, label, ll
double	shdr_wl(), shdr_lw()
errchk	id_peaks, center1d

begin
	call smark (sp)

	idr = AID_IDR(aid)
	npts = ID_NPTS(idr)
	sh = ID_SH(idr)
	specr = AID_SPECR(aid)
	idt = AID_IDT(aid)
	nt = ID_NPTS(idt)

	# Set reference parameters.
	if (sh != NULL) {
	    w0 = min (W0(sh), W1(sh))
	    w1 = max (W0(sh), W1(sh))
	    wp = abs (WP(sh))
	} else {
	    ll = ID_LL(idr)
	    nll = ID_NLL(idr)
	    if (ll == NULL) {
		ll = ID_LL(idt)
		nll = ID_NLL(idt)
	    }
	    x = ll
	    w0 = Memd[x]
	    w1 = Memd[x+nll-1]
	    wp = INDEFD
	}

	# Set limits for reference coordinate and dispersion values.
	AID_CRMIN(aid) = -MAX_DOUBLE
	AID_CRMAX(aid) = MAX_DOUBLE
	AID_CDMIN(aid) = 0D0
	AID_CDMAX(aid) = MAX_DOUBLE
	if (IS_INDEFD(AID_CDELT(aid))) {
	    switch (AID_CDDIR(aid)) {
	    case CDINC:
		AID_CDSIGN(aid) = 1
	    case CDDEC:
		AID_CDSIGN(aid) = -1
	    default:
		if (flip == YES)
		    AID_CDSIGN(aid) = -1
		else
		    AID_CDSIGN(aid) = 1
	    }

	    if (!IS_INDEFD(AID_CRVAL(aid))) {
		AID_CRMIN(aid) = AID_CRVAL(aid) - AID_CRSEARCH(aid)
		AID_CRMAX(aid) = AID_CRVAL(aid) + AID_CRSEARCH(aid)
	    }

	    if (sh != NULL) {
		i1 = 1
		i2 = npts
		sig = 0.
	    } else {
		wa = -MAX_DOUBLE
		wb = MAX_DOUBLE
	    }

	    AID_W1(aid) = INDEF
	    AID_W2(aid) = INDEF
	} else if (IS_INDEFD(AID_CRVAL(aid))) {
	    switch (AID_CDDIR(aid)) {
	    case CDINC:
		cdelt = abs (AID_CDELT(aid))
		AID_CDSIGN(aid) = 1
	    case CDDEC:
		cdelt = -abs (AID_CDELT(aid))
		AID_CDSIGN(aid) = -1
	    default:
		if (flip == YES)
		    cdelt = -AID_CDELT(aid)
		else
		    cdelt = AID_CDELT(aid)
		if (cdelt < 0.)
		    AID_CDSIGN(aid) = -1
		else
		    AID_CDSIGN(aid) = 1
	    }

	    AID_CDMIN(aid) = abs (cdelt) - AID_CDSEARCH(aid)
	    AID_CDMAX(aid) = abs (cdelt) + AID_CDSEARCH(aid)

	    if (sh != NULL) {
		i1 = 1
		i2 = npts
		sig = abs (AID_CDELT(aid)) / wp
	    } else {
		wa = -MAX_DOUBLE
		wb = MAX_DOUBLE
	    }

	    AID_W1(aid) = INDEF
	    AID_W2(aid) = INDEF
	} else {
	    switch (AID_CDDIR(aid)) {
	    case CDINC:
		cdelt = abs (AID_CDELT(aid))
		AID_CDSIGN(aid) = 1
	    case CDDEC:
		cdelt = -abs (AID_CDELT(aid))
		AID_CDSIGN(aid) = -1
	    default:
		if (flip == YES)
		    cdelt = -AID_CDELT(aid)
		else
		    cdelt = AID_CDELT(aid)
		if (cdelt < 0.)
		    AID_CDSIGN(aid) = -1
		else
		    AID_CDSIGN(aid) = 1
	    }

	    AID_CRMIN(aid) = AID_CRVAL(aid) - AID_CRSEARCH(aid)
	    AID_CRMAX(aid) = AID_CRVAL(aid) + AID_CRSEARCH(aid)
	    AID_CDMIN(aid) = abs (cdelt) - AID_CDSEARCH(aid)
	    AID_CDMAX(aid) = abs (cdelt) + AID_CDSEARCH(aid)

	    if (cdelt > 0.) {
		wa = AID_CRMIN(aid) + (cdelt + AID_CDSEARCH(aid)) *
		    (1 - AID_CRPIX(aid))
		wb = AID_CRMAX(aid) + (cdelt + AID_CDSEARCH(aid)) *
		    (nt - AID_CRPIX(aid))
	    } else {
		wa = AID_CRMIN(aid) + (cdelt - AID_CDSEARCH(aid)) *
		    (nt - AID_CRPIX(aid))
		wb = AID_CRMAX(aid) + (cdelt - AID_CDSEARCH(aid)) *
		    (1 - AID_CRPIX(aid))
	    }

	    if (stridxs ("m", AID_DEBUG(aid,1)) != 0) {
		call eprintf ("wa=%g wb=%g\n")
		    call pargd (wa)
		    call pargd (wb)
	    }

	    if (sh != NULL) {
		i1 = max (1, min (npts, nint (shdr_wl (sh, wa))))
		i2 = max (1, min (npts, nint (shdr_wl (sh, wb))))
		sig = abs (AID_CDELT(aid)) / wp
	    }

	    AID_W1(aid) = AID_CRVAL(aid) + (1-AID_CRPIX(aid)) * cdelt
	    AID_W2(aid) = AID_CRVAL(aid) + (nt-AID_CRPIX(aid)) * cdelt
	}

	# Select lines from line list.
	if (ID_IMAGE(idr) == EOS) {
	    ll = ID_LL(idr)
	    if (ll == NULL)
		ll = ID_LL(idt)
	    x = ll
	    npts = 0
	    while (!IS_INDEFD(Memd[x])) {
		if (Memd[x] > wb)
		    break
		if (Memd[x] >= wa)
		    npts = npts + 1
		x = x + 1
	    }
	    x = x - npts
	    if (npts == 0) {
		call salloc (x, SZ_LINE, TY_CHAR)
		call sprintf (Memc[x], SZ_LINE, "No reference lines found")
		call error (1, Memc[x])
	    }

	    wa = Memd[x]
	    wb = Memd[x+npts-1] - Memd[x]
	    wb = wb / ((AID_BIN1(ev) + 1) / 2)
	    wa = wa + wb / 2 * (AID_BIN2(ev) - 1)
	    wb = wa + wb

	    x = ll
	    npts = 0
	    while (!IS_INDEFD(Memd[x])) {
		if (Memd[x] > wb)
		    break
		if (Memd[x] >= wa)
		    npts = npts + 1
		x = x + 1
	    }
	    x = x - npts
	    if (npts == 0) {
		call salloc (x, SZ_LINE, TY_CHAR)
		call sprintf (Memc[x], SZ_LINE, "No reference lines found")
		call error (1, Memc[x])
	    }

	    AID_NRMAX(aid) = npts
	    nr = AID_NRMAX(aid)
	    AID_NR(aid) = nr
	    if (AID_XR(aid) == NULL)
		call malloc (AID_XR(aid), nr, TY_DOUBLE)
	    else
		call realloc (AID_XR(aid), nr, TY_DOUBLE)
	    xr = AID_XR(aid)

	    if (nr < npts) {
		w = real (npts) / nr
		do i = 0, nr {
		    j = i * w
		    Memd[xr+i] = Memd[x+j]
		}
	    } else {
		do i = 0, nr-1
		    Memd[xr+i] = Memd[x+i]
	    }

	# Select lines using reference spectrum.
	} else {
	    wb = (i2 - i1) / ((AID_BIN1(ev) + 1) / 2)
	    i1 = max (i1, nint (i1 + wb / 2 * (AID_BIN2(ev) - 1)))
	    i2 = min (i2, nint (i1 + wb))

	    if (i2 - i1 + 1 < 100) {
		i1 = 1
		i2 = npts
	    }
	    npts = i2 - i1 + 1

	    if (specr == NULL)
		call malloc (specr, npts, TY_REAL)
	    else
		call realloc (specr, npts, TY_REAL)
	    AID_SPECR(aid) = specr
	    AID_X1R(aid) = i1
	    AID_X2R(aid) = i2
	    wa = Memr[SX(sh)+i1-1]
	    wb = Memr[SX(sh)+i2-1]
	    call amovr (IMDATA(idr,i1), Memr[specr], npts)

	    if (sig > 1.) {
		ID_MINSEP(idr) = sig * ID_MINSEP(idt)
		ID_FWIDTH(idr) = sig * ID_FWIDTH(idt)
		sig = sig / 1.1774
		j = nint (3 * sig)
		call malloc (x, npts, TY_REAL)
		call malloc (xr, npts+2*j+1, TY_REAL)
		xr = xr + j
		call amovr (Memr[specr], Memr[xr], npts)
		do i = 1, j {
		    wt = exp (-0.5 * (i / sig) ** 2)
		    call amulkr (Memr[specr], wt, Memr[x], npts)
		    call aaddr (Memr[x], Memr[xr+i], Memr[xr+i], npts)
		    call aaddr (Memr[x], Memr[xr-i], Memr[xr-i], npts)
		}
		call amovr (Memr[xr], Memr[specr], npts)
		call mfree (x, TY_REAL)
		call mfree (xr-j, TY_REAL)
	    }

	    call salloc (x, npts, TY_REAL)

	    # Find the peaks in the reference spectrum.
	    AID_NRMAX(aid) = 2 * AID_NTF(aid)
	    if (ID_FTYPE(idr) == ABSORPTION) {
		call anegr (Memr[specr], Memr[specr], nt)
		nr = id_peaks (idr, Memr[specr], Memr[x], npts, INDEF,
		    int (ID_MINSEP(idr)), 0, AID_NRMAX(aid), INDEF, false)
		call anegr (Memr[specr], Memr[specr], nt)
	    } else {
		nr = id_peaks (idr, Memr[specr], Memr[x], npts, INDEF,
		    int (ID_MINSEP(idr)), 0, AID_NRMAX(aid), INDEF, false)
	    }

	    # Center and sort the lines.
	    if (AID_XR(aid) == NULL)
		call malloc (AID_XR(aid), nr, TY_DOUBLE)
	    else
		call realloc (AID_XR(aid), nr, TY_DOUBLE)
	    xr = AID_XR(aid)

	    j = 0
	    label = NULL
	    do i = 1, nr {
		wt = center1d (Memr[x+i-1], Memr[specr], npts, ID_FWIDTH(idr),
		    ID_FTYPE(idr), ID_CRADIUS(idt), 0.)
		if (IS_INDEF(wt))
		    next
		w = shdr_lw (sh, double(wt+i1-1))
		Memd[xr+j] = w
		call id_match (idt, w, Memd[xr+j], label, -2.)
		if (IS_INDEFD(Memd[xr+j]) || (j>0 && Memd[xr+j]==Memd[xr+j-1]))
		    next
		j = j + 1
	    }
	    call mfree (label, TY_CHAR)
	    nr = j
	    AID_NR(aid) = nr

	    # Sort the lines.
	    if (j > 0)
		call asrtd (Memd[xr], Memd[xr], nr)
	    else {
		call salloc (x, SZ_LINE, TY_CHAR)
		call sprintf (Memc[x], SZ_LINE,
		    "No reference lines found in `%s'")
		    call pargstr (ID_IMAGE(idr))
		call error (1, Memc[x])
	    }
	}

	#AID_NT(aid) = min (2 * AID_NR(aid), AID_NTF(aid))
	AID_NT(aid) = AID_NTF(aid)
	call asrtd (Memd[AID_XTF(aid)], Memd[AID_XT(aid)], AID_NT(aid))

	# Debug w: Print wavelength bin limits.
	if (stridxs ("w", AID_DEBUG(aid,1)) != 0) {
	    call eprintf ("%2d/%-2d %g %g\n")
		call pargi (AID_BIN1(ev))
		call pargi (AID_BIN2(ev))
		call pargd (wa)
		call pargd (wb)
	}

	# Debug b: Print search limits.
	if (stridxs ("b", AID_DEBUG(aid,1)) != 0) {
	    if (ev == AID_EVS(aid)) {
		call eprintf ("Search: CRVAL = %.8g - %.8g,  CDELT = %.5g - %.5g\n\n")
		    call pargd (AID_CRMIN(aid))
		    call pargd (AID_CRMAX(aid))
		    call pargd (AID_CDMIN(aid))
		    call pargd (AID_CDMAX(aid))
	    }
	}

	# Debug r: Print list of reference lines.
	if (stridxs ("r", AID_DEBUG(aid,1)) != 0) {
	    call eprintf ("# Selected reference lines:\n")
	    do i = 1, nr {
		call eprintf ("%10.6g\n")
		    call pargd (Memd[xr+i-1])
	    }
	    call eprintf ("\n")
	}

	call sfree (sp)
end


# AID_AUTOID1 -- Automatically identify lines.
# This routine takes preset target and reference line lists and tries to
# find correspondences.  It returns lists of possible correspondences
# and dispersions.

procedure aid_autoid1 (aid, ev)

pointer	aid		#I AID pointer
pointer	ev		#I EV pointer

int	i, nn, n1, n2, nr1, nr2, n, nd
pointer	sp, idt, x1, x2, x3, r1, s1, r2, s2, votes, svotes
pointer	x, y, w, w1, dw, nw, nv

int	aid_rsort(), aid_vsort(), stridxs()
extern	aid_rsort, aid_vsort
errchk	aid_select, aid_disp

begin
	call smark (sp)

	idt = AID_IDT(aid)
	nn = AID_NN(aid)
	x1 = AID_XR(aid)
	n1 = AID_NR(aid)
	x2 = AID_XTL(aid)
	x3 = AID_XT(aid)
	n2 = AID_NT(aid)

        # Debug l: Graph lines and spectra.
	if (stridxs ("l", AID_DEBUG(aid,1)) != 0)
	    call aid_lgraph (aid, Memd[x1], n1, Memd[x2], n2)

	# Make ratio lists.
	i = min (nn, n1-1)
	nr1 = (n1-i) * i * (i - 1) / 2 + i * (i - 1) * (i - 2) / 6
	call salloc (r1, nr1, TY_REAL)
	call aid_ratios (aid, Memd[x1], n1, 1, Memr[r1], nr1, 1)
	call salloc (s1, nr1, TY_INT)
	do i = 1, nr1
	    Memi[s1+i-1] = i
	call gqsort (Memi[s1], nr1, aid_rsort, r1)

	i = min (nn, n2-1)
	nr2 = (n2-i) * i * (i - 1) / 2 + i * (i - 1) * (i - 2) / 6
	call salloc (r2, 2*nr2, TY_REAL)
	call aid_ratios (aid, Memd[x2], n2, AID_CDSIGN(aid), Memr[r2], nr2, 2)
	call salloc (s2, nr2, TY_INT)
	do i = 1, nr2
	    Memi[s2+i-1] = i
	call gqsort (Memi[s2], nr2, aid_rsort, r2)

	call salloc (votes, n1 * n2, TY_INT)
	call aid_votes (aid, Memr[r1], Memi[s1], nr1, Memr[r2], Memi[s2],
	    nr2, Memd[x1], Memd[x2], Memi[votes], n1, n2)

	call salloc (svotes, n1 * n2, TY_INT)
	do i = 1, n1 * n2
	    Memi[svotes+i-1] = i
	call gqsort (Memi[svotes], n1*n2, aid_vsort, votes)

	do n = 1, n1 * n2
	    if (Memi[votes+Memi[svotes+n-1]-1] < 1)
	        break
	n = max (3 * n2, n-1)

	call malloc (x, n, TY_REAL)
	call malloc (y, n, TY_REAL)
	call salloc (w, n, TY_REAL)
	iferr (call aid_select (aid, Memd[x1], Memd[x2], Memd[x3], Memi[votes],
	    Memi[svotes], n1, n2, Memr[x], Memr[y], Memr[w], n)) {
	    call sfree (sp)
	    call erract (EA_ERROR)
	}

	nd = AID_NDMAX(aid)
	call malloc (w1, nd, TY_REAL)
	call malloc (dw, nd, TY_REAL)
	call salloc (nw, nd, TY_INT)
	call salloc (nv, nd, TY_INT)
	call aid_disp (aid, Memr[y], Memr[x], Memr[w], n, Memr[w1], Memr[dw],
	    Memi[nw], Memi[nv], nd)

	AID_X(ev) = x
	AID_Y(ev) = y
	AID_N(ev) = n
	AID_A(ev) = w1
	AID_B(ev) = dw
	AID_ND(ev) = nd

	call sfree (sp)
end


# AID_RATIOS -- Generate list of spacing ratios from list of lines.

procedure aid_ratios (aid, x, n, cdsign, r, nr, nv)

pointer	aid		#I AID pointer
double	x[n]		#I Line positions (sorted)
int	n		#I Number of lines
int	cdsign		#I Sign of dispersion
real	r[nr,nv]	#O Ratios
int	nr		#O Number of ratios
int	nv		#I Number of values

int	i, j, k, l, nn, stridxs()
real	minr, maxr, xi, xj, xk, xij, xjk, sig, ratio, err

begin
	nn = AID_NN(aid)
	sig = AID_SIG(aid)
	minr = AID_MINRATIO(aid)
	maxr = 1 / AID_MINRATIO(aid)

	# Compute ratios.
	l = 0
	if (cdsign == 1) {
	    do i = 1, n-2 {
		xi = x[i]
		do j = i+1, min (i+nn-1, n-1) {
		    xj = x[j]
		    xij = xj - xi
		    do k = j+1, min (i+nn, n) {
			xk = x[k]
			xjk = xk - xj
			ratio = xij / xjk

			l = l + 1
			if (nv == 1) {
			    if (ratio < minr || ratio > maxr)
				r[l,1] = 1000.
			    else
				r[l,1] = ratio
			} else {
			    if (ratio < minr || ratio > maxr) {
				r[l,1] = 1000.
				r[l,2] = 1000.
			    } else {
				err = sig * sqrt (2*(1+ratio+ratio**2)) / xjk
				r[l,1] = ratio - err
				r[l,2] = ratio + err
			    }
			}
		    }
		}
	    }
	} else {
	    do i = n, 3, -1 {
		xi = x[i]
		do j = i-1, max (i-nn+1, 2), -1 {
		    xj = x[j]
		    xij = xi - xj
		    do k = j-1, max (i-nn, 1), -1 {
			xk = x[k]
			xjk = xj - xk
			ratio = xij / xjk

			l = l + 1
			if (nv == 1) {
			    if (ratio < minr || ratio > maxr)
				r[l,1] = 1000.
			    else
				r[l,1] = ratio
			} else {
			    if (ratio < minr || ratio > maxr) {
				r[l,1] = 1000.
				r[l,2] = 1000.
			    } else {
				err = sig * sqrt (2*(1+ratio+ratio**2)) / xjk
				r[l,1] = ratio - err
				r[l,2] = ratio + err
			    }
			}
		    }
		}
	    }
	}
	nr = l

	# Debug c: Print list of line ratios.
	if (stridxs ("c", AID_DEBUG(aid,1)) != 0) {
	    do l = 1, nr {
		call aid_lines (l, n, nn, i, j, k)
		if (nv == 1)
		    call printf ("%2d %2d %2d %8.2f %8.2f %8.2f %6.4f\n")
		else
		    call printf ("%2d %2d %2d %8.2f %8.2f %8.2f %6.4f %6.4f\n")
		    call pargi (i)
		    call pargi (j)
		    call pargi (k)
		    if (cdsign == 1) {
			call pargd (x[i])
			call pargd (x[j])
			call pargd (x[k])
		    } else {
			call pargd (x[n-i+1])
			call pargd (x[n-j+1])
			call pargd (x[n-k+1])
		    }
		    call pargr (r[l,1])
		    if (nv == 2)
			call pargr (r[l,2])
	    }
	}
end


# AID_LINES --  Convert ratio index to line indices.

procedure aid_lines (s, n, nn, i, j, k)

int	s		# Index into ratio array
int	n		# Number of lines
int	nn		# Number of neigbhors
int	i		#O Index of first line
int	j		#O Index of second line
int	k		#O Index of third line

int	l

begin
	k = s
	for (i=1;;i=i+1) {
	    l = min (nn, n-i)
	    l = l * (l-1) / 2
	    if (k <= l)
		break
	    k = k - l
	}
	for (j=i+1;;j=j+1) {
	    l = min (nn-1, n-j)
	    if (k <= l)
		break
	    k = k - l
	}
	k = k + j
end


# AID_RSORT -- Compare ratio array with smallest first.

int procedure aid_rsort (ptr, i, j)

pointer	ptr		#I Pointer to array to be sorted.
int	i		#I Index 1
int	j		#I Index 2

real	a, b

begin
	a = Memr[ptr+i-1]
	b = Memr[ptr+j-1]

	if (a < b)
	    return (-1)
	else if (b < a)
	    return (1)
	else
	    return (0)
end


# AID_VSORT -- Compare vote array with biggest first.

int procedure aid_vsort (ptr, i, j)

pointer	ptr		#I Pointer to array to be sorted.
int	i		#I Index 1
int	j		#I Index 2

int	a, b

begin
	a = Memi[ptr+i-1]
	b = Memi[ptr+j-1]

	if (a < b)
	    return (1)
	else if (b < a)
	    return (-1)
	else
	    return (0)
end


# AID_VOTES -- Find ratio matches and increment the vote array.

procedure aid_votes (aid, r1, s1, nr1, r2, s2, nr2, x1, x2, votes, n1, n2)

pointer	aid		#I AID pointer
real	r1[nr1]		#I Ratio array (reference)
int	s1[nr1]		#I Sort array
int	nr1		#I Number of ratios
real	r2[nr2,2]	#I Ratio array (target)
int	s2[nr2]		#I Sort array
int	nr2		#I Number of ratios
double	x1[n1]		#I Reference lines
double	x2[n2]		#I Target lines
int	votes[n1,n2]	#O Votes
int	n1, n2		#I Size of votes array

int	i, j, nn, np, start, stridxs()
real	maxr, ra, rb1, rb2
pointer	sp, a, b

begin
	nn = AID_NN(aid)
	np = max (3, min (AID_NP(aid), n1 - 5))
	maxr = 1. / AID_MINRATIO(aid)

	call smark (sp)
	call salloc (a, np, TY_INT)
	call salloc (b, np, TY_INT)

	call aclri (votes, n1*n2)

	start = 1
	do j = 1, nr2 {
	    rb1 = r2[s2[j],1]
	    if (rb1 > maxr)
		break
	    rb2 = r2[s2[j],2]
	    do i = start, nr1 {
		ra = r1[s1[i]]
		if (ra > rb2)
		    break
		if (ra < rb1) {
		    start = i + 1
		    next
		}
		call aid_lines (s1[i], n1, nn, Memi[a], Memi[a+1], Memi[a+2])
		call aid_lines (s2[j], n2, nn, Memi[b], Memi[b+1], Memi[b+2])
		call aid_addlines (aid, r1, nr1, s1[i], r2, nr2, s2[j], nn,
		    Memi[a], Memi[b], np, votes, n1, n2)
	    }
	}

        # Debug v: Print vote array.
	if (stridxs ("v", AID_DEBUG(aid,1)) != 0) {
	    call printf ("%4w")
	    do i = 1, n2 {
		call printf (" %3d")
		    call pargi (nint (x2[i]))
	    }
	    call printf ("\n")
	    do i = 1, n1 {
		call printf ("%4d")
		    call pargi (nint (x1[i]))
		do j = 1, n2 {
		    call printf (" %3d")
			call pargi (votes[i,j])
		}
		call printf ("\n")
	    }
	    call printf ("\n")
	    call flush (STDOUT)
	}

	call sfree (sp)
end


# AID_ADDLINES -- Starting with a matching triplets add more lines.
# The lines are added recursively.  To avoid recursive calls this
# routine is repeated to a maximum depth.  The indentation is intentionally
# non-standard.

procedure aid_addlines (aid, r1, nr1, s1, r2, nr2, s2, nn, a, b, npattern,
	votes, n1, n2)

pointer	aid		#I AID pointer
real	r1[nr1]		#I Reference ratios
int	nr1		#I Number of ratios
int	s1		#I Ratio index
real	r2[nr2,2]	#I Target ratios
int	nr2		#I Number of ratios
int	s2		#I Ratio index
int	nn		#I Number of neighbors
int	a[npattern]	#I Reference lines (indices)
int	b[npattern]	#I Target lines (indices)
int	npattern	#I Number of lines in pattern
int	votes[n1,n2]	#O Vote array
int	n1, n2		#O Number of reference and target lines

int	i, j, i1, j1, na, nb

begin
	na = min (a[1] + nn, n1)
	nb = min (b[1] + nn, n2)
	i1 = s1 - a[3]
	j1 = s2 - b[3]

	if (npattern > 3) {
	    for (a[4]=a[3]+1; a[4]<=na; a[4]=a[4]+1) {
		for (b[4]=b[3]+1; b[4]<=nb; b[4]=b[4]+1) {
		    i = i1 + a[4]
		    j = j1 + b[4]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 4) {
	    for (a[5]=a[4]+1; a[5]<=na; a[5]=a[5]+1) {
		for (b[5]=b[4]+1; b[5]<=nb; b[5]=b[5]+1) {
		    i = i1 + a[5]
		    j = j1 + b[5]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 5) {
	    for (a[6]=a[5]+1; a[6]<=na; a[6]=a[6]+1) {
		for (b[6]=b[5]+1; b[6]<=nb; b[6]=b[6]+1) {
		    i = i1 + a[6]
		    j = j1 + b[6]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 6) {
	    for (a[7]=a[6]+1; a[7]<=na; a[7]=a[7]+1) {
		for (b[7]=b[6]+1; b[7]<=nb; b[7]=b[7]+1) {
		    i = i1 + a[7]
		    j = j1 + b[7]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 7) {
	    for (a[8]=a[7]+1; a[8]<=na; a[8]=a[8]+1) {
		for (b[8]=b[7]+1; b[8]<=nb; b[8]=b[8]+1) {
		    i = i1 + a[8]
		    j = j1 + b[8]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 8) {
	    for (a[9]=a[8]+1; a[9]<=na; a[9]=a[9]+1) {
		for (b[9]=b[8]+1; b[9]<=nb; b[9]=b[9]+1) {
		    i = i1 + a[9]
		    j = j1 + b[9]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
	if (npattern > 9) {
	    for (a[10]=a[9]+1; a[10]<=na; a[10]=a[10]+1) {
		for (b[10]=b[9]+1; b[10]<=nb; b[10]=b[10]+1) {
		    i = i1 + a[10]
		    j = j1 + b[10]
		    if (r1[i] < r2[j,1] || r1[i] > r2[j,2])
			next
		    call aid_vote (aid, a, b, 10, votes, n1, n2)
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
	}
	}
	} else {
	    call aid_vote (aid, a, b, npattern, votes, n1, n2)
	}
end


# AID_VOTE -- Add votes for the lines in the pattern to the vote array.

procedure aid_vote (aid, a, b, npattern, votes, n1, n2)

pointer	aid		#I AID pointer
int	a[npattern]	#I Reference lines (indices)
int	b[npattern]	#I Target lines (indices)
int	npattern	#I Number of lines in pattern
int	votes[n1,n2]	#O Vote array
int	n1, n2		#O Number of reference and target lines

int	i, stridxs()
pointer	xr, xt

begin
	if (AID_CDSIGN(aid) == 1) {
	    do i = 1, npattern
		votes[a[i],b[i]] = votes[a[i],b[i]] + 1
	} else {
	    do i = 1, npattern
		votes[a[i],n2-b[i]+1] = votes[a[i],n2-b[i]+1] + 1
	}

        # Debug a: Print line assignments.
	if (stridxs ("a", AID_DEBUG(aid,1)) != 0) {
	    xr = AID_XR(aid)-1
	    xt = AID_XT(aid)-1
	    if (AID_CDSIGN(aid) == 1) {
		do i = 1, npattern {
		    call eprintf (" %6g %6g %5d")
			call pargd (Memd[xr+a[i]])
			call pargd (Memd[xt+b[i]])
			call pargi (b[i])
		}
	    } else {
		xt = xt+n2+1
		do i = 1, npattern {
		    call eprintf (" %6g %6g %5d")
			call pargd (Memd[xr+a[i]])
			call pargd (Memd[xt-b[i]])
			call pargi (n2-b[i]+1)
		}
	    }
	    call eprintf ("\n")
	}
end


# AID_SELECT -- Select top vote getters.

procedure aid_select (aid, x1, x2, x3, votes, svotes, n1, n2, x, y, w, ns)

pointer	aid		#I AID pointer
double	x1[n1]		#I Reference lines
double	x2[n2]		#I Linearized target lines
double	x3[n2]		#I Target lines
int	votes[n1,n2]	#I Vote array
int	svotes[ARB]	#I Sort indices for vote array
int	n1, n2		#I Number of lines
real	x[ns]		#O Selected target coordinates
real	y[ns]		#O Selected reference coordinates
real	w[ns]		#O Weight (votes)
int	ns		#U Maximum number on input, number selected on output

int	i, j, k, n
double	a, b
bool	check

begin
	check = (AID_CRMIN(aid) > -MAX_DOUBLE / 10. &&
	    AID_CRMAX(aid) < MAX_DOUBLE / 10.)

	# Select the highest votes.
	n = 0
	for (k=1; k<=n1*n2 && n<ns; k=k+1) {
	    i = mod (svotes[k]-1, n1) + 1 
	    j = (svotes[k]-1) / n1 + 1 
	    if (votes[i,j] < 1)
		break
	    if (check) {
		a = (x2[j] - AID_CRPIX(aid)) * AID_CDSIGN(aid) * AID_CDMIN(aid)
		b = (x2[j] - AID_CRPIX(aid)) * AID_CDSIGN(aid) * AID_CDMAX(aid)
		if (x1[i] < AID_CRMIN(aid) + min (a,b))
		    next
		if (x1[i] > AID_CRMAX(aid) + max (a,b))
		    next
	    }
	    n = n + 1
	    x[n] = x3[j]
	    y[n] = x1[i]
	    w[n] = votes[i,j]
	}
	ns = n

	if (ns < 1)
	    call error (1, "No matches found")
end


# AID_DISP -- Given a set of candidate identifications (pixel, wavelength)
# find all linear dispersions between two or more identifications which
# satisfy the dispersion constraints.  The list of ranked dispersions with
# higher rankings for higher number of points the dispersion goes through
# higher total votes for the points.  Hopefully the true dispersion will be
# in the highest ranked dispersions.

procedure aid_disp (aid, x, y, v, n, w1, dw, nw, nv, nd)

pointer	aid		#I AID pointer
real	x[n]		#I Array of candidate reference coordinates
real	y[n]		#I Array of candidate target coordinates
real	v[n]		#I Votes
int	n		#I Number of candidate pairs
real	w1[nd]		#O Dispersion origin
real	dw[nd]		#O Dispersion slope
int	nw[nd]		#O Number of points
int	nv[nd]		#O Sum of votes
int	nd		#U Number of dispersions

bool	debug, skip
int	i, j, k, l, m, ii, sumn, sumv, stridxs()
double	aw, bw, cw, sumx, sumy, sumyy, sumxy
pointer	iii

begin
	# Sort the candidates by reference coordinate.
	call xt_sort2 (x, y, n)

	debug = (stridxs ("m", AID_DEBUG(aid,1)) != 0)
	if (debug) {
	    call eprintf ("# Selected pairs with votes.\n")
	    do i = 1, n {
		call eprintf ("%4d %8.6g %8.6g %d\n")
		call pargi (i)
		call pargr (x[i])
		call pargr (y[i])
		call pargr (v[i])
	    }
	    call eprintf ("# Dispersions to check up to %d.\n")
	        call pargi (nd)
	}

	m = 0
	ii = 0
	call malloc (iii, nd, TY_INT)
	do i = 1, n-2 {
	    do j = i+1, n-1 {
		if (x[j] == x[i] || y[j] == y[i])
		    next

		bw = (x[j] - x[i]) / (y[j] - y[i])
		aw = x[i] - bw * y[i]
		cw = aw + bw * AID_CRPIX(aid)

		# Check dispersion ranges.
		skip = false
		if (abs (bw) < AID_CDMIN(aid) || abs (bw) > AID_CDMAX(aid))
		    skip = true
		else if (cw < AID_CRMIN(aid) || cw > AID_CRMAX(aid))
		    skip = true
		if (AID_CDSIGN(aid) * bw < 0.)
		    skip = true
		if (skip)
		    next

		sumn = 2
		sumv = v[i] + v[j]
		sumx = x[i] + x[j]
		sumy = y[i] + y[j]
		sumyy = y[i]*y[i] + y[j]*y[j]
		sumxy = x[i]*y[i] + x[j]*y[j]

		do k = j+1, n {
		    if (abs ((x[k] - aw - bw * y[k]) / bw) > 2.) 
			next

		    sumn = sumn + 1
		    sumv = sumv + v[k]
		    sumx = sumx + x[k]
		    sumy = sumy + y[k]
		    sumyy = sumyy + y[k]*y[k]
		    sumxy = sumxy + x[k]*y[k]
		}

		aw = (sumx*sumyy - sumy*sumxy) / (sumn * sumyy - sumy * sumy)
		bw = (sumn*sumxy - sumx*sumy) / (sumn * sumyy - sumy * sumy)
		cw = aw + bw * AID_CRPIX(aid)
		ii = ii + 1

		if (debug) {
		    call eprintf ("     %4d %4d %4d %8.5g  %8.3g  %8d %8d")
		    call pargi (ii)
		    call pargi (i)
		    call pargi (j)
		    call pargd (aw+bw*(ID_NPTS(AID_IDT(aid))/2.+1))
		    call pargd (bw)
		    call pargi (sumn)
		    call pargi (sumv)
		}

		# Check if already found.
		for (k = 1; k <= m; k = k + 1)
		    if (abs ((x[1]-aw)/bw - (x[1]-w1[k])/dw[k]) < 2. &&
		        abs ((x[n]-aw)/bw - (x[n]-w1[k])/dw[k]) < 2.)
			break
		if (k <= m) {
		    if (sumn > nw[k] || (sumn == nw[k] && sumv > nv[k])) {
			for (l = k; l > 1; l = l - 1) {
			    if (sumn<nw[l-1] || (sumn==nw[l-1] && sumv<nv[l-1]))
				break
			    w1[l] = w1[l-1]
			    dw[l] = dw[l-1]
			    nw[l] = nw[l-1]
			    nv[l] = nv[l-1]
			    Memi[iii+l-1] = Memi[iii+l-2]
			}
			if (debug) {
			    call eprintf (" replace %4d\n")
			    call pargi (Memi[iii+l-1])
			}
			w1[l] = aw
			dw[l] = bw
			nw[l] = sumn
			nv[l] = sumv
			Memi[iii+l-1] = ii
		    } else if (debug) {
			call eprintf (" use %4d\n")
			call pargi (Memi[iii+k-1])
		    }
		    next
		}

		# Check dispersion ranges.
		if (abs (bw) < AID_CDMIN(aid) || abs (bw) > AID_CDMAX(aid))
		    skip = true
		else if (cw < AID_CRMIN(aid) || cw > AID_CRMAX(aid))
		    skip = true
		if (AID_CDSIGN(aid) * bw < 0.)
		    skip = true
		if (skip) {
		    if (debug)
			call eprintf (" out of range\n")
		    next
		}

		# Add to ordered list.
		for (k = 1; k <= m; k = k + 1)
		    if (sumn > nw[k] || (sumn == nw[k] && sumv > nv[k]))
			break
		if (k <= nd) {
		    if (m < nd) {
			m = m + 1
			if (debug)
			    call eprintf (" add\n")
		    } else if (debug) {
			call eprintf (" bump %4d\n")
			    call pargi (Memi[iii+m-1])
		    }
		    for (l = m; l > k; l = l - 1) {
			w1[l] = w1[l-1]
			dw[l] = dw[l-1]
			nw[l] = nw[l-1]
			nv[l] = nv[l-1]
			Memi[iii+l-1] = Memi[iii+l-2]
		    }
		    w1[k] = aw
		    dw[k] = bw
		    nw[k] = sumn
		    nv[k] = sumv
		    Memi[iii+k-1] = ii
		} else if (debug)
		    call eprintf (" failed\n")
	    }
	}

	nd = m

	if (debug) {
	    call eprintf ("# Final ordered dispersions to try.\n")
	    do i = 1, nd {
		call eprintf ("     %4d %8.5g  %8.3g  %8d %8d\n")
		call pargi (Memi[iii+i-1])
		call pargr (w1[i]+dw[i]*(ID_NPTS(AID_IDT(aid))/2.+1))
		call pargr (dw[i])
		call pargi (nw[i])
		call pargi (nv[i])
	    }
	}
	call mfree (iii, TY_INT)

        # Debug d: Graph dispersions.
	if (stridxs ("d", AID_DEBUG(aid,1)) != 0)
	    call aid_dgraph (aid, x, y, n, w1, dw, nd)
end


# AID_EVAL -- Evaluate possible solutions.

double procedure aid_eval (aid, ev, nd)

pointer	aid		#I AID pointer
pointer	ev		#I EV pointer
int	nd		#I Dispersion candidate to evaluate
double	best		#O Best statistic

int	i, n
pointer	idt, x, y
double	a, b, c, d, rms, fmatch, ftmatch
int	stridxs()

int	ncandidate, nmatch1, nmatch2
common	/llstat/ ncandidate, nmatch1, nmatch2

define	done_	90

begin
	best = INDEFD
	if (nd > AID_ND(ev))
	    return (best)

	idt = AID_IDT(aid)
	x = AID_X(ev) - 1
	y = AID_Y(ev) - 1
	n = AID_N(ev)

	a = Memr[AID_A(ev)+nd-1]
	b = Memr[AID_B(ev)+nd-1]
	c = ID_NPTS(AID_IDT(aid)) / 2. + 1
	if (IS_INDEFD(AID_CDELT(aid)))
	    d = b
	else
	    d = AID_CDELT(aid)

	ID_IC(idt) = AID_IC1(aid)
	ID_NFEATURES(idt) = 0
	do i = 1, n {
	    if (abs ((Memr[y+i] - a - b * Memr[x+i]) / b) < 2.)
		call id_newfeature (idt, double(Memr[x+i]), double(Memr[x+i]),
		    double(Memr[y+i]), 1.0D0, ID_FWIDTH(idt), ID_FTYPE(idt),
		    NULL)
	}
	if (ID_NFEATURES(idt) <= 1)
	    goto done_

	call dcvfree (ID_CV(idt))
	iferr (call aid_dofit (aid, idt, d, rms, fmatch, ftmatch, best))
	    goto done_

	# Debug s: Print search iterations.
	if (stridxs ("s", AID_DEBUG(aid,1)) != 0) {
	    call eprintf (
	"%2d/%-2d %8.2f %8.3f %3d %3d/%-3d %3d/%-3d %3d %3d %6.3f %5.2f\n")
		call pargi (AID_BIN1(ev))
		call pargi (AID_BIN2(ev))
		call pargd (a+c*b)
		call pargd (b)
		call pargi (ID_NFEATURES(idt))
		call pargi (nmatch2)
		call pargi (ncandidate)
		call pargi (nint(min (ncandidate, AID_NT(aid))*(1-ftmatch)))
		call pargi (min (ncandidate, AID_NT(aid)))
		call pargi (nint(100.*fmatch))
		call pargi (nint(100.*ftmatch))
		call pargd (rms)
		call pargd (best)
	}

	if (best < AID_BEST(aid)) {
	    AID_FMATCH(aid) = fmatch
	    AID_FTMATCH(aid) = ftmatch
	    AID_RMS(aid) = rms
	    AID_BEST(aid) = best
	    ID_IC(idt) = AID_IC2(aid)
	    call id_saveid (idt, "autoidentify")
	}

done_
	ID_IC(idt) = AID_IC2(aid)
	return (best)
end


# AID_DOFIT -- From a set of candidate identifications fit and evaluate
# a dispersion solution.

procedure aid_dofit (aid, id, cdelt, rms, fmatch, ftmatch, best)

pointer	aid		#I AID pointer
pointer	id		#I ID pointer
double	cdelt		#I Dispersion to use in pixel rms conversion
double	rms		#O Final RMS in pixels
double	fmatch		#O Line list non-matching fraction
double	ftmatch		#O Target line non-matching fraction
double	best		#O Best fit parameter

int	i, j, k, l, nmin, nfound, nt, ntmatch, maxfeatures, stridxs()
double	fit, user, dcveval(), id_fitpt()
pointer	cv, xt, label

int	ncandidate, nmatch1, nmatch2
common	/llstat/ ncandidate, nmatch1, nmatch2

errchk	id_dofit, id_fitdata, id_fitfeatures, id_linelist, id_match

begin
	maxfeatures = ID_MAXFEATURES(id)
	ID_MAXFEATURES(id) = 1000
	iferr {
	    do k = 1, 3 {
		if (ID_NFEATURES(id) < 2)
		    call error (0, "aid_dofit: not enough features")
		if (k > 1)
		    call id_linelist (id)

		if (stridxs ("i", AID_DEBUG(aid,1)) != 0)
		    call id_dofit (id, YES)
		else
		    call id_dofit (id, NO)
		do l = AID_ORD(aid)-1, 2, -1 {
		    cv = ID_CV(id)
		    user = dcveval (cv, 1D0)
		    fit = (dcveval (cv, double (ID_NPTS(id)/2)) - user) /
			(dcveval (cv, double (ID_NPTS(id))) - user)
		    if (abs (fit - 0.5) <= AID_MAXNL(aid))
		        break
		    if (stridxs ("n", AID_DEBUG(aid,1)) != 0) {
			call eprintf (
			    "order %d: non-linearity of %.1f%% > %.1f%%\n")
			call pargi (l+1)
			call pargd (100*abs(fit-0.5))
			call pargr (100*AID_MAXNL(aid))
		    }
		    call ic_puti (ID_IC(id), "order", l)
		    if (stridxs ("i", AID_DEBUG(aid,1)) != 0)
			call id_dofit (id, YES)
		    else
			call id_dofit (id, NO)
		    call ic_puti (ID_IC(id), "order", AID_ORD(aid))
		}
		call id_fitdata (id)
		call id_fitfeatures (id)
		
		j = 0
		do i = 1, ID_NFEATURES(id) {
		    if (IS_INDEFD(USER(id,i)) || WTS(id,i) != 0.) {
			j = j + 1
			PIX(id,j) = PIX(id,i)
			FIT(id,j) = FIT(id,i)
			USER(id,j) = USER(id,i)
			WTS(id,j) = WTS(id,i)
			FWIDTH(id,j) = FWIDTH(id,i)
			FTYPE(id,j) = FTYPE(id,i)
		    }
		}
		ID_NFEATURES(id) = j
	    }
	    ID_MAXFEATURES(id) = maxfeatures
	} then {
	    ID_MAXFEATURES(id) = maxfeatures
	    call erract (EA_ERROR)
	}
	if (IS_INDEFD(cdelt))
	    return

	nmin = 2
        nfound = AID_NFOUND(aid)
	if (ID_NFEATURES(id) < nfound)
	    call error (0, "aid_dofit: not enough features")

	# Compute fwidth rms.
	rms = 0.
	for (i=1; i<=ID_NFEATURES(id); i=i+1)
	    rms = rms + (FIT(id,i) - USER(id,i)) ** 2
	rms = sqrt (rms/ max (1, ID_NFEATURES(id)-nmin)) / abs (cdelt)
	rms = rms / ID_FWIDTH(id)

	# Compute line list matching fraction.
	ncandidate = max (nfound, (ncandidate-(nmatch1-nmatch2)))
	fmatch = 1 - real (nmatch2) / ncandidate

	# Compute target line matching fraction.
	xt = AID_XT(aid)
	nt = AID_NT(aid)
	label = NULL
	ntmatch = 0
	do i = 1, nt {
	    fit = id_fitpt (id, Memd[xt+i-1])
	    user = INDEFD
	    call id_match (id, fit, user, label, ID_MATCH(id))
	    if (!IS_INDEFD(user))
		ntmatch = ntmatch + 1
	}
	ftmatch = 1 - real (ntmatch) / min (nt, ncandidate)
	call mfree (label, TY_CHAR)

	if (AID_RMSG(aid) > 0. && AID_FMATCHG(aid) > 0.) {
	    best = AID_WRMS(aid) *  rms / AID_RMSG(aid)
	    best = best + AID_WFMATCH(aid) * fmatch / AID_FMATCHG(aid)
	    best = best + AID_WFTMATCH(aid) * ftmatch / AID_FMATCHG(aid)
	} else
	    best = MAX_DOUBLE
end


# AID_DOFITF -- From a set of candidate identifications fit and evaluate
# a final dispersion solution.

procedure aid_dofitf (aid, id)

pointer	aid		#I AID pointer
pointer	id		#I ID pointer

int	i, j, k, maxfeatures

errchk	id_dofit, id_fitdata, id_fitfeatures, id_linelist

begin
	maxfeatures = ID_MAXFEATURES(id)
	ID_MAXFEATURES(id) = 1000
	iferr {
	    do k = 1, 3 {
		if (ID_NFEATURES(id) < 2)
		    call error (0, "aid_dofit: not enough features")
		if (k > 1)
		    call id_linelist (id)

		call id_dofit (id, NO)
		call id_fitdata (id)
		call id_fitfeatures (id)
		if (k < 3) {
		    j = 0
		    do i = 1, ID_NFEATURES(id) {
			if (IS_INDEFD(USER(id,i)) || WTS(id,i) != 0.) {
			    j = j + 1
			    PIX(id,j) = PIX(id,i)
			    FIT(id,j) = FIT(id,i)
			    USER(id,j) = USER(id,i)
			    WTS(id,j) = WTS(id,i)
			    FWIDTH(id,j) = FWIDTH(id,i)
			    FTYPE(id,j) = FTYPE(id,i)
			}
		    }
		    ID_NFEATURES(id) = j
		}
	    }
	    ID_MAXFEATURES(id) = maxfeatures
	} then {
	    ID_MAXFEATURES(id) = maxfeatures
	    call erract (EA_ERROR)
	}
end


# AID_EVALLOC -- Allocate memory to save the candidate identifications
# and dispersions to be evaluated.

pointer procedure aid_evalloc (aid, index)

pointer	aid		#I AID pointer
int	index		#I Reference sample index

begin
	if (AID_EVS(aid) == NULL)
	    call calloc (AID_EVS(aid), (index+49)*AID_EVLEN, TY_POINTER)
	else if (index > 1 && mod (index-1, 50) == 0) {
	    call realloc (AID_EVS(aid), (index+49)*AID_EVLEN, TY_POINTER)
	    call aclri (Memi[AID_EVS(aid)+(index-1)*AID_EVLEN], 50*AID_EVLEN)
	}
	return (AID_EVS(aid)+(index-1)*AID_EVLEN)
end


# AID_EVFREE -- Free memory from the evaluation step.

procedure aid_evfree (aid, index)

pointer	aid		#I AID pointer
int	index		#I Reference sample index

pointer	ev, aid_evalloc()

begin
	ev = aid_evalloc (aid, index)
	call mfree (AID_X(ev), TY_REAL)
	call mfree (AID_Y(ev), TY_REAL)
	call mfree (AID_A(ev), TY_REAL)
	call mfree (AID_B(ev), TY_REAL)
end


# AID_IMGD -- Get value from image header or parameter string.

double procedure aid_imgd (im, param)

pointer	im			#I IMIO pointer
char	param[ARB]		#I Parameter

int	i, ctod()
double	dval, imgetd()

begin
	if (param[1] == '!') {
	    iferr (dval = imgetd (im, param[2]))
		dval = INDEFD
	} else {
	    iferr (dval = imgetd (im, param)) {
		i = 1
		if (ctod (param, i, dval) == 0)
		    dval = INDEFD
	    }
	}
	return (dval)
end
