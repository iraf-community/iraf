include	<mach.h>
include	<gset.h>
include	<pkg/gtools.h>
include	<smw.h>
include	"../identify.h"
include	"autoid.h"


# AID_LGRAPH -- Graph target and reference spectra and associated lines.
# This is only used for debugging.

procedure aid_lgraph (aid, x1, n1, x2, n2)

pointer	aid		#I AID pointer
double	x1[n1]		#I Reference lines
int	n1		#I Number of reference lines
double	x2[n2]		#I Target lines
int	n2		#I Number of target lines

int	i, wcs, key, nr, nt, redraw, clgcur(), stridxs()
real	wx, wy, wz, a, b, c, d, dy, ytmin, ytmax
pointer	sp, cmd, id, sht, shr, gp, gt, xr, yr, yt, y, gt_init()
double	shdr_lw()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	id = AID_IDT(aid)
	sht = ID_SH(id)
	shr = ID_SH(AID_IDR(aid))

	gp = ID_GP(id)
	if (gp == NULL)
	    return
	gt = gt_init()
	call gt_sets (gt, GTTYPE, "line")
	call gt_seti (gt, GTSYSID, NO)
	if (DC(sht) == DCNO || WP(sht) * AID_CDSIGN(aid) < 0.) {
	    call gt_setr (gt, GTXMIN, AID_W1(aid))
	    call gt_setr (gt, GTXMAX, AID_W2(aid))
	} else {
	    call gt_setr (gt, GTXMIN, W0(sht))
	    call gt_setr (gt, GTXMAX, W1(sht))
	}

	if (shr != NULL) {
	    xr = SX(shr) + AID_X1R(aid) - 1
	    yr = AID_SPECR(aid)
	    nr = AID_X2R(aid) - AID_X1R(aid) + 1
	}

	nt = ID_NPTS(id)
	yt = ID_IMDATA(id)
	call alimr (Memr[yt], nt, ytmin, ytmax)

	call malloc (y, max(nr,nt), TY_REAL)

	key = 'r'
	repeat {
	    switch (key) {
	    case ':':
		call gt_colon (Memc[cmd], gp, gt, redraw)
	    case 'Q':
		i = stridxs ("l", AID_DEBUG(aid,1))
		AID_DEBUG(aid,i) = ' '
		break
	    case 'q':
		break
	    case 'r':
		redraw = YES
	    case 'w':
		call gt_window (gt, gp, "gcur", redraw)
	    }

	    if (redraw == YES) {
		call gclear (gp)
		call gseti (gp, G_YDRAWTICKS, NO)
		if (shr != NULL) {
		    call gascale (gp, Memr[xr], nr, 1)
		    call gascale (gp, Memr[yr], nr, 2)
		} else {
		    call gswind (gp, real(x1[1]), real(x1[n1]), 0., 1.)
		}
		call gt_swind (gp, gt)
		call ggwind (gp, a, b, c, d)
		dy = 2 * (d - c)
		call gswind (gp, a, b, c, c + dy)
		call gt_labax(gp, gt)

		if (shr != NULL) {
		    call aminkr (Memr[yr], c + 0.44 * dy, Memr[y], nr)
		    call gt_plot (gp, gt, Memr[xr], Memr[y], nr)
		}

		wy = c + 0.46 * dy
		wz = c + 0.49 * dy
		do i = 1, n1 {
		    wx = x1[i]
		    if (wx < min (a,b) || wx > max (a,b))
			next
		    call gline (gp, wx, wy, wx, wz)
		}

		call amapr (Memr[yt], Memr[y], nt,
		    ytmin, ytmax, c+0.55*dy, c+0.99*dy)
		wy = c + 0.50 * dy
		wz = c + 0.53 * dy

		if (DC(sht) == DCNO || WP(sht) * AID_CDSIGN(aid) < 0.) {
		    call gvline (gp, Memr[y], nt, a, b)
		    do i = 1, n2 {
			wx = a + (x2[i] - 1) / (nt - 1) * (b - a) 
			call gline (gp, wx, wy, wx, wz)
		    }
		} else {
		    call gpline (gp, Memr[SX(sht)], Memr[y], nt)
		    do i = 1, n2 {
			wx = shdr_lw (sht, double (x2[i]))
			call gline (gp, wx, wy, wx, wz)
		    }
		}

		redraw = NO
	    }
	} until (clgcur ("gcur", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)
	    
	call gdeactivate (gp, 0)
	call mfree (y, TY_REAL)
	call gt_free (gt)
	call sfree (sp)
end


# AID_DGRAPH -- Graph candidate dispersions.
# This routine is only used for debugging.

procedure aid_dgraph (aid, x, y, n, w1, dw, nd)

pointer	aid		#I AID pointer
real	x[n]		#I Array of candidate reference coordinates (sorted)
real	y[n]		#I Array of candidate target coordinates
int	n		#I Number of candidate pairs
real	w1[nd]		#I Dispersion origin
real	dw[nd]		#I Dispersion slope
int	nd		#I Number of dispersions

int	i, ndplot, wcs, key, redraw, clgcur(), stridxs(), ctoi()
real	wx, wy, a, b, c, d, dy, crpix, crval, cdelt
pointer	sp, cmd, sh, gp, gt, gt_init()

begin
	call smark (sp)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	sh = ID_SH(AID_IDT(aid))
	gp = ID_GP(AID_IDT(aid))
	if (gp == NULL)
	    return
	gt = gt_init()
	call gt_seti (gt, GTSYSID, NO)
	if (DC(sh) != DCNO) {
	    call gt_setr (gt, GTXMIN, W0(sh))
	    call gt_setr (gt, GTXMAX, W1(sh))
	    call gt_setr (gt, GTYMIN, 1.)
	    call gt_setr (gt, GTYMAX, real(SN(sh)))
	}

	ndplot = nd
	key = 'r'
	repeat {
	    switch (key) {
	    case ':':
		if (Memc[cmd] == '/')
		    call gt_colon (Memc[cmd], gp, gt, redraw)
		else {
		    i = 1
		    if (ctoi (Memc[cmd], i, ndplot) == 0)
			ndplot = nd
		}
	    case 'Q':
		i = stridxs ("d", AID_DEBUG(aid,1))
		AID_DEBUG(aid,i) = ' '
		break
	    case 'q':
		break
	    case 'r':
		redraw = YES
	    case 'w':
		call gt_window (gt, gp, "gcur", redraw)
	    }

	    if (redraw == YES) {
		call gclear (gp)
		call gascale (gp, x, n, 1)
		call gascale (gp, y, n, 2)
		call gt_swind (gp, gt)
		call gt_labax(gp, gt)

		call gt_plot (gp, gt, x, y, n)

		call ggwind (gp, a, b, c, d)
		dy = (b - a) / 500.
		do i = 1, ndplot {
		    crval = w1[i]
		    cdelt = dw[i]
		    wy = c
		    wx = crval + wy * cdelt
		    call gamove (gp, wx, wy)
		    for (wy=wy+dy; wy<d+dy; wy=wy+dy) {
			wx = crval + wy * cdelt
			call gadraw (gp, wx, wy)
		    }
		}

		if (AID_CRMIN(aid) > -MAX_DOUBLE / 10. &&
		    AID_CRMAX(aid) < MAX_DOUBLE / 10.) {
		    crpix = AID_CRPIX(aid)
		    crval = AID_CDSIGN(aid) * AID_CDMIN(aid)
		    cdelt = AID_CDSIGN(aid) * AID_CDMAX(aid)
		    for (wy=c; wy<d+dy; wy=wy+dy) {
			wx = AID_CRMIN(aid) +
			    min ((wy-crpix)*crval, (wy-crpix)*cdelt)
			call gmark (gp, wx, wy, GM_POINT, 2, 2)
		    }
		    for (wy=c; wy<d+dy; wy=wy+dy) {
			wx = AID_CRMAX(aid) +
			    max ((wy-crpix)*crval, (wy-crpix)*cdelt)
			call gmark (gp, wx, wy, GM_POINT, 2, 2)
		    }
		}

		redraw = NO
	    }
	} until (clgcur ("gcur", wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)
	    
	call gdeactivate (gp, 0)
	call gt_free (gt)
	call sfree (sp)
end
