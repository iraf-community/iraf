include <imhdr.h>
include "quadgeom.h"

# QUADGEOM -- Set up section information in quadgeom structure based on 
# information in the image header. The sections given in the image header are
# "whole image" sections (i.e. those that would be appropriate for single
# readout. From these we must calculate the sections to apply to the data 
# read through each readout. The values of datasec and ccdsec are taken from 
# the header. The values of trimsec and biassec can be supplied explicitly
# via the corresponding arguments. If these are given as "image" or "" then the
# image header values are used.

procedure quadgeom (im, qg, trimsec, biassec)

pointer	im			#I  Pointer to input image.
pointer	qg			#IO Pointer to open quadgeom structure.
char	trimsec[SZ_LINE]	#I  Trimsec may be used to overide header value.
char	biassec[SZ_LINE]	#I  Biassec may be used to overide header value.

char	section[SZ_LINE], nampsyx[SZ_LINE]
int	nx, ny, xdata, ydata, xover, yover, amp, xamp, yamp, pre
int	dx1, dx2, dxs, dy1, dy2, dys
int	ddx1, ddx2, ddy1, ddy2
int	cx1, cx2, cxs, cy1, cy2, cys
int	ccx1, ccx2, ccy1, ccy2
int	tx1, tx2, txs, txskip1, txskip2
int	ty1, ty2, tys, tyskip1, tyskip2
int	ttx1, ttx2, tty1, tty2
int	bx1, bx2, bxs, bxskip1, bxskip2
int	by1, by2, bys, byskip1, byskip2
int	bbx1, bbx2, bby1, bby2

bool	streq()

begin

	# Get input image dimensions.
	nx  = IM_LEN(im, 1)
	ny  = IM_LEN(im, 2)

	# Get number of active amplifiers in Y and X.
	call hdmgstr (im, "nampsyx", nampsyx, SZ_LINE)
	call sscan (nampsyx)
	    call gargi (QG_NAMPSY(qg))
	    call gargi (QG_NAMPSX(qg))
	
	QG_NAMPS(qg) = QG_NAMPSY(qg) * QG_NAMPSX(qg)
	if (QG_NAMPS(qg) > QG_MAXAMPS)
	    call error (0, "CCD has two many read-outs for this program")

	# Get list of active amplifiers.
	# Presently the header doesn't contain this information so we fake it
	# since we know all the posibilities.
	do amp = 1, QG_NAMPS(qg)
	    call malloc (QG_AMPID(qg, amp), SZ_AMPID, TY_CHAR)

	switch (QG_NAMPSX(qg)) {
	case 1:
	    switch (QG_NAMPSY(qg)) {
	    case 1:	# Mono
		QG_AMPTYPE (qg, 1) = AMP11
		call strcpy ("11", Memc[QG_AMPID(qg, 1)], SZ_AMPID)

	    case 2:	# Split parallels
		call error (0, "Unsuported read-out configuration")
	    }

	case 2:

	    switch (QG_NAMPSY(qg)) {
	    case 1:	# Split serials
		QG_AMPTYPE (qg, 1) = AMP11
		call strcpy ("11", Memc[QG_AMPID(qg, 1)], SZ_AMPID)
		QG_AMPTYPE (qg, 2) = AMP12
		call strcpy ("12", Memc[QG_AMPID(qg, 2)], SZ_AMPID)

	    case 2:	# Quad
		QG_AMPTYPE (qg, 1) = AMP11
		call strcpy ("11", Memc[QG_AMPID(qg, 1)], SZ_AMPID)
		QG_AMPTYPE (qg, 2) = AMP12
		call strcpy ("12", Memc[QG_AMPID(qg, 2)], SZ_AMPID)
		QG_AMPTYPE (qg, 3) = AMP21
		call strcpy ("21", Memc[QG_AMPID(qg, 3)], SZ_AMPID)
		QG_AMPTYPE (qg, 4) = AMP22
		call strcpy ("22", Memc[QG_AMPID(qg, 4)], SZ_AMPID)
	    }
	}

	# Set X and Y dimensions of subimage read out by each amplifier
	QG_NX (qg, 0) = nx
	QG_NY (qg, 0) = ny
	do amp = 1, QG_NAMPS (qg) {
	    QG_NX(qg, amp) = nx / QG_NAMPSX (qg)
	    QG_NY(qg, amp) = ny / QG_NAMPSY (qg)
	}

	# Get datasec, trimsec and biassec parameters from image header.
	# trimsec and biassec may be overidden by supplying an explicit
	# section in the biassec and trimsec arguments.
	call hdmgstr (im, "datasec", section, SZ_LINE)
	dx1 = 1
	dx2 = nx
	dxs = 1
	dy1 = 1
	dy2 = ny
	dys = 1
	call ccd_section (section, dx1, dx2, dxs, dy1, dy2, dys)
	QG_DX1(qg, 0) = dx1
	QG_DX2(qg, 0) = dx2
	QG_DY1(qg, 0) = dy1
	QG_DY2(qg, 0) = dy2

	if (streq (trimsec, "image") || streq (trimsec, "")) {
	    call hdmgstr (im, "trimsec", section, SZ_LINE)
	} else {
	    call strcpy (trimsec, section, SZ_LINE)
	}
	tx1 = dx1
	tx2 = dx2
	txs = 1
	ty1 = dy1
	ty2 = dy2
	tys = 1
	call ccd_section (section, tx1, tx2, txs, ty1, ty2, tys)
	QG_TX1(qg, 0) = tx1
	QG_TX2(qg, 0) = tx2
	QG_TY1(qg, 0) = ty1
	QG_TY2(qg, 0) = ty2

	if (streq (biassec, "image") || streq (biassec, "")) {
	    call hdmgstr (im, "biassec", section, SZ_LINE)
	} else {
	    call strcpy (biassec, section, SZ_LINE)
	}
	bx1 = dx2 + 1
	bx2 = nx
	bxs = 1
	by1 = 1
	by2 = ny
	bys = 1
	call ccd_section (section, bx1, bx2, bxs, by1, by2, bys)
	QG_BX1(qg, 0) = bx1
	QG_BX2(qg, 0) = bx2
	QG_BY1(qg, 0) = by1
	QG_BY2(qg, 0) = by2

	call hdmgstr (im, "ccdsec", section, SZ_LINE)
	cx1 = dx1
	cx2 = dx2
	cxs = 1
	cy1 = dy1
	cy2 = dy2
	cys = 1
	call ccd_section (section, cx1, cx2, cxs, cy1, cy2, cys)
	QG_CX1(qg, 0) = cx1
	QG_CX2(qg, 0) = cx2
	QG_CY1(qg, 0) = cy1
	QG_CY2(qg, 0) = cy2

	# Calculate number of data pixels and margins to leave around
	# trimsection.
	xdata = dx2 - dx1 + 1
	ydata = dy2 - dy1 + 1
	txskip1 = tx1 - dx1
	# *************			KLUDGE!		*********************
	# The datasec is the whole image. We have no way of knowing where the
	# division between data and overscan is supposed to be so we assume 
	# that trimsec leaves an equal margin on both sides of the true datasec.
	if ((dx1 == 1 ) && (dx2 == nx)) {	
	    dx2 = tx2 + txskip1 	
	    xdata = dx2 - dx1 + 1
	    cx2 = cx1 + xdata - 1
	    QG_DX2(qg, 0) = dx2
	    QG_CX2(qg, 0) = cx2
	}
	txskip2 = dx2 - tx2
	tyskip1 = ty1 - dy1
	tyskip2 = dy2 - ty2

	# Calculate number of overscan pixels and margins to leave around
	# biassec.
	xover   = nx  - xdata
	yover   = ny
	bxskip1 = bx1 - dx2 - 1
	bxskip2 = nx  - bx2
	byskip1 = by1 - dy1
	byskip2 = ny  - by2

	# Calculate number of data and overscan pixels in subimages
	xdata = xdata / QG_NAMPSX(qg)
	ydata = ydata / QG_NAMPSY(qg)
	xover = xover / QG_NAMPSX(qg)
	yover = yover / QG_NAMPSY(qg)

	# Calculate datasec, trimsec, etc. for each amplifier
	do amp = 1, QG_NAMPS(qg) {

	    # Assume there are no phantoms
	    QG_PHANTOM (qg, amp) = NO

	    # X coordinates
	    switch (QG_AMPTYPE(qg, amp)) {
	    case AMP11, AMP21:	# Left hand side
		ddx1 = dx1
		ddx2 = ddx1 + xdata - 1
		ttx1 = ddx1 + txskip1
		ttx2 = ddx2 
		bbx1 = ddx2 + bxskip1 + 1
		bbx2 = ddx2 + xover - bxskip2
		ccx1 = cx1
		ccx2 = cx1  + xdata - 1

	    case AMP12, AMP22:	# Right hand side
		bbx1 = bxskip2 + 1
		bbx2 = xover - bxskip1 
		ddx1 = xover + 1
		ddx2 = ddx1 + xdata - 1
		ttx1 = ddx1
		ttx2 = ddx2 - txskip2 
		ccx1 = cx1 +  xdata 
		ccx2 = cx2
	    }

	    # Y Coordinates
	    switch (QG_AMPTYPE(qg, amp)) {
	    case AMP11, AMP12:	# Lower row
		ddy1 = dy1
		ddy2 = ddy1 + ydata - 1
		tty1 = ddy1 + tyskip1
		bby1 = ddy1 + byskip1 
		if (QG_NAMPSY(qg) == 1) {
		    tty2 = ddy2 - tyskip2
		    bby2 = ddy2 - byskip2
		} else {
		    tty2 = ddy2
		    bby2 = ddy2
		}
		ccy1 = cy1
		ccy2 = cy1  + ydata - 1

	    case AMP21, AMP22:	# Upper row
		ddy1 = 1
		ddy2 = ddy1 + ydata - 1
		if (QG_NAMPSY(qg) == 1) {
		    tty1 = ddy1 + tyskip1
		    bby1 = ddy1 + byskip1 
		} else {
		    tty1 = 1
		    bby1 = 1
		}
		tty2 = ddy2 - tyskip2
		bby2 = ddy2 - byskip2
		ccy1 = cy1  + ydata 
		ccy2 = cy2
	    }


	    QG_DX1(qg, amp) = ddx1
	    QG_DX2(qg, amp) = ddx2
	    QG_DY1(qg, amp) = ddy1
	    QG_DY2(qg, amp) = ddy2

	    QG_TX1(qg, amp) = ttx1
	    QG_TX2(qg, amp) = ttx2
	    QG_TY1(qg, amp) = tty1
	    QG_TY2(qg, amp) = tty2

	    QG_BX1(qg, amp) = bbx1
	    QG_BX2(qg, amp) = bbx2
	    QG_BY1(qg, amp) = bby1
	    QG_BY2(qg, amp) = bby2

	    QG_CX1(qg, amp) = ccx1
	    QG_CX2(qg, amp) = ccx2
	    QG_CY1(qg, amp) = ccy1
	    QG_CY2(qg, amp) = ccy2
	}

	# Set up "ampsec" - the section of the composite image derived from
	# each sub-image.
	do yamp = 1, QG_NAMPSY(qg) {
	    amp = QG_AMP (qg, 1, yamp)
	    QG_AX1(qg, amp) = 1
	    QG_AX2(qg, amp) = QG_NX(qg, amp)
	    do xamp = 2, QG_NAMPSX(qg) {
		amp = QG_AMP (qg, xamp,   yamp)
		pre = QG_AMP (qg, xamp-1, yamp)
		QG_AX1(qg, amp) = QG_AX2(qg, pre) + 1
		QG_AX2(qg, amp) = QG_AX1(qg, amp) + QG_NX(qg, amp) - 1
	    }
	}
	do xamp = 1, QG_NAMPSX(qg) {
	    amp = QG_AMP (qg, xamp, 1)
	    QG_AY1(qg, amp) = 1
	    QG_AY2(qg, amp) = QG_NY(qg, amp)
	    do yamp = 2, QG_NAMPSY(qg) {
		amp = QG_AMP (qg, xamp, yamp)
		pre = QG_AMP (qg, xamp, yamp-1)
		QG_AY1(qg, amp) = QG_AY2(qg, pre) + 1
		QG_AY2(qg, amp) = QG_AY1(qg, amp) + QG_NY(qg, amp) - 1
	    }
	}

end
