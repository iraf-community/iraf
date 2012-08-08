include <imhdr.h>
include "quadgeom.h"

# QUADGEOMRED -- Set up section information in quadgeom structure based on 
# information in the image header for a reduced image. The sections given in the
# image header are "whole image" sections (i.e. those that would be appropriate
# for single  readout. From these we must calculate the sections to apply to
# the data read through each readout.

procedure quadgeomred (im, qg)

pointer	im			#I  Pointer to input image.
pointer	qg			#IO Pointer to open quadgeom structure.

char	section[SZ_LINE], keyword[SZ_LINE], nampsyx[SZ_LINE]
int	nx, ny, x, y, amp, pre
int	dx1, dx2, dxs, dy1, dy2, dys
int	cx1, cx2, cxs, cy1, cy2, cys
int	ax1, ax2, axs, ay1, ay2, ays

begin

	# Get input image dimensions.
	nx  = IM_LEN(im, 1)
	ny  = IM_LEN(im, 2)
	QG_NX (qg, 0) = nx
	QG_NY (qg, 0) = ny

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


	# Get datasec.
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

	# Get ccdsec.
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


	do amp = 1, QG_NAMPS (qg) {

	    # Get AMPSECmn for each readout
	    call sprintf (keyword, SZ_LINE, "AMPSEC%s")
		call pargstr (Memc[QG_AMPID(qg, amp)])
	    call hdmgstr (im, keyword, section, SZ_LINE)
	    ax1 = 1
	    ax2 = nx
	    axs = 1
	    ay1 = 1
	    ay2 = ny
	    ays = 1
	    call ccd_section (section, ax1, ax2, axs, ay1, ay2, ays)
	    QG_AX1(qg, amp) = ax1
	    QG_AX2(qg, amp) = ax2
	    QG_AY1(qg, amp) = ay1
	    QG_AY2(qg, amp) = ay2


	    # Set X and Y dimensions of subimage read out by each amplifier
	    QG_NX(qg, amp) = ax2 - ax1 + 1
	    QG_NY(qg, amp) = ay2 - ay1 + 1

	    # Set datsec and trimsec for each sub image
	    QG_DX1(qg, amp) = 1
	    QG_DX2(qg, amp) = QG_NX(qg, amp)
	    QG_DY1(qg, amp) = 1
	    QG_DY2(qg, amp) = QG_NY(qg, amp)

	    QG_TX1(qg, amp) = 1
	    QG_TX2(qg, amp) = QG_NX(qg, amp)
	    QG_TY1(qg, amp) = 1
	    QG_TY2(qg, amp) = QG_NY(qg, amp)
	}

	# Determine ccdsec for each each sub-image.
	do y = 1, QG_NAMPSY(qg) {
	    amp = QG_AMP (qg, 1, y)
	    QG_CX1(qg, amp) = QG_CX1(qg, 0)
	    QG_CX2(qg, amp) = QG_CX1(qg, amp) + QG_NX(qg, amp) - 1
	    do x = 2, QG_NAMPSX(qg) {
		amp = QG_AMP (qg, x,   y)
		pre = QG_AMP (qg, x-1, y)
		QG_CX1(qg, amp) = QG_CX2(qg, pre) + 1
		QG_CX2(qg, amp) = QG_CX1(qg, amp) + QG_NX(qg, amp) - 1
	    }
	}
	do x = 1, QG_NAMPSX(qg) {
	    amp = QG_AMP (qg, x, 1)
	    QG_CY1(qg, amp) = QG_CY1(qg, 0)
	    QG_CY2(qg, amp) = QG_CY1(qg, amp) + QG_NY(qg, amp) - 1
	    do y = 2, QG_NAMPSY(qg) {
		amp = QG_AMP (qg, x, y)
		pre = QG_AMP (qg, x, y-1)
		QG_CY1(qg, amp) = QG_CY2(qg, pre) + 1
		QG_CY2(qg, amp) = QG_CY1(qg, amp) + QG_NY(qg, amp) - 1
	    }
	}
end
