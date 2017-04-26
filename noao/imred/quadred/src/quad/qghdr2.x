include <imhdr.h>
include "quadgeom.h"

define	SZ_KEYWRD	8		# Chars in FITS keyword

# QGHDR2 -- Set up section information in quadgeom structure based on 
# information in the image header.

procedure qghdr2 (im, qg)

pointer	im			#I  Pointer to input image.
pointer	qg			#IO Pointer to open quadgeom structure.

pointer sp, keyword, hdrvalue, section
int	amp
int	ax1, ax2, axs, ay1, ay2, ays
int	bx1, bx2, bxs, by1, by2, bys
int	cx1, cx2, cxs, cy1, cy2, cys
int	dx1, dx2, dxs, dy1, dy2, dys
int	tx1, tx2, txs, ty1, ty2, tys

int	hdmaccf()

begin

	# Get stack space
	call smark (sp)
	call salloc (keyword,  SZ_KEYWRD, TY_CHAR)
	call salloc (hdrvalue, SZ_LINE,   TY_CHAR)
	call salloc (section,  SZ_LINE,   TY_CHAR)

	# Get input image dimensions.
	QG_NX (qg, 0) = IM_LEN(im, 1)
	QG_NY (qg, 0) = IM_LEN(im, 2)

	# Get number of active amplifiers in Y and X.
	call hdmgstr (im, "nampsyx", Memc[hdrvalue], SZ_LINE)
	call sscan (Memc[hdrvalue])
	    call gargi (QG_NAMPSY(qg))
	    call gargi (QG_NAMPSX(qg))
	
	QG_NAMPS(qg) = QG_NAMPSY(qg) * QG_NAMPSX(qg)
	if (QG_NAMPS(qg) > QG_MAXAMPS)
	    call error (0, "CCD has too many read-outs for this program")

	# Get decode and order list of active amplifiers.
	call hdmgstr (im, "amplist", Memc[hdrvalue], SZ_LINE)
	call ampnames (qg, Memc[hdrvalue])

	# Read geometry keywords for each amplifier from header.
	do amp = 1, QG_NAMPS (qg) {

	    # Ampsec (ASECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_KEYWRD, "ASEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])
	    call hdmgstr (im, Memc[keyword], Memc[section], SZ_LINE)

	    ax1 = 1
	    ax2 = QG_NX(qg, 0) / QG_NAMPSX(qg)
	    axs = 1
	    ay1 = 1
	    ay2 = QG_NY(qg, 0) / QG_NAMPSY(qg)
	    ays = 1

	    call ccd_section (Memc[section], ax1, ax2, axs, ay1, ay2, ays)
	    QG_AX1(qg, amp) = ax1
	    QG_AX2(qg, amp) = ax2
	    QG_AY1(qg, amp) = ay1
	    QG_AY2(qg, amp) = ay2
	    
	    # Set X and Y dimensions of subimage read out by each amplifier
	    QG_NX(qg, amp) = ax2 - ax1 + 1
	    QG_NY(qg, amp) = ay2 - ay1 + 1

	    # Datasec (DSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_KEYWRD, "DSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])
	    call hdmgstr (im, Memc[keyword], Memc[section], SZ_LINE)

	    dx1 = ax1
	    dx2 = ax2
	    dxs = 1
	    dy1 = ay1
	    dy2 = ay2
	    dys = 1
	    call ccd_section (Memc[section], dx1, dx2, dxs, dy1, dy2, dys)
	    QG_DX1(qg, amp) = dx1 - ax1 + 1
	    QG_DX2(qg, amp) = dx2 - ax1 + 1
	    QG_DY1(qg, amp) = dy1 - ay1 + 1
	    QG_DY2(qg, amp) = dy2 - ay1 + 1

	    # CCDsec (CSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_KEYWRD, "CSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])
	    call hdmgstr (im, Memc[keyword], Memc[section], SZ_LINE)

	    cx1 = dx1
	    cx2 = dx2
	    cxs = 1
	    cy1 = dy1
	    cy2 = dy2
	    cys = 1
	    call ccd_section (Memc[section], cx1, cx2, cxs, cy1, cy2, cys)
	    QG_CX1(qg, amp) = cx1
	    QG_CX2(qg, amp) = cx2
	    QG_CY1(qg, amp) = cy1
	    QG_CY2(qg, amp) = cy2

	    # Trimsec (TSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_KEYWRD, "TSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if (hdmaccf (im, Memc[keyword]) == YES) {
		call hdmgstr (im, Memc[keyword], Memc[section], SZ_LINE)

		tx1 = dx1
		tx2 = dx2
		txs = 1
		ty1 = dy1
		ty2 = dy2
		tys = 1
		call ccd_section (Memc[section], tx1, tx2, txs, ty1, ty2, tys)
		QG_TX1(qg, amp) = tx1 - ax1 + 1
		QG_TX2(qg, amp) = tx2 - ax1 + 1
		QG_TY1(qg, amp) = ty1 - ay1 + 1
		QG_TY2(qg, amp) = ty2 - ay1 + 1

		QG_PHANTOM(qg, amp) = NO

	    } else {
		QG_TX1(qg, amp) = 0
		QG_TX2(qg, amp) = 0
		QG_TY1(qg, amp) = 0
		QG_TY2(qg, amp) = 0

		# If the image has not been reduced this must be a phantom
		if (hdmaccf (im, "trim") == NO) {
		    QG_PHANTOM(qg, amp) = YES
		} else {
		    QG_PHANTOM(qg, amp) = NO
		}
	    }

	    # Biassec (BSECyx keyword)
	    #
	    call sprintf (Memc[keyword], SZ_KEYWRD, "BSEC%2s")
		call pargstr (Memc[QG_AMPID(qg, amp)])

	    if (hdmaccf (im, Memc[keyword]) == YES) {
		call hdmgstr (im, Memc[keyword], Memc[section], SZ_LINE)

		bx1 = 0
		bx2 = 0
		bxs = 1
		by1 = 0
		by2 = 0
		bys = 1
		call ccd_section (Memc[section], bx1, bx2, bxs, by1, by2, bys)
		QG_BX1(qg, amp) = bx1 - ax1 + 1
		QG_BX2(qg, amp) = bx2 - ax1 + 1
		QG_BY1(qg, amp) = by1 - ay1 + 1
		QG_BY2(qg, amp) = by2 - ay1 + 1
	    } else {
		QG_BX1(qg, amp) = 0
		QG_BX2(qg, amp) = 0
		QG_BY1(qg, amp) = 0
		QG_BY2(qg, amp) = 0
	    }
	}

	call sfree (sp)
end


procedure ampnames (qg, amplist)

pointer	qg			#I/O Pointer to open quadgeom structure
char	amplist[ARB]		#I List of active amplifiers

int	amp, nch
pointer	sp, ampnum

int	strdic(), itoc()

begin
	call smark (sp)
	call salloc (ampnum, QG_NAMPS (qg), TY_INT)

	# parse amplist into array of ordinal numbers
	call sscan (amplist)
	do amp = 1, QG_NAMPS (qg) {
	    call gargi (Memi[ampnum+amp-1])
	}

	# Sort ordinal numbers into increasing order
	call asrti (Memi[ampnum], Memi[ampnum], QG_NAMPS(qg))

	# Convert ordinal numbers back into id strings
	do amp = 1, QG_NAMPS (qg) {
	    call malloc (QG_AMPID(qg, amp), SZ_AMPID, TY_CHAR)
	    nch = itoc (Memi[ampnum+amp-1], Memc[QG_AMPID(qg, amp)], SZ_AMPID)
	}

	# Set AMPTYPE codes
	do amp = 1, QG_NAMPS (qg) {
	    QG_AMPTYPE (qg, amp) = strdic (Memc[QG_AMPID (qg, amp)], 
	    Memc[QG_AMPID (qg, amp)], SZ_AMPID, AMPDICT)
	}

	call sfree (sp)

end
