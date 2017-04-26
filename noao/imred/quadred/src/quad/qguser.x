include "quadgeom.h"

# QGUSER -- modify open quadgeom structure for user specified trim and
# overscan.

procedure qguser (qg, xtrim1, xtrim2, ytrim1, ytrim2, xskip1, xskip2)

pointer	qg		# Pointer to open quadgeom structure.
int	xtrim1		# Number of pixels to trim at right.
int	xtrim2		# Number of pixels to trim at left.
int	ytrim1		# Number of pixels to trim at bottom.
int	ytrim2		# Number of pixels to trim at top.
int	xskip1		# Number of pixels to skip at start of overscan in X.
int	xskip2		# Number of pixels to skip at end   of overscan in X.

int	amp, x, y
int	bx1, bx2, by1, by2

begin

	# Modify overscan margins
	Do amp = 1, QG_NAMPS (qg) {

	    switch (QG_AMPTYPE(qg, amp)) {
	    case AMP11, AMP21:  # Left hand side
		if (IS_INDEFI (xskip1)) {
		    bx1 = QG_BX1(qg, amp)
		} else {
		    bx1 = QG_DX2(qg, amp) + xskip1 + 1
		}

		if (IS_INDEFI (xskip2)) {
		    bx2 = QG_BX2(qg, amp)
		} else {
		    bx2 = QG_AX2(qg, amp) - QG_AX1(qg, amp) - xskip2 + 1
		}

	    case AMP12, AMP22:  # Right hand side
		if (IS_INDEFI (xskip2)) {
		    bx1 = QG_BX1(qg, amp)
		} else {
		    bx1 = 1 + xskip2 
		}
		if (IS_INDEFI (xskip1)) {
		    bx2 = QG_BX2(qg, amp)
		} else {
		    bx2 = QG_DX1(qg, amp) - xskip1 - 1
		}

	    }
	    by1 = QG_BY1(qg, amp)
	    by2 = QG_BY2(qg, amp)

	    if (bx1 > bx2) {
		bx1 = 0
		bx2 = 0
		by1 = 0
		by2 = 0
	    }

	    QG_BX1(qg, amp) = bx1
	    QG_BX2(qg, amp) = bx2
	    QG_BY1(qg, amp) = by1
	    QG_BY2(qg, amp) = by2

	}

	# Modify trim margins

	# Set left hand edge
	if (! IS_INDEFI(xtrim1)) {
	    do y = 1, QG_NAMPSY(qg) {
		do x = 1, QG_NAMPSX(qg) {

		    amp = QG_AMP(qg, x, y)
		    if (QG_PHANTOM(qg, amp) == NO) {
			QG_TX1(qg, amp) = QG_DX1(qg, amp) + xtrim1
			break
		    }
		}
	    }
	}

	# Set right hand edge
	if (! IS_INDEFI(xtrim2)) {
	    do y = 1, QG_NAMPSY(qg) {
		do x = QG_NAMPSX(qg), 1, -1 {

		    amp = QG_AMP(qg, x, y)
		    if (QG_PHANTOM(qg, amp) == NO) {
			QG_TX2(qg, amp) = QG_DX2(qg, amp) - xtrim2
			break
		    }
		}
	    }
	}


	# Set lower edge
	if (! IS_INDEFI(ytrim1)) {
	    do x = 1, QG_NAMPSX(qg) {
		do y = 1, QG_NAMPSY(qg) {

		    amp = QG_AMP(qg, x, y)
		    if (QG_PHANTOM(qg, amp) == NO) {
			QG_TY1(qg, amp) = QG_DY1(qg, amp) + ytrim1
			break
		    }
		}
	    }
	}

	# Set upper edge
	if (! IS_INDEFI(ytrim2)) {
	    do x = 1, QG_NAMPSX(qg) {
		do y = QG_NAMPSY(qg), 1, -1 {

		    amp = QG_AMP(qg, x, y)
		    if (QG_PHANTOM(qg, amp) == NO) {
			QG_TY2(qg, amp) = QG_DY2(qg, amp) - ytrim2
			break
		    }
		}
	    }
	}
end
