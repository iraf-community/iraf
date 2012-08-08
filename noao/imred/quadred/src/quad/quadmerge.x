include <imhdr.h>
include "quadgeom.h"

procedure quadmerge (qg)

pointer	qg		#I Pointer to quadgeom structure.

int	nx, ny, xdata, ydata, txskip1, txskip2, tyskip1, tyskip2
int	bxskip1, bxskip2, byskip1, byskip2
int	x, y, amp, pre, namps, nampsx, nampsy

begin
	namps  = QG_NAMPS(qg)
	nampsx = QG_NAMPSX(qg)
	nampsy = QG_NAMPSY(qg)

	# Check consistancy of number of active readouts.
	if (namps == 0)
	    call error (0, "No input images")
	if (namps != nampsx * nampsy)
	    call error (0, "Incomplete or inconsistant set of sub-images")

	# Determine dimensions of the composite image.
	# We just sum the dimensions of the first row and column of sub-images
	# We should realy check that the sub-images do form a regular grid.
	nx = 0
	do x = 1, nampsx {
	    nx = nx + QG_NX(qg, QG_AMP(qg, x, 1))
	}
	ny = 0
	do y = 1, nampsy {
	    ny = ny + QG_NY(qg, QG_AMP(qg, 1, y))
	}
	QG_NX(qg, 0) = nx 
	QG_NY(qg, 0) = ny 

	# Calculate datasec, trimsec, and biassec, ccdsec for composite image.
	# The required sections are those for the equivalent mono-readout image.
	# If datasec is uninitialised assume all these sections are absent as 
	# will be the case for processed [OT] images.
	if (QG_DX1 (qg, 1) != 0) {
	    # Calculate number of data pixels.
	    xdata = 0
	    do x = 1, nampsx {
		amp   = QG_AMP(qg, x, 1)
		xdata = xdata + QG_DX2(qg, amp) - QG_DX1(qg, amp) + 1
	    }
	    ydata = 0
	    do y = 1, nampsy {
		amp   = QG_AMP(qg, 1, y)
		ydata = ydata + QG_DY2(qg, amp) - QG_DY1(qg, amp) + 1
	    }
	    txskip1 = QG_TX1(qg, 1)     - QG_DX1(qg, 1)
	    txskip2 = QG_DX2(qg, namps) - QG_TX2(qg, namps)
	    tyskip1 = QG_TY1(qg, 1)     - QG_DY1(qg, 1)
	    tyskip2 = QG_DY2(qg, namps) - QG_TY2(qg, namps)

	    # Calculate width of bias strip margins.
	    switch (QG_AMPTYPE(qg, 1)) {
	    case AMP11, AMP21:		# "Left amp"
		bxskip1 = QG_BX1(qg, 1) - QG_DX2(qg, 1) - 1
		bxskip2 = QG_NX(qg, 1)  - QG_BX2(qg, 1)

	    case AMP12, AMP22:		# "Right amp"
		bxskip1 = QG_DX1(qg, 1) - QG_BX2(qg, 1) - 1
		bxskip2 = QG_BX1(qg, 1) - 1
	    }

	    byskip1 = QG_BY1(qg, 1)    - 1
	    byskip2 = QG_NY(qg, namps) - QG_BY2(qg, namps)

	    QG_DX1(qg, 0) = QG_DX1(qg, 1)
	    QG_DX2(qg, 0) = QG_DX1(qg, 0) + xdata - 1
	    QG_DY1(qg, 0) = QG_DY1(qg, 1)
	    QG_DY2(qg, 0) = QG_DY1(qg, 0) + ydata - 1

	    QG_TX1(qg, 0) = QG_DX1(qg, 0) + txskip1
	    QG_TX2(qg, 0) = QG_DX2(qg, 0) - txskip2
	    QG_TY1(qg, 0) = QG_DY1(qg, 0) + tyskip1
	    QG_TY2(qg, 0) = QG_DY2(qg, 0) - tyskip2

	    QG_BX1(qg, 0) = QG_DX2(qg, 0) + bxskip1 + 1
	    QG_BX2(qg, 0) = nx - bxskip2
	    QG_BY1(qg, 0) = 1  + byskip1
	    QG_BY2(qg, 0) = ny - byskip2
	}

	# Calculate ccdsec for composite image using sub-images in BLH and TRH
	# corners.
	if (QG_CX1 (qg, 1) != 0) {
	    QG_CX1(qg, 0) = QG_CX1(qg, 1)
	    QG_CX2(qg, 0) = QG_CX2(qg, nampsx)
	    QG_CY1(qg, 0) = QG_CY1(qg, 1)
	    QG_CY2(qg, 0) = QG_CY2(qg, namps)
	}

	# Set up "ampsec" - the section of the composite image derived from
	# each sub-image.
	do y = 1, nampsy {
	    amp = QG_AMP (qg, 1, y)
	    QG_AX1(qg, amp) = 1
	    QG_AX2(qg, amp) = QG_NX(qg, amp)
	    do x = 2, nampsx {
		amp = QG_AMP (qg, x,   y)
		pre = QG_AMP (qg, x-1, y)
		QG_AX1(qg, amp) = QG_AX2(qg, pre) + 1
		QG_AX2(qg, amp) = QG_AX1(qg, amp) + QG_NX(qg, amp) - 1
	    }
	}
	do x = 1, nampsx {
	    amp = QG_AMP (qg, x, 1)
	    QG_AY1(qg, amp) = 1
	    QG_AY2(qg, amp) = QG_NY(qg, amp)
	    do y = 2, nampsy {
		amp = QG_AMP (qg, x, y)
		pre = QG_AMP (qg, x, y-1)
		QG_AY1(qg, amp) = QG_AY2(qg, pre) + 1
		QG_AY2(qg, amp) = QG_AY1(qg, amp) + QG_NY(qg, amp) - 1
	    }
	}

end
