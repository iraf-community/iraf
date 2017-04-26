# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_C2TRAN -- Optimized 2D coordinate transformation.

procedure mw_c2tranr (a_ct, x1,y1, x2,y2)

pointer	a_ct			#I pointer to CTRAN descriptor
real	x1,y1			#I coordinates in input system
real	x2,y2			#O coordinates in output system

pointer	ct, ltm, ltv
real	p1[2], p2[2]

begin
	# Get real or double version of descriptor.
	ct = CT_R(a_ct)

	ltm = CT_LTM(ct)
	ltv = CT_LTV(ct)

	if (CT_TYPE(ct) == LNR) {
	    # Simple linear, nonrotated transformation.
	    x2 = Memr[ltm  ] * x1 + Memr[ltv  ]
	    y2 = Memr[ltm+3] * y1 + Memr[ltv+1]
	} else if (CT_TYPE(ct) == LRO) {
	    # Linear, rotated transformation.
	    p1[1] = x1;  p1[2] = y1
	    x2 = Memr[ltm  ] * p1[1] + Memr[ltm+1] * p1[2] + Memr[ltv  ]
	    y2 = Memr[ltm+2] * p1[1] + Memr[ltm+3] * p1[2] + Memr[ltv+1]
	} else {
	    # General case involving one or more functional terms.
	    p1[1] = x1;  p1[2] = y1
	    call mw_ctranr (a_ct, p1, p2, 2)
	    x2 = p2[1];  y2 = p2[2]
	}
end
