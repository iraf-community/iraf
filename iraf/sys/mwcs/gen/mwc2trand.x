# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_C2TRAN -- Optimized 2D coordinate transformation.

procedure mw_c2trand (a_ct, x1,y1, x2,y2)

pointer	a_ct			#I pointer to CTRAN descriptor
double	x1,y1			#I coordinates in input system
double	x2,y2			#O coordinates in output system

pointer	ct, ltm, ltv
double	p1[2], p2[2]

begin
	# Get real or double version of descriptor.
	ct = CT_D(a_ct)

	ltm = CT_LTM(ct)
	ltv = CT_LTV(ct)

	if (CT_TYPE(ct) == LNR) {
	    # Simple linear, nonrotated transformation.
	    x2 = Memd[ltm  ] * x1 + Memd[ltv  ]
	    y2 = Memd[ltm+3] * y1 + Memd[ltv+1]
	} else if (CT_TYPE(ct) == LRO) {
	    # Linear, rotated transformation.
	    p1[1] = x1;  p1[2] = y1
	    x2 = Memd[ltm  ] * p1[1] + Memd[ltm+1] * p1[2] + Memd[ltv  ]
	    y2 = Memd[ltm+2] * p1[1] + Memd[ltm+3] * p1[2] + Memd[ltv+1]
	} else {
	    # General case involving one or more functional terms.
	    p1[1] = x1;  p1[2] = y1
	    call mw_ctrand (a_ct, p1, p2, 2)
	    x2 = p2[1];  y2 = p2[2]
	}
end
