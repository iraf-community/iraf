# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"../mwcs.h"

# MW_V2TRAN -- Optimized 2D coordinate transformation for an array of points.

procedure mw_v2trand (a_ct, x1,y1, x2,y2, npts)

pointer	a_ct			#I pointer to CTRAN descriptor
double	x1[ARB],y1[ARB]		#I coordinates in input system
double	x2[ARB],y2[ARB]		#O coordinates in output system
int	npts

int	i
pointer	ct, ltm, ltv
double	p1[2], p2[2]
errchk	mw_ctrand

begin
	# Get real or double version of descriptor.
	ct = CT_D(a_ct)

	ltm = CT_LTM(ct)
	ltv = CT_LTV(ct)

	if (CT_TYPE(ct) == LNR) {
	    # Simple linear, nonrotated transformation.
	    do i = 1, npts {
		x2[i] = Memd[ltm  ] * x1[i] + Memd[ltv  ]
		y2[i] = Memd[ltm+3] * y1[i] + Memd[ltv+1]
	    }
	} else if (CT_TYPE(ct) == LRO) {
	    # Linear, rotated transformation.
	    do i = 1, npts {
		p1[1] = x1[i];  p1[2] = y1[i]
		x2[i] = Memd[ltm  ] * p1[1] + Memd[ltm+1] * p2[1] +
		    Memd[ltv  ]
		y2[i] = Memd[ltm+2] * p1[1] + Memd[ltm+3] * p2[1] +
		    Memd[ltv+1]
	    }
	} else {
	    # General case involving one or more functional terms.
	    do i = 1, npts {
		p1[1] = x1[i];  p1[2] = y1[i]
		call mw_ctrand (a_ct, p1, p2, 2)
		x2[i] = p2[1];  y2[i] = p2[2]
	    }
	}
end
