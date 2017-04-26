include	"ccdred.h"

# DOPROC -- Call the appropriate processing procedure.
#
# There are four data type paths depending on the readout axis and
# the calculation data type.

procedure doproc (ccd)

pointer	ccd		# CCD processing structure

begin
	switch (READAXIS (ccd)) {
	case 1:
	    switch (CALCTYPE (ccd)) {
	    case TY_SHORT:
	        call proc1s (ccd)
	    default:
	        call proc1r (ccd)
	    }
	case 2:
	    switch (CALCTYPE (ccd)) {
	    case TY_SHORT:
	        call proc2s (ccd)
	    default:
	        call proc2r (ccd)
	    }
	}
end
