include <imhdr.h>
include "rvpackage.h"
include "rvflags.h"
include "rvkeywords.h"
include "rvmwcs.h"

# GET_DISP_INFO - Get the dispersion information from the image header.  Leave
# it up to the caller to do any necessary rebinning

int procedure get_disp_info (rv, im, apnum, w0, wpc, dcflag)

pointer	rv					#I RV struct pointer
pointer	im					#I Image pointer
int	apnum					#I Requested aperture
real	w0					#O Starting wavelength
real	wpc					#O Wavelength increment
int	dcflag					#O DC header flag

real	a, b
int	i, nspec

begin
	# First check to see if all we're doing is a pixel correlation
	if (RV_PIXCORR(rv) == YES) {
	    w0 = 1.
	    wpc = 1.
	    dcflag = -1
	    return (PIXELS)
	}

        # Get the structure
        call rv_mwcs_open (rv)
        call rv_mwcs (im, RVMW_AP(rv), RVMW_DTYPE(rv), RVMW_W1(rv), RVMW_DW(rv),
            RVMW_NW(rv), nspec)

	do i = 1, nspec {
	    # Handles the case where we have a onedspec image but a beam-num=0
	    # keyword.
	    if ((apnum == 1 && RMW_AP(rv,i) == 0) ||
                (RMW_AP(rv,i) == apnum)) {                        # Found it
		if (RMW_DTYPE(rv,i) == 2) {
		    return (NONLINEAR)
		} else {
		    if (RMW_DTYPE(rv,i) == 1) {
			a = log10 (RMW_W1(rv,i))
                        b = log10 (RMW_W1(rv,i)+(RMW_NW(rv,i)-1)*RMW_DW(rv,i))
                        w0 = a
                        wpc = (b - a) / (RMW_NW(rv,i) - 1)
		    } else {
                        w0 = RMW_W1(rv,i)
                        wpc = RMW_DW(rv,i)
		    }
                    dcflag = RMW_DTYPE(rv,i)
                    break
		}
            }
	}

	# Debug output.
	if (DBG_DEBUG(rv) == YES || RV_APODIZE(rv) == 0.116) {
	    call d_printf (DBG_FD(rv),"get_disp_info:\n")
	    call d_printf (DBG_FD(rv), "\tap=%d dcf=%d w0=%g wpc=%g\n")
	        call pargi (apnum); call pargi(dcflag); 
	        call pargr(w0); call pargr(wpc)
	    do i = 1, nspec {
		call d_printf (DBG_FD(rv), "\t%3d: %d %d %g %g %d\n")
		    call pargi(i); call pargi (RMW_AP(rv,i)); 
		    call pargi(RMW_DTYPE(rv,i)); call pargr(RMW_W1(rv,i)); 
		    call pargr(RMW_DW(rv,i)); call pargi(RMW_NW(rv,i))
	    }
	}

	# Match it and clean up
	call rv_mwcs_close (rv)
	if (dcflag == -1)
	    return (PIXELS)
	else if (dcflag == 0)
	    return (LAMBDA)
	else if (dcflag == 1)
	    return (LOGLAMBDA)
	else
	    return (NONLINEAR)
end
