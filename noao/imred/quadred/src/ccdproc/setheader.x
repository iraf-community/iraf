include	<imhdr.h>
include	"ccdred.h"

# SET_HEADER -- Set the output image header.

procedure set_header (ccd)

pointer	ccd			# CCD structure

int	nc, nl
pointer	sp, str, out
long	clktime()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	out = OUT_IM(ccd)
	nc = IM_LEN(out,1)
	nl = IM_LEN(out,2)

	# Set the data section if it is not the whole image.
	if ((OUT_C1(ccd) != 1) || (OUT_C2(ccd) != nc) ||
	    (OUT_L1(ccd) != 1) || (OUT_L2(ccd) != nl)) {
	    call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
	        call pargi (OUT_C1(ccd))
	        call pargi (OUT_C2(ccd))
	        call pargi (OUT_L1(ccd))
	        call pargi (OUT_L2(ccd))
	    call hdmpstr (out, "datasec", Memc[str])
	} else {
	    iferr (call hdmdelf (out, "datasec"))
		;
	}

	# Set the CCD section.
	call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
	    call pargi (CCD_C1(ccd))
	    call pargi (CCD_C2(ccd))
	    call pargi (CCD_L1(ccd))
	    call pargi (CCD_L2(ccd))
	call hdmpstr (out, "ccdsec", Memc[str])

	# If trimming update the trim and bias section parameters.
	if (CORS(ccd, TRIM) == YES) {
	    iferr (call hdmdelf (out, "trimsec"))
	        ;
	    iferr (call hdmdelf (out, "biassec"))
	        ;
	    BIAS_C1(ccd) = max (1, BIAS_C1(ccd) - TRIM_C1(ccd) + 1)
	    BIAS_C2(ccd) = min (nc, BIAS_C2(ccd) - TRIM_C1(ccd) + 1)
	    BIAS_L1(ccd) = max (1, BIAS_L1(ccd) - TRIM_L1(ccd) + 1)
	    BIAS_L2(ccd) = min (nl, BIAS_L2(ccd) - TRIM_L1(ccd) + 1)
	    if ((BIAS_C1(ccd)<=BIAS_C2(ccd)) && (BIAS_L1(ccd)<=BIAS_L2(ccd))) {
		call sprintf (Memc[str], SZ_LINE, "[%d:%d,%d:%d]")
		    call pargi (BIAS_C1(ccd))
		    call pargi (BIAS_C2(ccd))
		    call pargi (BIAS_L1(ccd))
		    call pargi (BIAS_L2(ccd))
		call hdmpstr (out, "biassec", Memc[str])
	    }
	}

	# Set mean value if desired.
	if (CORS(ccd, FINDMEAN) == YES) {
	    call hdmputr (out, "ccdmean", MEAN(ccd))
	    call hdmputi (out, "ccdmeant", int (clktime (long (0))))
	}

	# Mark image as processed.
	call sprintf (Memc[str], SZ_LINE, "CCD processing done")
	call timelog (Memc[str], SZ_LINE)
	call hdmpstr (out, "ccdproc", Memc[str])

	call sfree (sp)
end
