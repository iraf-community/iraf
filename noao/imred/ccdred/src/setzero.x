# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_ZERO -- Set parameters for zero level correction.
#   1.  Return immediately if the zero level correction is not requested or
#	if the image has been previously corrected.
#   2.	Get the zero level correction image.  Return an error if not found.
#   3.	If the zero level image has not been processed call ZEROPROC.
#   4.	Set the processing flag.
#   5.  Log the operation (to user, logfile, and output image header).

procedure set_zero (ccd)

pointer	ccd			# CCD structure

int	nscan, nc, nl, c1, c2, cs, l1, l2, ls, data_c1, ccd_c1, data_l1, ccd_l1
pointer	sp, str, image, im, ccd_cache()
bool	clgetb(), ccdflag(), ccdcheck()
int	ccdtypei(), ccdnscan()
errchk	cal_image, ccd_cache, ccdproc

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("zerocor") || ccdflag (IN_IM(ccd), "zerocor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the zero level correction image.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), ccdtypei(IN_IM(ccd)))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), ZERO, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print zero correction image and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Zero level correction image is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the zero image if necessary.
	# If nscan > 1 then the zero may not yet exist so create it
	# from the unscanned zero.

	iferr (im = ccd_cache (Memc[image], ZERO)) {
	    call cal_image (IN_IM(ccd), ZERO, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], ZERO)
	    if (ccdcheck (im, ZERO)) {
		call ccd_flush (im)
		call ccdproc (Memc[str], ZERO)
	    }
	    call scancor (Memc[str], Memc[image], nscan, INDEF)
	    im = ccd_cache (Memc[image], ZERO)
	}

	if (ccdcheck (im, ZERO)) {
	    call ccd_flush (im)
	    call ccdproc (Memc[image], ZERO)
	    im = ccd_cache (Memc[image], ZERO)
	}

	# Set the processing parameters in the CCD structure.
	nc = IM_LEN(im,1)
	nl = IM_LEN(im,2)
	c1 = 1
	c2 = nc
	l1 = 1
	l2 = nl
	cs = 1
	ls = 1
	call hdmgstr (im, "datasec", Memc[str], SZ_FNAME)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if ((c1<1)||(c2>nc)||(l1<1)||(l2>nl)||(cs!=1)||(ls!=1)) {
	    call sprintf (Memc[str], SZ_LINE,
		"Data section error: image=%s[%d,%d], datasec=[%d:%d,%d:%d]")
		call pargstr (Memc[image])
		call pargi (nc)
		call pargi (nl)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (0, Memc[str])
	}
	data_c1 = c1
	data_l1 = l1
	call hdmgstr (im, "ccdsec", Memc[str], SZ_FNAME)
	call ccd_section (Memc[str], c1, c2, cs, l1, l2, ls)
	if (nc == 1) {
	    c1 = CCD_C1(ccd)
	    c2 = CCD_C2(ccd)
	}
	if (nl == 1) {
	    l1 = CCD_L1(ccd)
	    l2 = CCD_L2(ccd)
	}
	ccd_c1 = c1
	ccd_l1 = l1
	if ((c1 > CCD_C1(ccd)) || (c2 < CCD_C2(ccd)) ||
	    (l1 > CCD_L1(ccd)) || (l2 < CCD_L2(ccd))) {
	    call sprintf (Memc[str], SZ_LINE,
	        "CCD section error: input=[%d:%d,%d:%d], %s=[%d:%d,%d:%d]")
		call pargi (CCD_C1(ccd))
		call pargi (CCD_C2(ccd))
		call pargi (CCD_L1(ccd))
		call pargi (CCD_L2(ccd))
		call pargstr (Memc[image])
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (0, Memc[str])
	}

	ZERO_IM(ccd) = im
	ZERO_C1(ccd) = CCD_C1(ccd) - ccd_c1 + data_c1
	ZERO_C2(ccd) = CCD_C2(ccd) - ccd_c1 + data_c1
	ZERO_L1(ccd) = CCD_L1(ccd) - ccd_l1 + data_l1
	ZERO_L2(ccd) = CCD_L2(ccd) - ccd_l1 + data_l1

	CORS(ccd, ZEROCOR) = Z
	COR(ccd) = YES

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE, "Zero level correction image is %s")
	    call pargstr (Memc[image])
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "zerocor", Memc[str])

	call sfree (sp)
end
