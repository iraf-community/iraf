include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"


# SET_DARK -- Set parameters for dark count correction.
#
#   1.  Return immediately if the dark count correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the dark count correction image and return an error if not found.
#   3.  If the dark count image has not been processed call PROC.
#   4.  Compute the dark count integration time scale factor.
#   5.  Set the processing flags.
#   6.  Log the operation (to user, logfile, and output image header).

procedure set_dark (ccd)

pointer	ccd			# CCD structure

int	nscan, nc, nl, c1, c2, cs, l1, l2, ls, data_c1, ccd_c1, data_l1, ccd_l1
real	darktime1, darktime2
pointer	sp, image, str, im

bool	clgetb(), ccdflag(), ccdcheck()
int	ccdnscan(), ccdtypei()
real	hdmgetr()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, ccdproc, hdmgetr

begin
	# Check if the user wants this operation or it has already been done.
	if (!clgetb ("darkcor") || ccdflag (IN_IM(ccd), "darkcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the dark count correction image name.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), ccdtypei(IN_IM(ccd)))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), DARK, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print dark count image and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Dark count correction image is %s.\n")
	        call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the dark count image if necessary.
	# If nscan > 1 then the dark may not yet exist so create it
	# from the unscanned dark.

	iferr (im = ccd_cache (Memc[image], DARK)) {
	    call cal_image (IN_IM(ccd), DARK, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], DARK)
	    if (ccdcheck (im, DARK)) {
		call ccd_flush (im)
		call ccdproc (Memc[str], DARK)
	    }
	    call scancor (Memc[str], Memc[image], nscan, INDEF)
	    im = ccd_cache (Memc[image], DARK)
	}

	if (ccdcheck (im, DARK)) {
	    call ccd_flush (im)
	    call ccdproc (Memc[image], DARK)
	    im = ccd_cache (Memc[image], DARK)
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

	DARK_IM(ccd) = im
	DARK_C1(ccd) = CCD_C1(ccd) - ccd_c1 + data_c1
	DARK_C2(ccd) = CCD_C2(ccd) - ccd_c1 + data_c1
	DARK_L1(ccd) = CCD_L1(ccd) - ccd_l1 + data_l1
	DARK_L2(ccd) = CCD_L2(ccd) - ccd_l1 + data_l1

	# Get the dark count integration times.  Return an error if not found.
	iferr (darktime1 = hdmgetr (IN_IM(ccd), "darktime"))
	    darktime1 = hdmgetr (IN_IM(ccd), "exptime")
	iferr (darktime2 = hdmgetr (im, "darktime"))
	    darktime2 = hdmgetr (im, "exptime")
	if (darktime2 <= 0.) {
	    call sprintf (Memc[str], SZ_LINE, "Dark time is zero for `%s'")
		call pargstr (Memc[image])
	    call error (1, Memc[str])
	}

	DARKSCALE(ccd) = darktime1 / darktime2
	CORS(ccd, DARKCOR) = D
	COR(ccd) = YES

	# Record the operation in the output image and write a log record.
	call sprintf (Memc[str], SZ_LINE,
	    "Dark count correction image is %s with scale=%g")
	    call pargstr (Memc[image])
	    call pargr (DARKSCALE(ccd))
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "darkcor", Memc[str])

	call sfree (sp)
end
