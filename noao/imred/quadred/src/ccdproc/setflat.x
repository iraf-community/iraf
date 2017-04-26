include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_FLAT -- Set parameters for flat field correction.
#
#   1.  Return immediately if the flat field correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the flat field image and return on an error.
#   3.  If the flat field image has not been processed call PROC.
#   4.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_flat (ccd)

pointer	ccd			# CCD structure

int	nc, nl, c1, c2, cs, l1, l2, ls, data_c1, ccd_c1, data_l1, ccd_l1
pointer	sp, str, image, im, ccd_cache()
bool	clgetb(), ccdflag(), ccdcheck()
int	nscan, ccdnscan(), ccdtypei()
real	hdmgetr()
errchk	cal_image, ccd_cache, ccdproc, hdmgetr

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("flatcor") || ccdflag (IN_IM(ccd), "flatcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the flat field correction image.
	if (clgetb ("scancor"))
	    nscan = ccdnscan (IN_IM(ccd), ccdtypei(IN_IM(ccd)))
	else
	    nscan = 1
	call cal_image (IN_IM(ccd), FLAT, nscan, Memc[image], SZ_FNAME)

	# If no processing is desired print flat field image name and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Flat correction image is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the image and return on an error.
	# Process the flat field image if necessary.
	# If nscan > 1 then the flat field may not yet exist so create it
	# from the unscanned flat field.

	iferr (im = ccd_cache (Memc[image], FLAT)) {
	    call cal_image (IN_IM(ccd), FLAT, 1, Memc[str], SZ_LINE)
	    im = ccd_cache (Memc[str], FLAT)
	    if (ccdcheck (im, FLAT)) {
		call ccd_flush (im)
		call ccdproc (Memc[str], FLAT)
	    }
	    call scancor (Memc[str], Memc[image], nscan, MINREPLACE(ccd))
	    im = ccd_cache (Memc[image], FLAT)
	}

	if (ccdcheck (im, FLAT)) {
	    call ccd_flush (im)
	    call ccdproc (Memc[image], FLAT)
	    im = ccd_cache (Memc[image], FLAT)
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

	FLAT_IM(ccd) = im
	FLAT_C1(ccd) = CCD_C1(ccd) - ccd_c1 + data_c1
	FLAT_C2(ccd) = CCD_C2(ccd) - ccd_c1 + data_c1
	FLAT_L1(ccd) = CCD_L1(ccd) - ccd_l1 + data_l1
	FLAT_L2(ccd) = CCD_L2(ccd) - ccd_l1 + data_l1

	# If no mean value use 1 as the scale factor.
	iferr (FLATSCALE(ccd) = hdmgetr (im, "ccdmean"))
	    FLATSCALE(ccd) = 1.
	CORS(ccd, FLATCOR) = F
	COR(ccd) = YES

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE,
	    "Flat field image is %s with scale=%g")
	    call pargstr (Memc[image])
	    call pargr (FLATSCALE(ccd))
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "flatcor", Memc[str])

	call sfree (sp)
end
