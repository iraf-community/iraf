include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_FRINGE -- Set parameters for fringe correction.
#
#   1.  Return immediately if the fringe correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the fringe image and return error if the mkfringe flag is missing.
#   3.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_fringe (ccd)

pointer	ccd			# CCD structure

int	nc, nl, c1, c2, cs, l1, l2, ls, data_c1, ccd_c1, data_l1, ccd_l1
real	exptime1, exptime2, fringescale
pointer	sp, str, image, im

bool	clgetb(), ccdflag()
real	hdmgetr()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, ccdproc, hdmgetr

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("fringecor") || ccdflag (IN_IM(ccd), "fringcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the fringe correction image.
	call cal_image (IN_IM(ccd), FRINGE, 1, Memc[image], SZ_FNAME)

	# If no processing is desired print fringe image name and return.
	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Fringe correction image is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Return an error if the fringe flag is missing.
	im = ccd_cache (Memc[image], FRINGE)
	if (!ccdflag (im, "mkfringe"))
	    call error (0, "MKFRINGE flag missing from fringe image.")

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

	FRINGE_IM(ccd) = im
	FRINGE_C1(ccd) = CCD_C1(ccd) - ccd_c1 + data_c1
	FRINGE_C2(ccd) = CCD_C2(ccd) - ccd_c1 + data_c1
	FRINGE_L1(ccd) = CCD_L1(ccd) - ccd_l1 + data_l1
	FRINGE_L2(ccd) = CCD_L2(ccd) - ccd_l1 + data_l1

	# Get the scaling factors.  If no fringe scale factor assume 1.
	exptime1 = hdmgetr (IN_IM(ccd), "exptime")
	exptime2 = hdmgetr (im, "exptime")
	iferr (fringescale = hdmgetr (im, "fringscl"))
	    fringescale = 1.

	FRINGESCALE(ccd) = exptime1 / exptime2 * fringescale
	CORS(ccd, FRINGECOR) = Q
	COR(ccd) = YES

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE,
	    "Fringe image is %s with scale=%g")
	    call pargstr (Memc[image])
	    call pargr (FRINGESCALE(ccd))
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "fringcor", Memc[str])

	call sfree (sp)
end
