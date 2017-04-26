include	<imhdr.h>
include	"ccdred.h"
include	"ccdtypes.h"

# SET_ILLUM -- Set parameters for illumination correction.
#
#   1.  Return immediately if the illumination correction is not requested or
#	if the image has been previously corrected.
#   2.  Get the illumination image and return error if mkillum flag missing.
#   3.  Set the processing flags and record the operation in the output
#   	image and write a log record.

procedure set_illum (ccd)

pointer	ccd			# CCD structure

int	nc, nl, c1, c2, cs, l1, l2, ls, data_c1, ccd_c1, data_l1, ccd_l1
long	time
pointer	sp, str, image, im

bool	clgetb(), ccdflag()
long	hdmgeti()
real	hdmgetr()
pointer	ccd_cache()
errchk	cal_image, ccd_cache, ccdproc, hdmgetr, hdmgeti

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("illumcor") || ccdflag (IN_IM(ccd), "illumcor"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the illumcor correction image.
	call cal_image (IN_IM(ccd), ILLUM, 1, Memc[image], SZ_FNAME)

	# If no processing is desired print illumination image name and return.
	if (clgetb ("noproc")) {
	    call eprintf (
		"  [TO BE DONE] Illumination correction image is %s.\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Return a warning if the illumination flag is missing.
	im = ccd_cache (Memc[image], ILLUM)
	if (!ccdflag (im, "mkillum")) {
	    call ccd_flush (im)
	    call error (0, "MKILLUM flag missing from illumination image")
	}

	# If no mean value for the scale factor compute it.
	iferr (ILLUMSCALE(ccd) = hdmgetr (im, "ccdmean"))
	    ILLUMSCALE(ccd) = INDEF
	iferr (time = hdmgeti (im, "ccdmeant"))
	    time = IM_MTIME(im)
	if (IS_INDEF(ILLUMSCALE(ccd)) || time < IM_MTIME(im)) {
	    call ccd_flush (im)
	    call ccdmean (Memc[image])
	    im = ccd_cache (Memc[image], ILLUM)
	}
	iferr (ILLUMSCALE(ccd) = hdmgetr (im, "ccdmean"))
	    ILLUMSCALE(ccd) = 1.
 
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

	ILLUM_IM(ccd) = im
	ILLUM_C1(ccd) = CCD_C1(ccd) - ccd_c1 + data_c1
	ILLUM_C2(ccd) = CCD_C2(ccd) - ccd_c1 + data_c1
	ILLUM_L1(ccd) = CCD_L1(ccd) - ccd_l1 + data_l1
	ILLUM_L2(ccd) = CCD_L2(ccd) - ccd_l1 + data_l1

	CORS(ccd, ILLUMCOR) = I
	COR(ccd) = YES

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE,
	    "Illumination image is %s with scale=%g")
	    call pargstr (Memc[image])
	    call pargr (ILLUMSCALE(ccd))
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "illumcor", Memc[str])

	call sfree (sp)
end
