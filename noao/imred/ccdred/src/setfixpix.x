include	<imhdr.h>
include	<imset.h>
include	<pmset.h>
include	"ccdred.h"


# SET_FIXPIX -- Set parameters for bad pixel correction.
#   1.  Return immediately if the bad pixel correction is not requested or
#	if the image has been previously corrected.
#   2.	Get the bad pixel mask.  Return an error if not found.
#   3.	If the bad pixel mask has not been processed call PROC.
#   4.	Set the processing flag.
#   5.  Log the operation (to user, logfile, and output image header).
#
# This routine relies on the physical coordinate system and assumes
# XT_PMMAP has taken care of matching the pixel mask to the input image.

procedure set_fixpix (ccd)

pointer	ccd			# CCD structure

pointer	sp, image, str, im

int	imstati()
bool	clgetb(), streq(), ccdflag()
pointer	xt_pmmap(), xt_fpinit()
errchk	xt_pmmap(), xt_fpinit()

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("fixpix") || ccdflag (IN_IM(ccd), "fixpix"))
	    return

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the bad pixel file.  If the name is "image" then get the file
	# name from the image header or symbol table.

	call clgstr ("fixfile", Memc[image], SZ_FNAME)
	if (streq (Memc[image], "image"))
	    call hdmgstr (IN_IM(ccd), "fixfile", Memc[image], SZ_FNAME)

	# If no processing is desired print message and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Bad pixel file is %s\n")
		call pargstr (Memc[image])
	    call sfree (sp)
	    return
	}

	# Map the bad pixel image and return on an error.
	im = xt_pmmap (Memc[image], IN_IM(ccd), Memc[image], SZ_FNAME)
	if (Memc[image] == EOS)
	    call error (1, "No bad pixel mask found")
	if (im != NULL)  {
	    MASK_IM(ccd) = im
	    MASK_PM(ccd) = imstati (im, IM_PMDES)
	    MASK_FP(ccd) = xt_fpinit (MASK_PM(ccd), 2, 3)

	    CORS(ccd, FIXPIX) = YES
	    COR(ccd) = YES
	}

	# Log the operation.
	call sprintf (Memc[str], SZ_LINE, "Bad pixel file is %s")
	    call pargstr (Memc[image])
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "fixpix", Memc[str])

	call sfree (sp)
end
