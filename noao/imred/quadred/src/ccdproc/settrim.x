include	<imhdr.h>
include	<imset.h>
include	"ccdred.h"

# SET_TRIM -- Set the trim parameters.
#
#   1.  Return immediately if the trim correction is not requested or
#	if the image has been previously corrected.
#   2.	Determine the trim section.  This may be specifed directly or
#	indirectly through the image header or symbol table.
#   3.  Parse the trim section and apply it to the output image.
#   4.  If the image is trimmed then log the operation and reset the output
#	image size.

procedure set_trim (ccd)

pointer	ccd			# CCD structure

int	xt1, xt2, yt1, yt2
int	nc, nl, c1, c2, l1, l2
pointer	sp, str, image
bool	clgetb(), ccdflag()
define	log_	10

begin
	# Check if the user wants this operation or it has been done.
	if (!clgetb ("trim") || ccdflag (IN_IM(ccd), "trim"))
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (IN_SEC(ccd) != NULL)
	    goto log_

	# Check trim section.
	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)
	c1 = TRIM_C1(ccd)
	c2 = TRIM_C2(ccd)
	l1 = TRIM_L1(ccd)
	l2 = TRIM_L2(ccd)
	if ((c1 < 1) || (c2 > nc) || (l1 < 1) || (l2 > nl)) {
	    call salloc (image, SZ_LINE, TY_CHAR)
	    call imstats (IN_IM(ccd), IM_IMAGENAME, Memc[image], SZ_FNAME)
	    call sprintf (Memc[str], SZ_LINE,
		"Error in trim section: image=%s[%d,%d], trimsec=[%d:%d,%d:%d]")
		call pargstr (Memc[image])
		call pargi (nc)
		call pargi (nl)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (0, Memc[str])
	}

	# If no processing is desired print trim section and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Trim section is [%d:%d,%d:%d].\n")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call sfree (sp)
	    return
	}

	xt1 = max (0, c1 - IN_C1(ccd))
	xt2 = min (0, c2 - IN_C2(ccd))
	yt1 = max (0, l1 - IN_L1(ccd))
	yt2 = min (0, l2 - IN_L2(ccd))

	CCD_C1(ccd) = CCD_C1(ccd) + xt1
	CCD_C2(ccd) = CCD_C2(ccd) + xt2
	CCD_L1(ccd) = CCD_L1(ccd) + yt1
	CCD_L2(ccd) = CCD_L2(ccd) + yt2
	IN_C1(ccd) = IN_C1(ccd) + xt1
	IN_C2(ccd) = IN_C2(ccd) + xt2
	IN_L1(ccd) = IN_L1(ccd) + yt1
	IN_L2(ccd) = IN_L2(ccd) + yt2
	OUT_C1(ccd) = IN_C1(ccd) - c1 + 1
	OUT_C2(ccd) = IN_C2(ccd) - c1 + 1
	OUT_L1(ccd) = IN_L1(ccd) - l1 + 1
	OUT_L2(ccd) = IN_L2(ccd) - l1 + 1
	IM_LEN(OUT_IM(ccd),1) = c2 - c1 + 1
	IM_LEN(OUT_IM(ccd),2) = l2 - l1 + 1

log_
	if (IN_SEC(ccd) == NULL) {
	    CORS(ccd, TRIM) = YES
	    COR(ccd) = YES

	    call sprintf (Memc[str], SZ_LINE,
		"Trim data section is [%d:%d,%d:%d]")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call timelog (Memc[str], SZ_LINE)
	    call ccdlog (IN_IM(ccd), Memc[str])
	    call hdmpstr (OUT_IM(ccd), "trim", Memc[str])
	} else {
	    CORS(ccd, TRIM) = NO
	    COR(ccd) = YES

	    call sprintf (Memc[str], SZ_LINE,
		"Trim multiple overscan sections")
	    call timelog (Memc[str], SZ_LINE)
	    call ccdlog (IN_IM(ccd), Memc[str])
	    call hdmpstr (OUT_IM(ccd), "trim", Memc[str])
	}

	call sfree (sp)
end
