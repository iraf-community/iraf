include	<imhdr.h>
include	<pkg/gtools.h>
include	"../idsmtn.h"

# MKTITLE -- Make a spectrum title (IIDS style)

procedure mktitle (image, nline, im, ids, gt)

char	image[ARB]
int	nline
pointer	im, ids, gt

int	beamnr
pointer	sp, str1, str2
errchk	imgstr()

begin
	# Do nothing if no GTOOLS pointer is defined.
	if (gt == NULL)
	    return

	# Copy image file name
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	if (nline > 0) {
	    call sprintf (Memc[str1], SZ_LINE, "[%s[*,%d]]: ")
	        call pargstr (image)
		call pargi (nline)
	} else {
	    call sprintf (Memc[str1], SZ_LINE, "[%s]: ")
	        call pargstr (image)
	}

	# Copy image title
	call strcpy (IM_TITLE(im), Memc[str2], SZ_LINE)
	call strcat (Memc[str2], Memc[str1], SZ_LINE)

	# Load exposure time and aperture number
	if (BEAM(ids) >= 0 && BEAM(ids) < 100)
	    beamnr = BEAM(ids)
	else
	    beamnr = 0
	call sprintf (Memc[str2], SZ_LINE, " %7.2fs ap:%1d")
	    call pargr (ITM(ids))
	    call pargi (beamnr)
	call strcat (Memc[str2], Memc[str1], SZ_LINE)

	# Set GTOOLS labels.
	call gt_sets (gt, GTTITLE, Memc[str1])
	iferr {
    	    call imgstr (im, "CTYPE1", Memc[str1], SZ_LINE)
   	    call gt_sets (gt, GTXLABEL, Memc[str1])
    	    call imgstr (im, "CUNIT1", Memc[str1], SZ_LINE)
    	    call gt_sets (gt, GTXUNITS, Memc[str1])
	} then
	    ;
	call sfree (sp)
end
