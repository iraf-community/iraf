include	<imhdr.h>
include	<pkg/gtools.h>
include	"../shdr.h"
include	"../units.h"

# MKTITLE -- Make a spectrum title (IIDS style)

procedure mktitle (sh, gt)

pointer	sh, gt

pointer	sp, str1, str2

begin
	# Do nothing if no GTOOLS pointer is defined.
	if (gt == NULL)
	    return

	# Copy image file name
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)
	switch (FORMAT(sh)) {
	case TWODSPEC:
	    switch (NDIM(sh)) {
	    case 1:
		call sprintf (Memc[str1], SZ_LINE, "[%s]: ")
		    call pargstr (SPECTRUM(sh))
	    default:
		if (NSUM(sh) == 1) {
		    if (DAXIS(sh) == 1)
			call sprintf (Memc[str1], SZ_LINE, "[%s[*,%d]]: ")
		    else
			call sprintf (Memc[str1], SZ_LINE, "[%s[%d,*]]: ")
		    call pargstr (SPECTRUM(sh))
		    call pargi (nint (APLOW(sh)))
		} else {
		    if (DAXIS(sh) == 1)
			call sprintf (Memc[str1], SZ_LINE, "[%s[*,%d:%d]]: ")
		    else
			call sprintf (Memc[str1], SZ_LINE, "[%s[%d:%d,*]]: ")
		    call pargstr (SPECTRUM(sh))
		    call pargi (nint (APLOW(sh)))
		    call pargi (nint (APHIGH(sh)))
		}
	    }
	default:
	    switch (NDIM(sh)) {
	    case 1:
		call sprintf (Memc[str1], SZ_LINE, "[%s]: ")
		    call pargstr (SPECTRUM(sh))
	    case 2:
		call sprintf (Memc[str1], SZ_LINE, "[%s[*,%d]]: ")
		    call pargstr (SPECTRUM(sh))
		    call pargi (INDEX1(sh))
	    case 3:
		call sprintf (Memc[str1], SZ_LINE, "[%s[*,%d,%d]]: ")
		    call pargstr (SPECTRUM(sh))
		    call pargi (INDEX1(sh))
		    call pargi (INDEX2(sh))
	    }
	}

	# Copy image title
	call strcpy (TITLE(sh), Memc[str2], SZ_LINE)
	call strcat (Memc[str2], Memc[str1], SZ_LINE)

	# Load exposure time and aperture number
	call sprintf (Memc[str2], SZ_LINE, " %.2fs ap:%d beam:%d")
	    call pargr (IT(sh))
	    call pargi (AP(sh))
	    call pargi (BEAM(sh))
	call strcat (Memc[str2], Memc[str1], SZ_LINE)

	# Set GTOOLS labels.
	call gt_sets (gt, GTTITLE, Memc[str1])
	if (UN_LABEL(UN(sh)) != EOS) {
	    call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
	    call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
	} else {
	    call gt_sets (gt, GTXLABEL, LABEL(sh))
	    call gt_sets (gt, GTXUNITS, UNITS(sh))
	}

	call sfree (sp)
end
