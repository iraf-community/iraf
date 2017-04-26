include	<pkg/gtools.h>
include	<smw.h>
include	<units.h>

# MKTITLE -- Make a spectrum title (IIDS style)

procedure mktitle (sh, gt)

pointer	sh, gt

pointer	sp, str

begin
	# Do nothing if the GTOOLS pointer is undefined.
	if (gt == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	call sprintf (Memc[str], SZ_LINE,
	    "[%s%s]: %s %.2s ap:%d beam:%d")
	    call pargstr (IMNAME(sh))
	    call pargstr (IMSEC(sh))
	    call pargstr (TITLE(sh))
	    call pargr (IT(sh))
	    call pargi (AP(sh))
	    call pargi (BEAM(sh))

	# Set GTOOLS labels.
	call gt_sets (gt, GTTITLE, Memc[str])
	if (UN_LABEL(UN(sh)) != EOS) {
	    call gt_sets (gt, GTXLABEL, UN_LABEL(UN(sh)))
	    call gt_sets (gt, GTXUNITS, UN_UNITS(UN(sh)))
	} else {
	    call gt_sets (gt, GTXLABEL, LABEL(sh))
	    call gt_sets (gt, GTXUNITS, UNITS(sh))
	}

	call sfree (sp)
end
