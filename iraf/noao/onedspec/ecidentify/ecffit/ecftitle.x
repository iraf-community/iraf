include	<pkg/gtools.h>

# ECF_TITLE -- Set the GTOOLS parameter string.

procedure ecf_title (gt)

pointer	gt		# GTOOLS pointer

include	"ecffit.com"

begin
	call sprintf (ecfstr, SZ_LINE,
	    "Function=%s, xorder=%d, yorder=%d, slope=%d, offset=%d, rms=%6g")
	    call pargstr (function)
	    call pargi (xorder)
	    call pargi (yorder)
	    call pargi (slope)
	    call pargi (offset)
	    call pargd (rms)
	call gt_sets (gt, GTPARAMS, ecfstr)
	call gt_sets (gt, GTTITLE, "Echelle Dispersion Function Fitting")

	switch (xtype) {
	case 'p':
	    call gt_sets (gt, GTXLABEL, "Pixel")
	case 'o':
	    call gt_sets (gt, GTXLABEL, "Order")
	case 'w':
	    call gt_sets (gt, GTXLABEL, "Wavelength")
	case 'r':
	    call gt_sets (gt, GTXLABEL, "Residual")
	case 'v':
	    call gt_sets (gt, GTXLABEL, "Velocity")
	}

	switch (ytype) {
	case 'p':
	    call gt_sets (gt, GTYLABEL, "Pixel")
	case 'o':
	    call gt_sets (gt, GTYLABEL, "Order")
	case 'w':
	    call gt_sets (gt, GTYLABEL, "Wavelength")
	case 'r':
	    call gt_sets (gt, GTYLABEL, "Residual")
	case 'v':
	    call gt_sets (gt, GTYLABEL, "Velocity")
	}
end
