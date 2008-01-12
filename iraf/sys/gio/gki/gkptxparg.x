# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# GKP_TXPARG -- Convert a short integer text attribute code into a string
# and pass the string to FMTIO.

procedure gkp_txparg (code)

short	code			# defined in <gset.h>

begin
	switch (code) {
	case GT_NORMAL:
	    call pargstr ("normal")
	case GT_CENTER:
	    call pargstr ("center")
	case GT_LEFT:
	    call pargstr ("left")
	case GT_RIGHT:
	    call pargstr ("right")
	case GT_UP:
	    call pargstr ("up")
	case GT_DOWN:
	    call pargstr ("down")
	case GT_TOP:
	    call pargstr ("top")
	case GT_BOTTOM:
	    call pargstr ("bottom")
	case GT_ROMAN:
	    call pargstr ("roman")
	case GT_GREEK:
	    call pargstr ("greek")
	case GT_ITALIC:
	    call pargstr ("italic")
	case GT_BOLD:
	    call pargstr ("bold")
	case GT_LOW:
	    call pargstr ("low")
	case GT_MEDIUM:
	    call pargstr ("medium")
	case GT_HIGH:
	    call pargstr ("high")
	default:
	    call pargstr ("??")
	}
end
