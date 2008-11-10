# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>

# INIT_MARKER -- Returns integers code for marker type string.

procedure init_marker (marker, imark)

char	marker[SZ_FNAME]	# Marker type as a string
int	imark			# Integer code for marker - returned

bool	streq()

begin
	if (streq (marker, "point"))
	    imark = GM_POINT
	else if (streq (marker,   "box"))
	    imark = GM_BOX
	else if (streq (marker,  "plus"))
	    imark = GM_PLUS
	else if (streq (marker, "cross"))
	    imark = GM_CROSS
	else if (streq (marker, "circle"))
	    imark = GM_CIRCLE
	else if (streq (marker, "hebar"))
	    imark = GM_HEBAR
	else if (streq (marker, "vebar"))
	    imark = GM_VEBAR
	else if (streq (marker, "hline"))
	    imark = GM_HLINE
	else if (streq (marker, "vline"))
	    imark = GM_VLINE
	else if (streq (marker, "diamond"))
	    imark = GM_DIAMOND
	else {
	    if (streq (marker, "line") ||
	        streq (marker, "lhist") ||
		streq (marker, "bhist"))
		imark = GM_BOX
	    else {
		call eprintf ("Unrecognized marker type, using 'box'\n")
		imark = GM_BOX
	    }
	}
end


