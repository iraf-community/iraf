include	"ccdtypes.h"


# CCDNSCAN -- Return the number CCD scan rows.
#
# If not found in the header return the "nscan" parameter for objects and
# 1 for calibration images.

int procedure ccdnscan (im, ccdtype)

pointer	im			#I Image
int	ccdtype			#I CCD type
int	nscan			#O Number of scan lines

bool	clgetb()
char	type, clgetc()
int	hdmgeti(), clgeti()

begin
	iferr (nscan = hdmgeti (im, "nscanrow")) {
	    switch (ccdtype) {
	    case ZERO, DARK, FLAT, ILLUM, FRINGE:
		nscan = 1
	    default:
		type = clgetc ("scantype")
		if (type == 's')
		    nscan = clgeti ("nscan")
		else {
		    if (clgetb ("scancor"))
			nscan = INDEFI
		    else
			nscan = 1
		}
	    }
	}

	return (nscan)
end
