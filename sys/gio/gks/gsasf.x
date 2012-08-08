# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "gks.h"

# GSASF -- Set aspect source flags.  Aspect source flags allow the following 
# elements to be set to either GBUNDL or GINDIV:
#	 1	linetype ASF
#	 2	linewidth scale factor ASF
#	 3	polyline colour index ASF
#	 4	marker type ASF
#	 5	marker size scale factor ASF
#	 6	polymarker colout index ASF
#	 7	text font and precision factor ASF
#	 8	character expansion factor ASF
#	 9	character spacing ASF
#	 10	text colour index ASF
#	 11	fill area interior style ASF
#	 12	fill area style index ASF
#	 13	fill area colout index ASF

procedure gsasf (lasf)

int	lasf[13]	# List of aspect source flags
int	i
include	"gks.com"

begin
	do i = 1, NASF
	    gk_asf[i] = lasf[i]
end
