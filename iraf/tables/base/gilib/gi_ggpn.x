include <imio.h>
include "gi.h"

# GI_GGPN -- Procedure to get a group parameter name

int procedure gi_ggpn (im, pn, pname, maxch)

pointer	im		# i: image descriptor
int	pn		# i: parameter number
char	pname[ARB]	# o: parameter name
int	maxch		# i: max name length
#--
int	nc
pointer	stf, pp

int	gstrcpy()

begin
	stf = IM_KDES(im)

	if (pn <= 0 || pn > STF_PCOUNT(stf)) {
	    pname[1] = EOS
	    nc = 0

	} else {
	    pp = STF_PDES(stf,pn)
	    nc = gstrcpy (P_PTYPE(pp), pname, maxch)
	}

	return (nc)
end


	
