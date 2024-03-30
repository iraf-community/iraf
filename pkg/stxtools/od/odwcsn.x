include <mwset.h>
include "od.h"

#---------------------------------------------------------------------------
.help od_wcs_open May93 source
.ih
NAME
od_wcs_open -- Open the WCS information for an image.
.endhelp
#---------------------------------------------------------------------------
procedure od_wcs_open (od)

pointer od                      # I:  Image descriptor.

pointer mw_openim()
pointer mw_sctran()
bool    streq()

begin
	if (OD_TYPE(od) == OD_IMAGE) {
	    OD_MW(od) = mw_openim (OD_FD(od))
	    call mw_gwattrs (OD_MW(od), 0, "system", OD_WSYS(od), SZ_LINE)
	    if (streq ("multispec", OD_WSYS(od))) {
		call mw_seti (OD_MW(od), MW_USEAXMAP, NO)
		OD_WL(od) = mw_sctran (OD_MW(od), "multispec", "logical", 3b)
		OD_LW(od) = mw_sctran (OD_MW(od), "logical", "multispec", 3b)
	    } else {
		OD_WL(od) = mw_sctran (OD_MW(od), "world", "logical", 1)
		OD_LW(od) = mw_sctran (OD_MW(od), "logical", "world", 1)
	    }
	} else {
	    OD_MW(od) = NULL
	    OD_LW(od) = NULL
	    OD_WL(od) = NULL
	}
end
#---------------------------------------------------------------------------
# End of od_wcs_open
#---------------------------------------------------------------------------
