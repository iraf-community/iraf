include "xregister.h"

# RG_PXPARS -- Update the cross-correlation algorithm parameters.

procedure rg_pxpars (xc)

pointer	xc		#I pointer to the cross-correlation structure

pointer	sp, str
int	rg_xstati()
real	rg_xstatr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Define the regions.
	call rg_xstats (xc, REGIONS, Memc[str], SZ_LINE)
	call clpstr ("regions", Memc[str])
	call clputi ("xlag", rg_xstati (xc, XLAG))
	call clputi ("ylag", rg_xstati (xc, YLAG))
	call clputi ("dxlag", rg_xstati (xc, DXLAG))
	call clputi ("dylag", rg_xstati (xc, DYLAG))

	# Store the background fitting parameters.
	call rg_xstats (xc, BSTRING, Memc[str], SZ_LINE)
	call clpstr ("background", Memc[str])
	call clputi ("border", rg_xstati (xc, BORDER))
	call clputr ("loreject", rg_xstatr (xc, LOREJECT))
	call clputr ("hireject", rg_xstatr (xc, HIREJECT))
	call clputr ("apodize", rg_xstatr (xc, APODIZE))
	call rg_xstats (xc, FSTRING, Memc[str], SZ_LINE)
	call clpstr ("filter", Memc[str])

	# Store the cross-correlation parameters.
	call rg_xstats (xc, CSTRING, Memc[str], SZ_LINE)
	call clpstr ("correlation", Memc[str])
	call clputi ("xwindow", rg_xstati (xc, XWINDOW))
	call clputi ("ywindow", rg_xstati (xc, YWINDOW))

	# Store the peak centering parameters.
	call rg_xstats (xc, PSTRING, Memc[str], SZ_LINE)
	call clpstr ("function", Memc[str])
	call clputi ("xcbox", rg_xstati (xc, XCBOX))
	call clputi ("ycbox", rg_xstati (xc, YCBOX))

	call sfree (sp)
end
