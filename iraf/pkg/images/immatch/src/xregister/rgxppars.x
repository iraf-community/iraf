include "xregister.h"

# RG_PXPARS -- Update the cross-correlation algorithm parameters.

procedure rg_pxpars (xc)

pointer	xc		#I pointer to the cross-correlation structure

size_t	sz_val
pointer	sp, str
int	rg_xstati()
long	rg_xstatl()
real	rg_xstatr()

begin
	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Define the regions.
	call rg_xstats (xc, REGIONS, Memc[str], SZ_LINE)
	call clpstr ("regions", Memc[str])
	call clputl ("xlag", rg_xstatl (xc, XLAG))
	call clputl ("ylag", rg_xstatl (xc, YLAG))
	call clputl ("dxlag", rg_xstatl (xc, DXLAG))
	call clputl ("dylag", rg_xstatl (xc, DYLAG))

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
	call clputl ("xwindow", rg_xstatl (xc, XWINDOW))
	call clputl ("ywindow", rg_xstatl (xc, YWINDOW))

	# Store the peak centering parameters.
	call rg_xstats (xc, PSTRING, Memc[str], SZ_LINE)
	call clpstr ("function", Memc[str])
	call clputl ("xcbox", rg_xstatl (xc, XCBOX))
	call clputl ("ycbox", rg_xstatl (xc, YCBOX))

	call sfree (sp)
end
