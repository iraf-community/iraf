include "xregister.h"

# RG_XGPARS -- Read in the XREGISTER task algorithm parameters.

procedure rg_xgpars (xc)

pointer	xc		#I pointer to the main structure

int	xlag, ylag, xwindow, ywindow, xcbox, ycbox
pointer	sp, str
int	clgwrd(), clgeti()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Initialize the correlation structure.
	call rg_xinit (xc, clgwrd ("correlation", Memc[str], SZ_LINE,
	    XC_CTYPES))

	# Fetch the initial shift information.
	xlag = clgeti ("xlag")
	ylag = clgeti ("ylag")
	call rg_xseti (xc, IXLAG, xlag)
	call rg_xseti (xc, IYLAG, ylag)
	call rg_xseti (xc, XLAG, xlag)
	call rg_xseti (xc, YLAG, ylag)
	call rg_xseti (xc, DXLAG, clgeti ("dxlag"))
	call rg_xseti (xc, DYLAG, clgeti ("dylag"))

	# Get the background value computation parameters.
	call rg_xseti (xc, BACKGRD, clgwrd ("background", Memc[str], SZ_LINE,
	    XC_BTYPES))
	call rg_xsets (xc, BSTRING, Memc[str])
	call rg_xseti (xc, BORDER, clgeti ("border"))
	call rg_xsetr (xc, LOREJECT, clgetr ("loreject"))
	call rg_xsetr (xc, HIREJECT, clgetr ("hireject"))
	call rg_xsetr (xc, APODIZE, clgetr ("apodize"))
	call rg_xseti (xc, FILTER, clgwrd ("filter", Memc[str], SZ_LINE,
	    XC_FTYPES))
	call rg_xsets (xc, FSTRING, Memc[str])

	# Get the window parameters and force the window size to be odd.
	xwindow = clgeti ("xwindow")
	if (mod (xwindow,2) == 0)
	    xwindow = xwindow + 1
	call rg_xseti (xc, XWINDOW, xwindow)
	ywindow = clgeti ("ywindow")
	if (mod (ywindow,2) == 0)
	    ywindow = ywindow + 1
	call rg_xseti (xc, YWINDOW, ywindow)

	# Get the peak fitting parameters.
	call rg_xseti (xc, PFUNC, clgwrd ("function", Memc[str], SZ_LINE,
	    XC_PTYPES))
	xcbox = clgeti ("xcbox")
	if (mod (xcbox,2) == 0)
	    xcbox = xcbox + 1
	call rg_xseti (xc, XCBOX, xcbox)
	ycbox = clgeti ("ycbox")
	if (mod (ycbox,2) == 0)
	    ycbox = ycbox + 1
	call rg_xseti (xc, YCBOX, ycbox)

	call sfree (sp)
end
