include "xregister.h"

# RG_XGPARS -- Read in the XREGISTER task algorithm parameters.

procedure rg_xgpars (xc)

pointer	xc		#I pointer to the main structure

size_t	sz_val
long	c_2
long	xlag, ylag, xwindow, ywindow, xcbox, ycbox
pointer	sp, str
int	clgwrd(), clgeti()
long	clgetl(), lmod()
real	clgetr()

begin
	c_2 = 2

	# Allocate working space.
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (str, sz_val, TY_CHAR)

	# Initialize the correlation structure.
	call rg_xinit (xc, clgwrd ("correlation", Memc[str], SZ_LINE,
	    XC_CTYPES))

	# Fetch the initial shift information.
	xlag = clgetl ("xlag")
	ylag = clgetl ("ylag")
	call rg_xsetl (xc, IXLAG, xlag)
	call rg_xsetl (xc, IYLAG, ylag)
	call rg_xsetl (xc, XLAG, xlag)
	call rg_xsetl (xc, YLAG, ylag)
	call rg_xsetl (xc, DXLAG, clgetl ("dxlag"))
	call rg_xsetl (xc, DYLAG, clgetl ("dylag"))

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
	xwindow = clgetl ("xwindow")
	if (lmod(xwindow,c_2) == 0)
	    xwindow = xwindow + 1
	call rg_xsetl (xc, XWINDOW, xwindow)
	ywindow = clgetl ("ywindow")
	if (lmod(ywindow,c_2) == 0)
	    ywindow = ywindow + 1
	call rg_xsetl (xc, YWINDOW, ywindow)

	# Get the peak fitting parameters.
	call rg_xseti (xc, PFUNC, clgwrd ("function", Memc[str], SZ_LINE,
	    XC_PTYPES))
	xcbox = clgetl ("xcbox")
	if (lmod(xcbox,c_2) == 0)
	    xcbox = xcbox + 1
	call rg_xsetl (xc, XCBOX, xcbox)
	ycbox = clgetl ("ycbox")
	if (lmod(ycbox,c_2) == 0)
	    ycbox = ycbox + 1
	call rg_xsetl (xc, YCBOX, ycbox)

	call sfree (sp)
end
