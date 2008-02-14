# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include	<math/iminterp.h>

# MSISINIT -- Procedure to initialize the sewquential 2D image interpolation
# package.  MSISINIT checks that the interpolant is one of the permitted
# types and allocates space for the interpolant descriptor structure.
# MSIINIT returns the pointer to the interpolant descriptor structure.

procedure msisinit (msi, interp_type, nsinc, nxincr, nyincr, xshift, yshift,
	badval)

pointer	msi		# pointer to the interpolant descriptor structure
int	interp_type	# interpolant type
int	nsinc		# nsinc interpolation width
int	nxincr, nyincr	# number of look-up table elements in x and y
real	xshift, yshift	# the x and y shifts
real	badval		# undefined value for drizzle interpolant

int	nconv
errchk	malloc

begin
	if (interp_type < 1 || interp_type > II_NTYPES2D) {
	    call error (0, "MSIINIT: Illegal interpolant.")
	} else {
	    call calloc (msi, LEN_MSISTRUCT, TY_STRUCT)
	    MSI_TYPE(msi) = interp_type
	    switch (interp_type) {

	    case II_BILSINC:
		MSI_NSINC(msi) = (nsinc - 1) / 2
		MSI_NXINCR(msi) = nxincr
		MSI_NYINCR(msi) = nyincr
		if (nxincr > 1) {
		    MSI_NXINCR(msi) = MSI_NXINCR(msi) + 1
		    MSI_XSHIFT(msi) = INDEFR
		} else {
		    MSI_XSHIFT(msi) = xshift
		}
		if (nyincr > 1) {
		    MSI_YSHIFT(msi) = INDEFR
		    MSI_NYINCR(msi) = MSI_NYINCR(msi) + 1
		} else {
		    MSI_YSHIFT(msi) = yshift
		}
		MSI_XPIXFRAC(msi) = PIXFRAC
		MSI_YPIXFRAC(msi) = PIXFRAC
		nconv = 2 * MSI_NSINC(msi) + 1
		call calloc (MSI_LTABLE(msi), nconv * MSI_NXINCR(msi) * nconv *
		    MSI_NYINCR(msi), TY_REAL)
		call ii_bisinctable (LTABLE(MSI_LTABLE(msi)), nconv,
		    MSI_NXINCR(msi), MSI_NYINCR(msi), MSI_XSHIFT(msi),
		    MSI_YSHIFT(msi))

	    case II_BISINC:
		MSI_NSINC(msi) = (nsinc - 1) / 2
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_XPIXFRAC(msi) = PIXFRAC
		MSI_YPIXFRAC(msi) = PIXFRAC
		MSI_LTABLE(msi) = NULL

	    case II_BIDRIZZLE:
		MSI_NSINC(msi) = 0
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_XPIXFRAC(msi) = max (MIN_PIXFRAC, min (xshift, 1.0))
		MSI_YPIXFRAC(msi) = max (MIN_PIXFRAC, min (yshift, 1.0))
		MSI_LTABLE(msi) = NULL

	    default:
		MSI_NSINC(msi) = 0
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_XPIXFRAC(msi) = PIXFRAC
		MSI_YPIXFRAC(msi) = PIXFRAC
		MSI_LTABLE(msi) = NULL

	    }
	    MSI_COEFF(msi) = NULL
	    MSI_BADVAL(msi) = badval
	}
end
