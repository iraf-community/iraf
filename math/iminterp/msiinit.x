# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "im2interpdef.h"
include	<math/iminterp.h>

# MSIINIT -- Procedure to initialize the sewquential 2D image interpolation
# package.  MSIINIT checks that the interpolant is one of the permitted
# types and allocates space for the interpolant descriptor structure.
# MSIINIT returns the pointer to the interpolant descriptor structure.

procedure msiinit (msi, interp_type)

pointer	msi		# pointer to the interpolant descriptor structure
int	interp_type	# interpolant type

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
		MSI_NSINC(msi) = NSINC
		MSI_NXINCR(msi) = NINCR
		if (MSI_NXINCR(msi) > 1)
		    MSI_NXINCR(msi) = MSI_NXINCR(msi) + 1
		MSI_NYINCR(msi) = NINCR
		if (MSI_NYINCR(msi) > 1)
		    MSI_NYINCR(msi) = MSI_NYINCR(msi) + 1
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		nconv = 2 * MSI_NSINC(msi) + 1
		call calloc (MSI_LTABLE(msi), nconv * MSI_NXINCR(msi) * nconv *
		    MSI_NYINCR(msi), TY_REAL)
		call ii_bisinctable (LTABLE(MSI_LTABLE(msi)), nconv,
		    MSI_NXINCR(msi), MSI_NYINCR(msi), MSI_XSHIFT(msi),
		    MSI_YSHIFT(msi))
	    case II_BISINC:
		MSI_NSINC(msi) = NSINC
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_LTABLE(msi) = NULL
	    case II_BIDRIZZLE:
		MSI_NSINC(msi) = 0
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_XPIXFRAC(msi) = PIXFRAC
		MSI_YPIXFRAC(msi) = PIXFRAC
		MSI_LTABLE(msi) = NULL
	    default:
		MSI_NSINC(msi) = 0
		MSI_NXINCR(msi) = 0
		MSI_NYINCR(msi) = 0
		MSI_XSHIFT(msi) = INDEFR
		MSI_YSHIFT(msi) = INDEFR
		MSI_LTABLE(msi) = NULL
	    }
	    MSI_COEFF(msi) = NULL
	    MSI_BADVAL(msi) = BADVAL
	}
end
