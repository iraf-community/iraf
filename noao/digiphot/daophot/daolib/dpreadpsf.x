include	<imhdr.h>
include "../lib/daophotdef.h"

# DP_READPSF -- Read in the PSF from the specified image.

procedure dp_readpsf (dao, im)

pointer	dao			# pointer to the DAOPHOT Structure
pointer	im			# image descriptor

int	ncols
pointer	sp, v, psffit, psflut, buf
real	scale
int	imgnlr()
real	imgetr()
errchk	imgetr()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (v, IM_MAXDIM, TY_LONG)

	# Set up needed daophot pointers.
	psffit = DP_PSFFIT(dao)

	# Get the psf parameters from the image header.
	iferr {
	    scale = imgetr (im, "SCALE")
	} then {
	    DP_PSFRAD(dao) = min (DP_RPSFRAD(dao) / DP_SCALE(dao),
	        imgetr (im, "PSFRAD"))
	    DP_SPSFRAD(dao) = DP_SCALE(dao) * DP_PSFRAD(dao)
	} else {
	    DP_PSFRAD(dao) = min (DP_RPSFRAD(dao) / DP_SCALE(dao),
	        imgetr (im, "PSFRAD") / scale)
	    DP_SPSFRAD(dao) = DP_SCALE(dao) * DP_PSFRAD(dao)
	}
	DP_PSFHEIGHT (psffit) = imgetr (im, "HEIGHT")
	DP_PSFDXCEN (psffit) = imgetr (im, "XOFFSET")
	DP_PSFDYCEN (psffit) = imgetr (im, "YOFFSET")
	DP_PSFSIGX (psffit) = imgetr (im, "SIGMAX")	
	DP_PSFSIGY (psffit) = imgetr (im, "SIGMAY")
	DP_PSFMAG (psffit) = imgetr (im, "PSFMAG")
	DP_XPSF (psffit) = imgetr (im, "XPSF")
	DP_YPSF (psffit) = imgetr (im, "YPSF")
	DP_PSFSIZE(psffit) = IM_LEN(im,1)

	# Allocate space for the PSFLUT.
	if (IM_NDIM(im) == 3) {
	    DP_VARPSF(dao) = YES
	    call realloc (DP_PSFLUT(psffit), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit) * 3, TY_REAL)
	} else {
	    DP_VARPSF(dao) = NO
	    call realloc (DP_PSFLUT(psffit), DP_PSFSIZE(psffit) *
	        DP_PSFSIZE(psffit), TY_REAL)
	}

	# Find the number of lines and columns in the psf image.
	ncols = DP_PSFSIZE(psffit)

	# Now read the PSF image.
	psflut = DP_PSFLUT (psffit)
	call amovkl (long(1), Meml[v], IM_MAXDIM)
	while (imgnlr (im, buf, Meml[v]) != EOF) {
	    call amovr (Memr[buf], Memr[psflut], ncols)
	    psflut = psflut + ncols
	}

	call sfree (sp)
end
