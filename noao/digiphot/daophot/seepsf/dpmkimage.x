include	<imhdr.h>
include "../lib/daophotdef.h"

# DP_MKIMAGE -- Make an image from the PSF.

procedure dp_mkimage (dao, im_psf, im_out, dimen, xpsf, ypsf, magnitude)

pointer	dao			# pointer to the DAOPHOT structure
pointer	im_psf			# pointer to the psf image descriptor
pointer	im_out			# pointer to the image descriptor
int	dimen			# size of the image (square)
real	xpsf			# x position of the psf
real	ypsf			# y position of the psf
real	magnitude		# magnitude of the PSF

int	i, j, psf_size, im_size 
pointer	psffit, pixels, pixel
real	psfrad, dxfrom_psf, dyfrom_psf, dvdx, dvdy, start, delta, dx, dy, scale
real	dysq, drsq, psfradsq
pointer	imps2r()
real	imgetr(), dp_usepsf()
errchk	imgetr()

begin
	# Get the other pointers.
	psffit = DP_PSFFIT (dao)
	if (DP_PSFSIZE(psffit) > 0)
	    psfrad = (real (DP_PSFSIZE(psffit) - 1) / 2.0 - 1.0) / 2.0
	else {
            iferr {
                scale = imgetr (im_psf, "SCALE")
            } then {
                psfrad = imgetr (im_psf, "PSFRAD")
            } else {
                psfrad = imgetr (im_psf, "PSFRAD") / scale
            }
	}
	psfradsq = psfrad ** 2
	psf_size = 2 * int (psfrad) + 1 

	IM_NDIM(imout) = 2
	if (IS_INDEFI(dimen)) {
	    IM_LEN(im_out, 1) = psf_size
	    IM_LEN(im_out, 2) = psf_size
	    im_size = psf_size
	} else {
	    IM_LEN(im_out, 1) = dimen
	    IM_LEN(im_out, 2) = dimen
	    im_size = dimen
	}
	IM_PIXTYPE(im_out) = TY_REAL

	if (IS_INDEFR(xpsf)) {
	    dx = DP_PSFX(psffit)
	    dxfrom_psf = (DP_PSFX(psffit) - 1.0) / DP_PSFX(psffit) - 1.0
	} else {
	    dx = xpsf
	    dxfrom_psf = (xpsf - 1.0) / DP_PSFX(psffit) - 1.0
	}
	if (IS_INDEFR(ypsf)) {
	    dy = DP_PSFY(psffit)
	    dyfrom_psf = (DP_PSFY(psffit) - 1.0) / DP_PSFY(psffit) - 1.0
	} else {
	    dy = ypsf
	    dyfrom_psf = (ypsf - 1.0) / DP_PSFY(psffit) - 1.0
	}

	if (IS_INDEFR (magnitude)) {
	    scale = 1.0
	} else {
	    scale = DAO_RELBRIGHT (psffit, magnitude)
	}

	call sprintf (IM_TITLE(im_out), SZ_IMTITLE,
	    "PSF evaluated at X: %.2f Y: %.2f Mag: %.3f")
	    call pargr (dx)
	    call pargr (dy)
	if (IS_INDEFR(magnitude))
	    call pargr (DP_PSFMAG(psffit))
	else
	    call pargr (magnitude)

        # Get the image buffer and evaluate the PSF.
        pixels = imps2r (im_out, 1, int (IM_LEN (im_out, 1)), 1,
	    int (IM_LEN (im_out, 2)))

	# Get the starting coordinates and interpolation interval.
	start = - (psf_size - 1) / 2.0
	delta = real (psf_size - 1) / real (im_size - 1)

	# Evaluate the pixels.
	pixel = pixels
   	do j = 1, im_size {
    	    dy = start + delta * (j - 1)
	    dysq = dy ** 2
    	    do i = 1, im_size {
	    	dx = start + delta * (i - 1)
		drsq = dx ** 2 + dysq
		if (drsq >= psfradsq)
		    Memr[pixel] = 0.0
		else
                    Memr[pixel] = scale * dp_usepsf (DP_PSFUNCTION(psffit),
		        dx, dy, DP_PSFHEIGHT(psffit), Memr[DP_PSFPARS(psffit)],
		        Memr[DP_PSFLUT(psffit)], DP_PSFSIZE(psffit),
		        DP_NVLTABLE(psffit), DP_NFEXTABLE(psffit), dxfrom_psf,
		        dyfrom_psf,  dvdx, dvdy)
		pixel = pixel + 1
	    }
        }
end
