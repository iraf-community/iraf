include	<imhdr.h>
include "../lib/daophotdef.h"
include "../lib/daophot.h"

# DP_MKIMAGE -- Make an image from the PSF.

procedure dp_mkimage (dao, im_out, dimen, xpsf, ypsf, magnitude)

pointer	dao			# pointer to the DAOPHOT structure
pointer	im_out			# pointer to the image descriptor
int	dimen			# size of the image (square)
real	xpsf			# x position of the psf
real	ypsf			# y position of the psf
real	magnitude		# magnitude of the PSF

int	psf_size, im_size, ix, iy
pointer	psffit, pixels, pixel
real	xfrom_psf, yfrom_psf, dvdx, dvdy, start, delta
real	dx, dy, brightness

pointer	imps2r()
real	dp_evalpsf()

begin
	# Get the other pointers.
	psffit = DP_PSFFIT (dao)
	psf_size = (DP_PSFSIZE (psffit) - 7) / 2

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

	if (IS_INDEFR(xpsf))
	    xfrom_psf = 0.0
	else
	    xfrom_psf = xpsf - DP_XPSF(psffit)
	if (IS_INDEFR(ypsf))
	    yfrom_psf = 0.0
	else
	    yfrom_psf = ypsf - DP_YPSF(psffit)

	if (IS_INDEFR (magnitude))
	    brightness = 1.0
	else
	    brightness = DAO_RELBRIGHT (psffit, magnitude)

	start = - (psf_size - 1) / 2.0
	delta = real (psf_size) / im_size

        # Get the image buffer and evaluate the PSF.
        pixels = imps2r (im_out, 1, int (IM_LEN (im_out, 1)), 1,
	    int (IM_LEN (im_out, 2)))

	# Evaluate the pixels.
	pixel = pixels
   	do iy = 1, im_size {
    	    dy = start + delta * (iy - 1)
    	    do ix = 1, im_size {
	    	dx = start + delta * (ix - 1)
                Memr[pixel] = brightness * dp_evalpsf (dx, dy, psffit,
		    xfrom_psf, yfrom_psf, DP_VARPSF(dao), dvdx, dvdy)
		pixel = pixel + 1
	    }
        }

end
