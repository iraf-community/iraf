include <imhdr.h>

# MK_MAG -- Procedure to compute the x and y magnification factors.

procedure mk_mag (im, iw, xmag, ymag)

pointer	im		# pointer to the frame buffer
pointer	iw		# pointer to the wcs structure
real	xmag, ymag	# x and y magnifications

real	xll, yll, xur, yur

begin
	# Compute the x and y magnification.
	call iw_fb2im (iw, 1.0, 1.0, xll, yll)
	call iw_fb2im (iw, real (IM_LEN(im,1)), real (IM_LEN(im,2)), xur, yur)

	xmag = abs (xur - xll) / (IM_LEN(im,1) - 1)
	ymag = abs (yur - yll) / (IM_LEN(im,2) - 1)
end
