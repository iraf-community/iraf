# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"wdes.h"
include	"crtpict.h"

# CRT_PLOT_IMAGE - Plot the image, graphics and greyscale portion of
# each image to be transformed.

procedure crt_plot_image (gp, im, image, cl)

pointer	gp			# Graphics descriptor
pointer	im			# Pointer to image
char	image[SZ_FNAME]		# Image filename
pointer	cl			# Pointer to structure of cl parameters

pointer	sp, wdes
errchk	crt_establish_transform, crt_transform_image
errchk  crt_draw_graphics, crt_draw_greyscale

begin
	call smark (sp)
	call salloc (wdes, LEN_WDES, TY_STRUCT)
	call strcpy (image, W_IMSECT(wdes), W_SZIMSECT)

	if (IMAGE_FRACTION(cl) > EPSILON) {
	    call crt_establish_transform (gp, im, cl, wdes)
	    call crt_transform_image (gp, im, wdes, cl)
	}

	if (GRAPHICS_FRACTION(cl) > EPSILON)  {
	    call crt_draw_graphics (gp, im, cl, wdes)
	}

	if (GREYSCALE_FRACTION(cl) > EPSILON) {
	    call crt_draw_greyscale (gp, cl)
	}

	call sfree (sp)
end
