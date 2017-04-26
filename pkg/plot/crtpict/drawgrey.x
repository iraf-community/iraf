# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<mach.h>
include	<gset.h>
include	"wdes.h"
include	"crtpict.h"

# CRT_DRAW_GREYSCALE -- Draw steps representing greyscale increments on output.

procedure crt_draw_greyscale (gp, cl)

pointer	gp
pointer	cl

short 	grey[NSTEPS]
char	label[SZ_LABEL]
int	ndev_rows, i, dummy
real	ndc_xs, ndc_xe, ndc_ys, ndc_ye, yres, del_y
real	delta_grey, delta_x, x_start, x, dz1, dz2

bool	ggetb()
int	ggeti(), itoc()
real	ggetr()
errchk	ggetb, ggeti, ggetr, gpcell, ggetr, gtext

begin
	if (ggetb (gp, "z1") && ggetb (gp, "z2")) {
	    dz1 = ggetr (gp, "z1")
	    dz2 = ggetr (gp, "z2")
	} else {
	    dz1 = 0.
	    dz2 = 255.
	}

	ndev_rows = ggeti (gp, "yr")
	yres = (CRT_YE - CRT_YS) * ndev_rows

	# The (NDC) device coordinates of the greyscale_window are calculated.

	ndc_xs = CRT_XS 
	ndc_xe = CRT_XE
	ndc_ys = CRT_YS + ((GRAPHICS_FRACTION(cl) + IMAGE_FRACTION(cl) + SPACE)*
	    yres) / ndev_rows
	ndc_ye = ndc_ys + ((yres * GREYSCALE_FRACTION(cl)) / ndev_rows) 
	ndc_ye = min (ndc_ye, CRT_YE)
	del_y = ndc_ye - ndc_ys

	# Calculate and output grey levels
	call gseti (gp, G_WCS, 0)
	delta_grey = (dz2 - dz1) / real(NSTEPS - 1)
	delta_x = (ndc_xe - ndc_xs) / NSTEPS 
	x_start = ndc_xs + (delta_x / 2.0)
	do i = 1, NSTEPS {
	    grey[i] = short (dz1 + (i-1) * delta_grey + 0.5)
	    dummy = itoc (int(grey[i]), label, SZ_LABEL)
	    x = x_start + (i - 1) * delta_x
	    call gtext (gp, x, ndc_ys, label, "h=c;s=0.25;v=t")
	}
	
	call gpcell (gp, grey, NSTEPS, 1, ndc_xs, ndc_ys + (0.05 * del_y), 
	    ndc_xe, ndc_ye)
end
