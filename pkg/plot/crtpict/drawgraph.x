# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<imhdr.h>
include	<time.h>
include "wdes.h"
include	"crtpict.h"


# CRT_DRAW_GRAPHICS -- Draw histogram plots and id information at the bottom of
# the output print.

procedure crt_draw_graphics (gp, im, cl, wdes)

pointer	gp	# Pointer to graphics descriptor
pointer	im	# Pointer to input image
pointer	cl	# Pointer to cl parameter structure
pointer	wdes	# Pointer to window descriptor

pointer	w1, w0
char	text[SZ_LINE],  system_id[SZ_LINE]
int	ndev_rows, ndev_cols
real	ndc_xs, ndc_xe, ndc_ys, ndc_ye 	 	# Graphics (NDC) viewport
real	h_xs, h_xe, h_ys, h_ye			# Histogram viewport
real	tx1_xc, tx1_ly, tx2_xs, tx2_ly, tx3_xs, tx3_ly, tx4_xs, tx4_ly
real	px1, px2, py1, py2, pxcenter, pycenter, yres
real	vx1, vx2, vy1, vy2, tx_start, xrf, yrf

int	junk
pointer	sp, buf
int	envfind(), envputs()

int	strlen(), ggeti()
errchk	strlen, ggeti, crt_plot_hgrams, ggwind, ggview
errchk	gtext

begin
	ndev_rows = ggeti (gp, "yr")
	ndev_cols = ggeti (gp, "xr")
	yres = (CRT_YE - CRT_YS) * ndev_rows

	w0 = W_WC (wdes, 0)
	w1 = W_WC (wdes, 1)

	# The (NDC) device coordinates of the entire graphics viewport:
	ndc_xs = CRT_XS 
	ndc_xe = CRT_XE
	ndc_ys = CRT_YS
	ndc_ye = CRT_YS + real (yres * GRAPHICS_FRACTION(cl) / ndev_rows) 

	# Working up from the bottom of the print, locations of various
	# sections of the graphics are are calculated.  String TEXT1 will
	# be centered in x at tx1_xc and have lower y coordinate tx1_ly:

	tx1_xc = (ndc_xe + ndc_xs) / 2.0
	tx1_ly = ndc_ys

	# The three histograms occupy the space calculated next.  This
	# space is broken into individual plots in a separate procedure.

	h_xs = ndc_xs
	h_xe = ndc_xe
	h_ys = ndc_ys + ((ndc_ye - ndc_ys) * (TEXT1 + SPACE))
	h_ye = h_ys + ((ndc_ye - ndc_ys) * HGRAMS)

	# The left starting position of the text strings is calculated to
	# line up with the leftmost histogram window:
	tx_start = h_xs + ((h_xe - h_xs) / 6.0) - ((h_xe - h_xs) / 8.0)

	# String TEXT2 has the following starting_x and lower_y coordinates:
	tx2_xs = tx_start
	tx2_ly = h_ye + ((ndc_ye - ndc_ys) * SPACE)

	# String TEXT3 has these starting_x and lower_y coordinates:
	tx3_xs = tx_start
	tx3_ly = ndc_ys + ((TEXT1 + HGRAMS + TEXT2 + SPACE) * (ndc_ye - ndc_ys))

	# String TEXT4 has these starting_x and lower_y coordinates:
	tx4_xs = tx_start
	tx4_ly = ndc_ys + ((TEXT1+HGRAMS+TEXT2+TEXT3+SPACE) * (ndc_ye - ndc_ys))

	# Draw 3 plots describing transformation of image
	call crt_plot_histograms (gp, cl, im, wdes, h_xs, h_xe, h_ys, h_ye)

	# Set graphics WCS to WCS 0 for text plotting
	call gseti (gp, G_WCS, 0)

	# Text line 3 has the image filename and title string.
	call sprintf (text, SZ_LINE, "%s: %s")
	    call pargstr (W_IMSECT(wdes))
	    call pargstr (IM_TITLE(im))
	call gtext (gp, tx3_xs, tx3_ly, text, "s=0.5")

	# Text line 2 contains image and transformation information; it
	# is necessary to change to WCS_2 to retrieve the information:

	call gseti (gp, G_WCS, 2)
	call ggwind (gp, px1, px2, py1, py2)
	call ggview (gp, vx1, vx2, vy1, vy2)
	call gseti (gp, G_WCS, 0)

	pxcenter = (px1 + px2) / 2.0
	pycenter = (py1 + py2) / 2.0
	xrf = ((vx2 * ndev_cols) - (vx1 * ndev_cols)) / (px2 - px1 + 1.0)
	yrf = ((vy2 * ndev_rows) - (vy1 * ndev_rows)) / (py2 - py1 + 1.0)

	call sprintf (text, SZ_LINE,
	    "ncols=%d nrows=%d zmin=%g zmax=%g xc=%0.2f yc=%0.2f")  
	        call pargi (IM_LEN(im,1))
	        call pargi (IM_LEN(im,2))
	        call pargr (IM_MIN(im))
	        call pargr (IM_MAX(im))
	        call pargr (pxcenter)
	        call pargr (pycenter)
	call sprintf (text[strlen(text)+1], SZ_LINE, " x_rep=%.2f y_rep=%.2f")
	   call pargr (xrf)
	   call pargr (yrf)
	call gtext (gp, tx2_xs, tx2_ly, text, "s=0.35")

	# Text line 1 gives the time and date the output was written
	call sysid (system_id, SZ_LINE)
	call gtext (gp, tx1_xc, tx1_ly, system_id, "h=c;s=0.45")

	# Also output transformation information to STDOUT
	call printf ("ncols=%d nrows=%d zmin=%g zmax=%g xc=%.2f yc=%.2f")
	    call pargi (IM_LEN(im,1))
	    call pargi (IM_LEN(im,2))
	    call pargr (IM_MIN(im))
	    call pargr (IM_MAX(im))
	    call pargr (pxcenter)
	    call pargr (pycenter)
	call printf (" xrf=%.2f yrf=%.2f\n")
	    call pargr (xrf)
	    call pargr (yrf)

	call printf ("%s \n")
	    call pargstr (system_id)

	# The following was added 17Dec85 at the request of the photo lab.
	# It allows the negative to be identified easily by user name in
	# addition to the sequence number written by the 11/23 program.

	call smark (sp)
	call salloc (buf, SZ_LINE, TY_CHAR)

	if (envfind ("userid", Memc[buf], SZ_LINE) <= 0) {
	    call getuid (Memc[buf], SZ_LINE)
	    junk = envputs ("userid", Memc[buf])
	}
	call gtext (gp, CRT_XE, 0.001, Memc[buf], "h=r;v=b;s=1.2")

	call sfree (sp)
end
