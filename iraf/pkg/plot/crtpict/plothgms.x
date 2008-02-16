# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<imhdr.h>
include	<mach.h>
include "wdes.h"
include	"crtpict.h"

# CRT_PLOT_HISTOGRAMS -- Calculate and plot three histograms describing the
# intensity to greyscale mapping.

procedure crt_plot_histograms (gp, cl, im, wdes, xs, xe, ys, ye)

pointer	gp
pointer	cl		# Pointer to cl structure.
pointer im
pointer	wdes
real	xs, xe, ys, ye, z1, z2

pointer	w1, inten_hgram, greys_hgram, sp, text, syv, greys, hgram
pointer real_greys, xval, yval
int	nsig_bits, i, zrange, mask
real	plot_ys, plot_ye, plot_width, plot_spacing, x, y, delta_inten, inten
real	plot1_xs, plot1_xe, plot2_xs, plot2_xe, plot3_xs, plot3_xe 
real	dz1, dz2, gio_char_x
real	major_length, minor_length, ux1, ux2, uy1, uy2, y_pos, label_y
real	wx1, wx2, wy1, wy2

bool	ggetb()
real 	ggetr()
int	ggeti(), and()
errchk	ggetb, ggeti, ggetr, crt_calc_hgrams, ggwind, gswind, gsview, gploto
errchk	gsetr, gseti, glabax, amapr, gpline, gvline, achtir, gtext

begin
	call smark (sp)
	call salloc (inten_hgram, NBINS, TY_INT)
	call salloc (greys_hgram, NBINS, TY_INT)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (syv, NBINS, TY_SHORT)
	call salloc (greys, NBINS, TY_INT)
	call salloc (hgram, NBINS, TY_REAL)
	call salloc (real_greys, NBINS, TY_REAL)
	call salloc (xval, NBINS, TY_REAL)
	call salloc (yval, NBINS, TY_REAL)

	# First, get pointer to WCS 1 and some device parameters.
	w1 = W_WC(wdes, 1)
	z1 = W_ZS(w1)
	z2 = W_ZE(w1)

	# If z1 and z2 not in graphcap, set to some reasonable numbers for
	# plots to be generated.

	if (ggetb (gp, "z1") && ggetb (gp, "z2")) {
	    dz1 = ggetr (gp, "z1")
	    dz2 = ggetr (gp, "z2")
	} else {
	    dz1 = 0.
	    dz2 = 255.
	}

	# To allow room for annotation, the y limits of each plot are
	# drawn in by 5%.  The y limits are the same for each plot.

	plot_ys = ys + (0.05 * (ye - ys))
	plot_ye = ye - (0.05 * (ye - ys))
	label_y = plot_ys - (plot_ye - plot_ys) * 0.20

	# Now calculate the x limits.  Each plot occupys a fourth of the
	# available space in x.  The distance between plot centers is a
	# third of the available space.

	plot_width = (xe - xs) / 4.0
	plot_spacing = (xe - xs) / 3.0

	plot1_xs = xs + (plot_spacing / 2.0) - (plot_width / 2.0)
	plot1_xe = plot1_xs + plot_width
	plot2_xs = plot1_xs + plot_spacing
	plot2_xe = plot2_xs + plot_width
	plot3_xs = plot2_xs + plot_spacing
	plot3_xe = plot3_xs + plot_width

	# Calculate the histograms for both the untransformed (intensity) and
	# transformed (greyscale) image in a single procedure. A separate
	# path is taken for linear or user transformations:

	if (W_ZT(w1) == W_USER)
	    call crt_user_hgram (im, gp, z1, z2, Mems[LUT(cl)], 
		Memi[inten_hgram], Memi[greys_hgram])
	else
	    call crt_linear_hgram (im, gp, z1, z2, W_ZT(w1), Memi[inten_hgram], 
	        Memi[greys_hgram])

	# Each histogram plot is a separate mapping in WCS 3
	call gseti (gp, G_WCS, 3)

	# The first histogram shows the number of pixels at a given
	# intensity versus intensity for the original image.

	call gsview (gp, plot1_xs, plot1_xe, plot_ys, plot_ye)
	gio_char_x = ((plot1_xe - plot1_xs) / 50.) / ggetr (gp, "cw")
	major_length = gio_char_x * (ggetr (gp, "cw"))
	minor_length = gio_char_x * (0.5 * (ggetr (gp, "cw")))

	call gseti (gp, G_YTRAN, GW_LOG)
	call gseti (gp, G_ROUND, YES)
	call gsetr (gp, G_MAJORLENGTH, major_length)
	call gsetr (gp, G_MINORLENGTH, minor_length)
	call gseti (gp, G_LABELAXIS, NO)
	call gsetr (gp, G_TICKLABELSIZE, 0.25)
	call achtir (Memi[inten_hgram], Memr[hgram], NBINS)
	call gploto (gp, Memr[hgram], NBINS, IM_MIN(im), IM_MAX(im), "")

	# Now to label the plot:
	call ggwind (gp, ux1, ux2, uy1, uy2)
	y_pos = uy1 - ((uy2 - uy1) * 0.20)  #y_pos below yaxis by 20% of height
	call gseti (gp, G_YTRAN, GW_LINEAR)
	call gtext (gp, (ux1+ux2)/2.0, y_pos, "LOG10(N(DN)) VS DN", 
	    "v=t;h=c;s=.25")

	# The third plot shows the number of pixels at a given greyscale
	# level versus greyscale level for the range of intensities
	# transformed. 

	call gsview (gp, plot3_xs, plot3_xe, plot_ys, plot_ye)
	call achtir (Memi[greys_hgram], Memr[hgram], NBINS)

	if (z2 > z1)
	    call gploto (gp, Memr[hgram], NBINS, dz1, dz2, "")
	else
	    call gploto (gp, Memr[hgram], NBINS, dz2, dz1, "")

	# Now to label the plot:
	call ggwind (gp, ux1, ux2, uy1, uy2)
	call gseti (gp, G_YTRAN, GW_LINEAR)
	y_pos = uy1 - ((uy2 - uy1) * 0.20) # y_pos below yaxis by 20% of height
	call gtext (gp, (ux1+ux2)/2.0, y_pos, "TRANSFORMED HISTOGRAM", 
	    "v=t;h=c;s=.25")

	# The second plot shows how the dynamic range of the transformed
	# image maps to the dynamic range of the output device.

	call gsview (gp, plot2_xs, plot2_xe, plot_ys, plot_ye)
	call gswind (gp, IM_MIN(im), IM_MAX(im), real (dz1), real (dz2))
	call gseti (gp, G_YTRAN, GW_LINEAR)
	call glabax (gp, "", "", "")

	if (W_ZT(w1) != W_UNITARY) {
	    do i = 1, NBINS 
		Memr[xval+i-1] = IM_MIN(im) + (i-1) * (IM_MAX(im) - 
		    IM_MIN(im))/ (NBINS-1)

	    if (W_ZT(w1) == W_USER) {
	        call sprintf (Memc[text], SZ_LINE, 
		    "USER DEFINED FUNCTION: FROM %g TO %g")
		call pargr (z1)
		call pargr (z2)
		call amapr (Memr[xval], Memr[yval], NBINS, z1, z2, STARTPT, 
		    ENDPT)
		call achtrs (Memr[yval], Mems[syv], NBINS)
		call aluts (Mems[syv], Mems[syv], NBINS, Mems[LUT[cl]])
		call achtsr (Mems[syv], Memr[yval], NBINS)
	    } else {
	        call sprintf (Memc[text], SZ_LINE, 
		    "TRANSFER FUNCTION: LINEAR FROM %g TO %g")
		call pargr (z1)
		call pargr (z2)
	        call amapr (Memr[xval], Memr[yval], NBINS, z1, z2, real (dz1), 
		    real (dz2))
	    }

	    call gpline (gp, Memr[xval], Memr[yval], NBINS)
	    call ggwind (gp, wx1, wx2, wy1, wy2)
	    x = (wx2 + wx1) / 2.0
	    y = wy1 - (wy2 - wy1) * 0.20
	    call gtext (gp, x, y, Memc[text], "h=c;v=t;s=0.25")
	    call printf ("%s\n")
		call pargstr (Memc[text])

	} else {
	    # Calculate number of bits depth in output device
	    zrange = ggeti (gp, "zr")
	    for (nsig_bits = 0; ; nsig_bits = nsig_bits + 1) {
	        zrange = zrange / 2
	        if (zrange == 0)
	            break
	    }

	    # Truncate intensities to dynamic range of output device.
	    delta_inten = (IM_MAX(im) - IM_MIN(im)) / (NBINS - 1)
	    mask = 2**(nsig_bits) - 1
	    do i = 1, NBINS {
		inten =  IM_MIN(im) + ((i-1) * delta_inten)
		Memi[greys+i-1] = and (int (inten), mask)
	    }

	    call achtir (Memi[greys], Memr[real_greys], NBINS)
	    call gvline (gp, Memr[real_greys], NBINS, IM_MIN(im), IM_MAX(im))
	    call ggwind (gp, wx1, wx2, wy1, wy2)
	    x = (wx2 + wx1) / 2.0
	    y = wy1 - (wy2 - wy1) * 0.20
	    call gtext (gp, x, y, "TRANSFER FUNCTION: UNITARY","h=c;v=t;s=0.25")
	    call printf ("Unitary Transfer Function; Lowest %d bits output.\n")
		call pargi (nsig_bits)
	}

	call sfree (sp)
end
