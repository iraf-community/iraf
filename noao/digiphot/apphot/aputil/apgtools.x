include <gset.h>
include <pkg/gtools.h>

# AP_GTINIT -- Initialize the gtools package for the apphot routines.

pointer procedure ap_gtinit (image, wx, wy)

char	image[ARB]	# the image name
real	wx, wy		# center of sky subraster

pointer	sp, gt, str
pointer	gt_init()

begin
	# Allocate working space.
    	gt = gt_init ()
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the plot title.
	call sprintf (Memc[str], SZ_LINE, "Image: %s:  %.2f %.2f\n")
	    call pargstr (image)
	    call pargr (wx)
	    call pargr (wy)
	call gt_sets (gt, GTTITLE, Memc[str])

	call sfree (sp)
	return (gt)
end


# AP_GTFREE -- Free the gtools package.

procedure ap_gtfree (gt)

pointer	gt		# pointer to gtools structure

begin
	call gt_free (gt)
end


# AP_PLOTRAD -- Plot the radial profile of a list of pixels.

procedure ap_plotrad (gd, gt, r, i, npts, polymark) 

pointer	gd		# pointer to graphics stream
pointer	gt		# the GTOOLS pointer
real	r[ARB]		# the radii array
real	i[ARB]		# the intensity array
int	npts		# number of points
char	polymark[ARB]	# polyline type

begin
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, polymark)
	call gt_plot (gd, gt, r, i, npts)
end


# AP_PLOTPTS -- Plot the radial profile of a list of pixels excluding points
# that are outside the plotting window altogether.

procedure ap_plotpts (gd, gt, r, i, npts, xmin, xmax, ymin, ymax, polymark) 

pointer	gd		# pointer to graphics stream
pointer	gt		# the GTOOLS pointer
real	r[ARB]		# the radii array
real	i[ARB]		# the intensity array
int	npts		# number of points
real	xmin, xmax	# the x plot limits
real	ymin, ymax	# the y plot limits
char	polymark[ARB]	# polyline type

int	j

begin
	call gt_sets (gt, GTTYPE, "mark")
	call gt_sets (gt, GTMARK, polymark)
	do j = 1, npts {
	    if (r[j] < xmin || r[j] > xmax)
		next
	    if (i[j] < ymin || i[j] > ymax)
		next
	    call gt_plot (gd, gt, r[j], i[j], 1)
	    
	}
end


# AP_RSET -- Set up the parameters for the radial profile plot.

procedure ap_rset (gd, gt, xmin, xmax, ymin, ymax, xscale)

pointer	gd		# pointer to GRAPHICS stream
pointer	gt		# pointer to GTOOLS structure
real	xmin, xmax	# min and max of x vector
real	ymin, ymax	# min and max of y vector
real	xscale		# image scale

pointer	sp, str, title
real	vx1, vx2, vy1, vy2, aspect
real	gstatr()

begin
	call smark (sp)
	call salloc (title, 3 * SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Reset the aspect ratio.
	aspect = gstatr (gd, G_ASPECT)
	call gsetr (gd, G_ASPECT, 0.75)

	# Construct the title.
	call sysid (Memc[title], 3 * SZ_LINE)
	call strcat ("\n", Memc[title], 3 * SZ_LINE)
	call gt_gets (gt, GTTITLE, Memc[str], SZ_LINE) 
	call strcat (Memc[str], Memc[title], 3 * SZ_LINE)
	call strcat ("\n\n", Memc[title], 3 * SZ_LINE)

	# Draw three axes.
	call gseti (gd, G_XDRAWAXES, 2)
	call gswind (gd, xmin / xscale, xmax / xscale, ymin, ymax)
	call glabax (gd, Memc[title], "", "Intensity") 

	# Draw the bottom x axis 
	call gseti (gd, G_YDRAWAXES, 0)
	call gseti (gd, G_XDRAWAXES, 1)
	call ggview (gd, vx1, vx2, vy1, vy2)
	call gsview (gd, vx1, vx2, vy1, vy2)
	call gswind (gd, xmin, xmax, ymin, ymax)
	call glabax (gd, "",
	    "Radial Distance (lower-pixels, upper-scale units)", "")

	# Reset to standard gio parameters.
	call gseti (gd, G_YDRAWAXES, 3)
	call gseti (gd, G_XDRAWAXES, 3)
	call gsetr (gd, G_ASPECT, aspect)

	call sfree (sp)
end
