# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>

# T_PCOL --  Plot an image column.

procedure t_pcol ()

bool	pointmode
pointer	im, gp, x_vec, y_vec
char	image[SZ_FNAME], device[SZ_FNAME], marker[SZ_FNAME], section[SZ_FNAME]
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE], suffix[SZ_FNAME]
int	mode, col, nxvals, nyvals, imark, ndim
real	zmin, zmax, szm, tol
real	wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2
pointer	immap(), gopen()
bool	clgetb(), streq()
int	clgeti(), btoi()
real	clgetr()

begin
	# Open image and graphics stream
	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	# Watch out for one dimensional images
	ndim  = IM_NDIM (im)
	if (ndim == 1)
	    call error (0, "One dimensional image - use prow")
	else
	    nxvals = IM_LEN(im, 1)
	call imunmap (im)

	call clputi ("col.p_maximum", nxvals)
	col = clgeti ("col")

	call pc_section_name (image, col, col, section)
	im = immap (section, READ_ONLY, 0)
	nyvals = IM_LEN(im, 2)

	call clgstr ("device", device, SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	gp = gopen (device, mode, STDGRAPH)

	call malloc (x_vec, nyvals, TY_REAL)
	call malloc (y_vec, nyvals, TY_REAL)

	call get_col (im, nyvals, Memr[x_vec], Memr[y_vec], zmin, zmax)

	tol = 10. * EPSILONR

	if (mode != APPEND) {

	    # Establish window
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    # If not specified by user, set window coordinates to full range
	    if ((wx2 - wx1) < tol) {
	        wx1 = 1.0
	        wx2 = real (nyvals)
	    }

	    if ((wy2 - wy1) < tol) {
	        wy1 = zmin
	        wy2 = zmax
	    }

	    call gswind (gp, wx1, wx2, wy1, wy2)
    
    
	    # Establish viewport
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")

	    # Set viewport only if specified by user
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol) 
	        call gsview (gp, vx1, vx2, vy1, vy2)

	    else {
		if (!clgetb ("fill"))
		    call gseti (gp, G_ASPECT, 1)
	    }
    
	    call clgstr ("xlabel", xlabel, SZ_LINE)
	    call clgstr ("ylabel", ylabel, SZ_LINE)
	    call clgstr ("title",  title,  SZ_LINE)
	    if (streq (title, "imtitle")) {
	        call strcpy (image, title, SZ_FNAME)
		call sprintf (suffix, SZ_FNAME, ": column %d")
		    call pargi (col)
	        call strcat (suffix, title, SZ_FNAME)
	    }
    
	    call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gp, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gp, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gp, G_YNMINOR, clgeti ("minry"))

	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))

	    if (clgetb ("logx"))
	        call gseti (gp, G_XTRAN, GW_LOG)
	    if (clgetb ("logy"))
	        call gseti (gp, G_YTRAN, GW_LOG)

	    # Draw axes using all this information
	    call glabax (gp, title, xlabel, ylabel)
	}
    
	pointmode = clgetb ("pointmode")
        if (pointmode) {
            call clgstr ("marker", marker, SZ_FNAME)
            szm= clgetr ("szmarker")
            call init_marker (marker, imark)
        }

        # Now to actually draw the plot
        if (pointmode)
            call gpmark (gp, Memr[x_vec], Memr[y_vec], nyvals, imark, szm, szm)
        else
            call gpline (gp, Memr[x_vec], Memr[y_vec], nyvals)
       
        # Close graphics and image
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)
        call gclose (gp)
	call imunmap (im)
end


# GET_COL -- Get column from image.  Return two vectors, one of the
# data values, the other of column ordinal.  Also, the min and max of
# the data vector is calculated and returned.

procedure get_col (im, nvals, x_vector, y_vector, zmin, zmax)

pointer	im		# Pointer to image header
int	nvals		# The number of elements in data vector
real	x_vector[ARB]	# The column values (returned)
real	y_vector[ARB]	# The column ordinal values (returned)
real	zmin, zmax	# Minimum and maximum data values (returned)

int	i

begin
	# Fill x and y arrays. 

	do i = 1, nvals
	    x_vector[i] = real(i)

	call im_projection (im, y_vector, nvals, 1)

	# Find min and max values in y array
	call alimr (y_vector, nvals, zmin, zmax)
end
