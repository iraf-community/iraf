# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>

# T_PROW --  Plot an image row.

procedure t_prow ()

bool	pointmode
pointer	im, gp, x_vec, y_vec
char	image[SZ_FNAME], device[SZ_FNAME], marker[SZ_FNAME]
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE], suffix[SZ_FNAME]
int	mode, row, ncols, imark
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
	call clputi ("row.p_maximum", IM_LEN(im, 2))
	row = clgeti ("row")
	ncols = IM_LEN(im, 1)

	call clgstr ("device", device, SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	gp = gopen (device, mode, STDGRAPH)
	call malloc (x_vec, ncols, TY_REAL)
	call malloc (y_vec, ncols, TY_REAL)

	call get_row (im, row, Memr[x_vec], Memr[y_vec], zmin, zmax)

	tol = 10.0 * EPSILONR

	if (mode != APPEND) {

	    # Establish window
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    if ((wx2 - wx1) < tol) {
	        wx1 = 1.0
	        wx2 = real (ncols)
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

	    # See if user has specified device viewport
	    if ((vx2 - vx1) > tol || (vy2 - vy1) > tol)
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
		call sprintf (suffix, SZ_FNAME, ": row %d")
		    call pargi (row)
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
            call gpmark (gp, Memr[x_vec], Memr[y_vec], ncols, imark, szm, szm)
        else
            call gpline (gp, Memr[x_vec], Memr[y_vec], ncols)
       
        # Close graphics and image
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)
        call gclose (gp)
	call imunmap (im)
end


# GET_ROW -- Get specified row from image and return data vector and
# vector of x coordinates.  ALso returned is the min and max value in 
# the data vector.

procedure get_row (im, row, x_vector, y_vector, zmin, zmax)

pointer	im		# Pointer to image section header
int	row		# The row to be extracted
real	x_vector[ARB]	# Data values in x direction (returned)
real	y_vector[ARB]	# Data values in y direction (returned)
real	zmin, zmax	# Minimum and maximum values in y_vector (returned)

int	i, ncols
pointer	imgl2r()

begin
	# Fill x and y arrays
	ncols = IM_LEN(im, 1)
	
	call amovr (Memr[imgl2r(im, row)], y_vector, ncols)
	do i = 1, ncols
	    x_vector[i] = real(i)

	# Now find min and max values in y array
	call alimr (y_vector, ncols, zmin, zmax)
end
