# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>

# T_PROWS --  Plot the average of a range of rows from an image.

procedure t_prows ()

bool	pointmode
pointer	im, gp, x_vec, y_vec
char	image[SZ_FNAME], device[SZ_FNAME], marker[SZ_FNAME], section[SZ_FNAME]
char	xlabel[SZ_LINE], ylabel[SZ_LINE], title[SZ_LINE], suffix[SZ_FNAME]
int	mode, row1, row2, ncols, imark
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
	call clputi ("row1.p_maximum", IM_LEN(im, 2))
	call clputi ("row2.p_maximum", IM_LEN(im, 2))
	call imunmap (im)

	row1 = clgeti ("row1")
	row2 = clgeti ("row2")
	call pr_section_name (image, row1, row2, section)
	im = immap (section, READ_ONLY, 0)
	ncols = IM_LEN(im, 1)

	call clgstr ("device", device, SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND
	gp = gopen (device, mode, STDGRAPH)

	call malloc (x_vec, ncols, TY_REAL)
	call malloc (y_vec, ncols, TY_REAL)

	call get_rows (im, Memr[x_vec], Memr[y_vec], zmin, zmax)

	tol = 10. * EPSILONR

	if (mode != APPEND) {

	    # Establish window
	    wx1 = clgetr ("wx1")
	    wx2 = clgetr ("wx2")
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    # Set window limits if not specified by the user
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

	    # Set viewport only if user has specified coordinates
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol) 
	        call gsview (gp, vx1, vx2, vy1, vy2)

	    else {
		if (!clgetb("fill"))
		    call gseti (gp, G_ASPECT, 1)
	    }
    
	    call clgstr ("xlabel", xlabel, SZ_LINE)
	    call clgstr ("ylabel", ylabel, SZ_LINE)
	    call clgstr ("title",  title,  SZ_LINE)
	    if (streq (title, "imtitle")) {
	        call strcpy (image, title, SZ_FNAME)
		call sprintf (suffix, SZ_FNAME, ": rows %d to %d")
		    call pargi (row1)
		    call pargi (row2)
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


# GET_ROWS -- Average image rows between given limits and return data
# vectors of column numbers and average pixel value.  Also returned is
# the min and max value in the data vector.

procedure get_rows (im, x_vector, y_vector, zmin, zmax)

pointer	im		# Pointer to image section header
real	x_vector[ARB]	# Data values in x direction (returned)
real	y_vector[ARB]	# Data values in y direction (returned)
real	zmin, zmax	# Minimum and maximum values in y_vector (returned)

int	i, ncols

begin

	ncols = IM_LEN(im, 1)
	call im_projection (im, y_vector, ncols, 1)

	# Now fill x_vector array and calculate min,max of y_vector
	do i = 1, ncols
	    x_vector[i] = real(i)

	call alimr (y_vector, ncols, zmin, zmax)
end


# PR_SECTION_NAME -- construct section name from image name and rows to be
# averaged..

procedure pr_section_name (image, row1, row2, section)

char	image[SZ_FNAME]		# Original image name
int	row1			# First row to average
int	row2			# Last row to average
char	section[SZ_FNAME]	# Section name to be returned

int	ip, op
int	stridxs()

begin
	# Construct name of image section to be compressed
	if (stridxs ("[", image) == 0) {
	    call sprintf (section, SZ_FNAME, "%s[*,%d:%d]") {
	        call pargstr (image)
	        call pargi (row1)
	        call pargi (row2)
	    }
	} else {
	    # More complicated - image is already an image section
	    ip = 1
	    op = 1
	    # First, copy through first comma of original image name
	    for (ip = 1; image[ip] != ',' && image[ip] != EOS; ip = ip + 1) {
		section[op] = image[ip]
		op = op + 1
	    }

	    # Append image section notation to new image name, skip comma
	    ip = ip + 1
	    call sprintf (section[op], SZ_FNAME, ",%d:%d")
		call pargi (row1)
		call pargi (row2)

	    # Find second comma or closing ']' in original image name
	    for (; image[ip] != ',' && image[ip] != ']' && image[ip] != EOS; 
		ip = ip + 1) 
		;
	    
	    # Append remainder of original name to new name
	    call strcat (image[ip], section, SZ_LINE)
	}
end
