# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>
include	<mwset.h>

# T_PROWS -- Plot the average of a range of rows from an image.

procedure t_prows()

pointer	image, wcslab, fmt
pointer	im, mw, ct, sp, x_vec, y_vec
int	row1, row2, ncols, nlines
real	zmin, zmax
int	clgeti()
pointer	immap(), mw_openim(), mw_sctran()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (wcslab, SZ_LINE, TY_CHAR)
	call salloc (fmt, SZ_LINE, TY_CHAR)

	# Open image
	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)
	call clgstr ("wcs", Memc[wcslab], SZ_LINE)
	mw = mw_openim (im)
	call mw_seti (mw, MW_USEAXMAP, NO)
	ct = mw_sctran (mw, "logical", Memc[wcslab], 0)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	row1 = clgeti ("row1")
	row2 = clgeti ("row2")
	if (min(row1,row2) < 1 || max(row1,row2) > nlines) {
	    call imunmap (im)
	    call error (2, "line index references outside image")
	}

	# Get the requested rows from the image.
	call malloc (x_vec, ncols, TY_REAL)
	call malloc (y_vec, ncols, TY_REAL)
	call plt_grows (im, mw, ct, min(row1,row2), max(row1,row2),
	    Memr[x_vec], Memr[y_vec], zmin, zmax, Memc[wcslab], Memc[fmt],
	    SZ_LINE)

	# Now draw the vector to the screen.
	call pr_draw_vector (Memc[image], Memr[x_vec], Memr[y_vec], ncols,
	    zmin, zmax, row1, row2, Memc[wcslab], Memc[fmt], true)

        # Free resources.
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)
	call imunmap (im)
	call sfree (sp)
end


# PLT_GROWS -- Get the average of specified rows from the image.  The average
# data vector is returned as y_vector; the column coordinates are returned in
# x_vector.  The data vector min and max are also returned.
 
procedure plt_grows (im, mw, ct, row1, row2, x_vector, y_vector, zmin, zmax,
	wcslab, format, sz_wcslab)
 
pointer im              # Pointer to image section header
pointer	mw		# MWCS pointer
pointer	ct		# CT pointer
int     row1            # The first row to be extracted
int     row2            # The last row to be extracted
real    x_vector[ARB]   # Data values in x direction (returned)
real    y_vector[ARB]   # Data values in y direction (returned)
real    zmin, zmax      # Minimum and maximum values in y_vector (returned)
char	wcslab[sz_wcslab]	# WCS label if present
char	format[sz_wcslab]	# WCS format if present
int	sz_wcslab	# String length
 
int     i, ncols, nrows
pointer sp, axvals, imgl2r()
 
begin
	call smark (sp)
	call salloc (axvals, IM_MAXDIM, TY_REAL)

        # Fill x and y arrays.
        ncols = IM_LEN(im,1)
        nrows = row2 - row1 + 1
 
	Memr[axvals+1] = (row1 + row2) / 2. 
	call plt_wcs (im, mw, ct, 1, Memr[axvals], 1., real(ncols), x_vector,
	    ncols, wcslab, format, sz_wcslab)
	    
        call aclrr (y_vector, ncols)
        do i = row1, row2 {
            call aaddr (Memr[imgl2r(im,i)], y_vector, y_vector, ncols)
        }
        call adivkr (y_vector, real(nrows), y_vector, ncols)
         
        # Now find min and max values in y array.
        call alimr (y_vector, ncols, zmin, zmax)

	call sfree (sp)
end


# PR_DRAW_VECTOR -- Draw the projected vector to the screen.

procedure pr_draw_vector (image,
	xvec, yvec, ncols, zmin, zmax, row1, row2, wcslab, format, prows)

char	image[SZ_FNAME]				#I image name
real	xvec[ncols], yvec[ncols]		#I vectors to be plot
int	ncols					#I number of columns
real	zmin, zmax				#I vector min max
int	row1, row2				#I selected rows
char	wcslab[ARB]				#I WCS label
char	format[ARB]				#I WCS format
bool	prows					#I is task PROWS (y/n)

pointer	sp, gp
pointer	device, marker, xlabel, ylabel, title, suffix
real	wx1, wx2, wy1, wy2, vx1, vx2, vy1, vy2, szm, tol
int	mode, imark
bool	pointmode

pointer	gopen()
real	clgetr(), plt_iformatr()
bool	clgetb(), streq()
int	btoi(), clgeti()

begin
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (marker, SZ_FNAME, TY_CHAR)
	call salloc (xlabel, SZ_LINE,  TY_CHAR)
	call salloc (ylabel, SZ_LINE,  TY_CHAR)
	call salloc (title,  SZ_LINE,  TY_CHAR)
	call salloc (suffix, SZ_FNAME, TY_CHAR)

	call clgstr ("device", Memc[device], SZ_FNAME)
	mode = NEW_FILE
	if (clgetb ("append"))
	    mode = APPEND

	gp = gopen (Memc[device], mode, STDGRAPH)
	tol = 10. * EPSILONR

	if (mode != APPEND) {
	    call clgstr ("xformat", Memc[xlabel], SZ_LINE)
	    if (!streq (Memc[xlabel], "wcsformat"))
		call strcpy (Memc[xlabel], format, SZ_FNAME)

            call clgstr ("xlabel", Memc[xlabel], SZ_LINE)
	    if (streq (Memc[xlabel], "wcslabel"))
		call strcpy (wcslab, Memc[xlabel], SZ_LINE)

            call clgstr ("title",  Memc[title],  SZ_LINE)
            if (streq (Memc[title], "imtitle")) {
                call strcpy (image, Memc[title], SZ_LINE)
                if (prows) {
		    call sprintf (Memc[suffix], SZ_FNAME, ": rows %d to %d")
                        call pargi (row1)
                        call pargi (row2)
		} else {
		    call sprintf (Memc[suffix], SZ_FNAME, ": row %d")
                        call pargi (row1)
		}
                call strcat (Memc[suffix], Memc[title], SZ_LINE)
            }

            call clgstr ("ylabel", Memc[ylabel], SZ_LINE)

	    # Establish window.
	    wx1 = plt_iformatr (clgetr ("wx1"), format)
	    wx2 = plt_iformatr (clgetr ("wx2"), format)
	    wy1 = clgetr ("wy1")
	    wy2 = clgetr ("wy2")

	    # Set window limits to defaults if not specified by user.
	    if ((wx2 - wx1) < tol) {
	        wx1 = x_vec[1]
	        wx2 = x_vec[ncols]
	    }

	    if ((wy2 - wy1) < tol) {
	        wy1 = zmin
	        wy2 = zmax
	    }

	    call gswind (gp, wx1, wx2, wy1, wy2)
    
	    # Establish viewport.
	    vx1 = clgetr ("vx1")
	    vx2 = clgetr ("vx2")
	    vy1 = clgetr ("vy1")
	    vy2 = clgetr ("vy2")

	    # Set viewport only if specified by user.
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol)
	        call gsview (gp, vx1, vx2, vy1, vy2)
	    else {
		if (!clgetb ("fill"))
		    call gseti (gp, G_ASPECT, 1)
	    }
   
	    call gsets (gp, G_XTICKFORMAT, format)
	    call gseti (gp, G_XNMAJOR, clgeti ("majrx"))
	    call gseti (gp, G_XNMINOR, clgeti ("minrx"))
	    call gseti (gp, G_YNMAJOR, clgeti ("majry"))
	    call gseti (gp, G_YNMINOR, clgeti ("minry"))

	    call gseti (gp, G_ROUND, btoi (clgetb ("round")))

	    if (clgetb ("logx"))
	        call gseti (gp, G_XTRAN, GW_LOG)
	    if (clgetb ("logy"))
	        call gseti (gp, G_YTRAN, GW_LOG)

	    # Draw axes using all this information.
	    call glabax (gp, Memc[title], Memc[xlabel], Memc[ylabel])
	}
    
	pointmode = clgetb ("pointmode")
        if (pointmode) {
            call clgstr ("marker", Memc[marker], SZ_FNAME)
            szm = clgetr ("szmarker")
            call init_marker (Memc[marker], imark)
        } else
            call clgstr ("marker", Memc[marker], SZ_FNAME)

        # Now to actually draw the plot.
        if (pointmode)
            call gpmark (gp, xvec, yvec, ncols, imark, szm, szm)
        else
            call hgpline (gp, xvec, yvec, ncols, Memc[marker])
       
	call gflush (gp)
        call gclose (gp)
	call sfree (sp)
end
