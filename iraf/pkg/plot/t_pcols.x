# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>
include	<mwset.h>

# T_PCOLS -- Plot the average of a range of columns from an image.

procedure t_pcols ()

pointer	image, wcslab, fmt
pointer	im, mw, ct, sp, x_vec, y_vec
int	col1, col2, ncols, nlines
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

	col1 = clgeti ("col1")
	col2 = clgeti ("col2")
	if (min(col1,col2) < 1 || max(col1,col2) > ncols) {
	    call imunmap (im)
	    call error (2, "column index references outside image")
	}

	# Get the requested columns from the image.
	call malloc (x_vec, nlines, TY_REAL)
	call malloc (y_vec, nlines, TY_REAL)
	call plt_gcols (im, mw, ct, min(col1,col2), max(col1,col2),
	    Memr[x_vec], Memr[y_vec], zmin, zmax, Memc[wcslab], Memc[fmt],
	    SZ_LINE)

	# Now draw the vector to the screen.
	call pc_draw_vector (Memc[image], Memr[x_vec], Memr[y_vec], nlines, 
	    zmin, zmax, col1, col2, Memc[wcslab], Memc[fmt], true)

	# Free resources.
	call mfree (x_vec, TY_REAL) 
	call mfree (y_vec, TY_REAL) 
	call imunmap (im) 
	call sfree (sp) 
end


# PLT_GCOLS -- Get average of specified columns from an image.  The average
# data vector is returned as y_vector; the line coordinates are returned in
# x_vector.  The data vector min and max are also returned.
 
procedure plt_gcols (im, mw, ct, col1, col2, x_vector, y_vector, zmin, zmax,
	wcslab, format, sz_wcslab)
 
pointer im              # Pointer to image header
pointer	mw		# MWCS  pointer
pointer	ct		# CT pointer
int     col1            # First column to extract
int     col2            # Last column to extract
real    x_vector[ARB]   # The row ordinal values (returned)
real    y_vector[ARB]   # The column data values (returned)
real    zmin, zmax      # Minimum and maximum data values (returned)
char	wcslab[sz_wcslab]	# WCS label if present
char	format[sz_wcslab]	# WCS format if present
int	sz_wcslab	# String length
 
int     i, nrows, ncols
pointer	sp, axvals, off, imgl2r()
real    asumr()
 
begin
	call smark (sp)
	call salloc (axvals, IM_MAXDIM, TY_REAL)

        # Fill x and y arrays.
        nrows = IM_LEN(im,2)
        ncols = col2 - col1 + 1
 
	Memr[axvals] = (col1 + col2) / 2.
	call plt_wcs (im, mw, ct, 2, Memr[axvals], 1., real(nrows), x_vector,
	    nrows, wcslab, format, sz_wcslab)

        do i = 1, nrows {
            off = imgl2r (im, i)
            y_vector[i] = asumr (Memr[off+col1-1], ncols) / real (ncols)
        }
 
        # Find min and max values in y array.
        call alimr (y_vector, nrows, zmin, zmax)

	call sfree (sp)
end


# PC_DRAW_VECTOR - Draw the projected vector to the screen.

procedure pc_draw_vector (image,
	xvec, yvec, nlines, zmin, zmax, col1, col2, wcslab, format, pcols)

char	image[SZ_FNAME]				#I Image name
real	xvec[nlines], yvec[nlines]		#I Vectors to be plot
int	nlines					#I Npts in vector
real	zmin, zmax				#I Vector min max
int	col1, col2				#I Selected columns
char	wcslab[ARB]				#I WCS label
char	format[ARB]				#I WCS format
bool	pcols					#I Is task PCOLS? (y/n)

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
		if (pcols) {
		    call sprintf (Memc[suffix], SZ_FNAME, ": columns %d to %d")
		        call pargi (col1)
		        call pargi (col2)
		} else {
		    call sprintf (Memc[suffix], SZ_FNAME, ": column %d")
		        call pargi (col1)
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
	        wx2 = x_vec[nlines]
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
	    if ((vx2 - vx1) > tol && (vy2 - vy1) > tol) {
	        call gsview (gp, vx1, vx2, vy1, vy2)
	    } else {
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
            call gpmark (gp, xvec, yvec, nlines, imark, szm, szm)
        else
            call hgpline (gp, xvec, yvec, nlines, Memc[marker])
       
	call gflush (gp)
        call gclose (gp)
	call sfree (sp)
end
