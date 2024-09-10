include <gset.h>
include <imhdr.h>

# T_WCSLAB -- Procedure to draw labels and grids in sky projection coordinates.
#
# Description
#  T_wcslab produces a labelling and grid based on the MWCS of a 
#  specified image.  This is the task interface to the programmer interface
#  wcslab.  See wcslab.x for more information.
#
# Bugs 
#  Can only handle sky projections for Right Ascension/Declination.   This
#  should be able to deal with any of the projections for this system, but
#  has only been tested with the Tangent projection.
#

procedure t_wcslab()

pointer image           # I: name of the image
int	frame		# I: display frame containing the image
bool	do_fill         # I: true if the graph fills the specified viewport
int	mode            # I: the graphics stream mode 
pointer	device          # I: the name of the graphics device
real	vl, vr, vb, vt  # I: the edges of the graphics viewport

pointer	sp, title, gp, im, mw
real	c1, c2, l1, l2
bool	clgetb()
int	clgeti(), strncmp()
pointer	gopen(), immap(), mw_openim()
real	clgetr()

begin
	# Get memory.
	call smark (sp)
	call salloc (device, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Since all the MWCS information comes from an image open it.
	call clgstr ("image", Memc[image], SZ_FNAME)

	if (Memc[image] != EOS) {

	    # Open the image.
	    im = immap (Memc[image], READ_ONLY, 0)

	    # Quit if the image is not 2-dimensional.
	    if (IM_NDIM(im) != 2) {
	        call eprintf ("Image: %s is not 2-dimensional\n")
		    call pargstr (Memc[image])
	        call sfree (sp)
	        return
	    }

	    # Set the default input image column and line limits.
	    c1 = 1.0
	    c2 = real (IM_LEN(im,1))
	    l1 = 1.0
	    l2 = real (IM_LEN(im,2))

	    # Open the WCS.
	    mw = mw_openim (im)

	    # Set up the default image title.
	    call strcpy (Memc[image], Memc[title], SZ_LINE)
	    call strcat (": ", Memc[title], SZ_LINE)
	    call strcat (IM_TITLE(im), Memc[title], SZ_LINE)

	} else {

	    # Set the image information to undefined. All this will
	    # be determined in wcslab.
	    Memc[title] = EOS
	    im = NULL
	    mw = NULL
	    c1 = 0.0
	    c2 = 1.0
	    l1 = 0.0
	    l2 = 1.0
	}

	# Set the graphics mode depending on whether we are appending to a plot
	# or starting a new plot.
	do_fill = clgetb ("fill")
	if (clgetb ("append")) 
	    mode = APPEND
	else
	    mode = NEW_FILE

	# Open graphics.
	call clgstr ("device", Memc[device], SZ_FNAME)

	# If we are appending, get the previous viewing parameters.
	if (mode == APPEND) {

	    gp = gopen (Memc[device], APPEND, STDGRAPH)
	    call ggview (gp, vl, vr, vb, vt)
	    do_fill = true

	# If drawing on the image display device try to match viewports.
	} else if (strncmp (Memc[device], "imd", 3) == 0) {

	    frame = clgeti ("frame")
	    vl = clgetr ("vl")
	    vr = clgetr ("vr")
	    vb = clgetr ("vb")
	    vt = clgetr ("vt")
	    call wl_imd_viewport (frame, im, c1, c2, l1, l2, vl, vr, vb, vt)
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)

	# Otherwise set up a standard viewport.
	} else {
	    vl = clgetr ("vl")
	    vr = clgetr ("vr")
	    vb = clgetr ("vb")
	    vt = clgetr ("vt")
	    gp = gopen (Memc[device], NEW_FILE, STDGRAPH)
	}

	# Set the viewport.
	call gseti (gp, G_WCS, 1)
	call wl_map_viewport (gp, c1, c2, l1, l2, vl, vr, vb, vt, do_fill)

	# All reading from CL parameters is now done.  Everything necessary to
	# do the plotting is in the WCSLAB descriptor.  Do it.
	call wcslab (mw, c1, c2, l1, l2, gp, Memc[title])

	# Release the memory.
	call gclose (gp) 
	if (mw != NULL)
	    call mw_close (mw)
	if (im != NULL)
	    call imunmap (im) 
	call sfree (sp)
end
