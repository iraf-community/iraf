include	<fset.h>
include	<error.h>
include "pexamine.h"

# T_PEXAMINE -- Interactively examine and edit APPHOT and DAOPHOT output.

procedure t_pexamine()

pointer	input		# pointer to the name of the catalog
pointer	output		# pointer to the name of the edited catalog
pointer	xcolumn		# pointer to the name of the X column
pointer	ycolumn		# pointer to the name of the Y column
pointer	xposcolumn	# pointer to the name of the X coord column
pointer	yposcolumn	# pointer to the name of the Y coord column
pointer	hcolumn		# pointer to the name of the histogram column
pointer	photcolumns	# pointer to the photometry columns
pointer	usercolumns	# pointer to the user columns
pointer	graphics	# pointer to the name of the graphics device
pointer	image		# pointer to the name of the input image
pointer	reject 		# pointer to the name of the deletions catalog
real	match_radius	# the matching radius

int	numrows, max_nstars, first_star, status
int	apd, apout, aprej, use_display
pointer	sp, key, px, gd, im, deleted

bool	clgetb()
int	fstati(), access(), clgeti(), pt_getphot(), pt_plot(), open(), btoi()
pointer	gopen(), tbtopn(), pt_init(), immap()
real	clgetr()

begin
	# Flush on a newline if the standard output is not redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get some working memory.
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (reject, SZ_FNAME, TY_CHAR)
	call salloc (photcolumns, PX_SZCOLNAME * (PX_MAXNCOLS + 1), TY_CHAR)
	call salloc (xcolumn, PX_SZCOLNAME, TY_CHAR)
	call salloc (ycolumn, PX_SZCOLNAME, TY_CHAR)
	call salloc (hcolumn, PX_SZCOLNAME, TY_CHAR)
	call salloc (xposcolumn, PX_SZCOLNAME, TY_CHAR)
	call salloc (yposcolumn, PX_SZCOLNAME, TY_CHAR)
	call salloc (usercolumns, PX_SZCOLNAME * (PX_MAXNCOLS + 1), TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)

	# Fetch the input and output file parameters and the column
	# definition parameters.
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)
	call clgstr ("image", Memc[image], SZ_FNAME)
	call clgstr ("deletions", Memc[reject], SZ_FNAME)
	call clgstr ("photcolumns", Memc[photcolumns], PX_SZCOLNAME * 
	    (PX_MAXNCOLS + 1))
	call clgstr ("xcolumn", Memc[xcolumn], PX_SZCOLNAME)
	call clgstr ("ycolumn", Memc[ycolumn], PX_SZCOLNAME)
	call clgstr ("hcolumn", Memc[hcolumn], PX_SZCOLNAME)
	call clgstr ("xposcolumn", Memc[xposcolumn], PX_SZCOLNAME)
	call clgstr ("yposcolumn", Memc[yposcolumn], PX_SZCOLNAME)
	call clgstr ("usercolumns", Memc[usercolumns], PX_SZCOLNAME *
	    (PX_MAXNCOLS + 1))

	match_radius = clgetr ("match_radius")
	max_nstars = clgeti ("max_nstars")
	first_star = clgeti ("first_star")

	# Get the graphics and display parameters.
	call clgstr ("icommands.p_filename", Memc[graphics], SZ_FNAME)
	if (Memc[graphics] != EOS)
	    use_display = YES
	else
	    use_display = btoi (clgetb ("use_display"))
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)

	# Initialize the pexamine struture.
	px = pt_init (Memc[photcolumns], Memc[usercolumns], Memc[xcolumn],
	    Memc[ycolumn], Memc[xposcolumn], Memc[yposcolumn], Memc[hcolumn])
	
	# Open the input catalog.
	if (access (Memc[input], 0, TEXT_FILE) == YES) {
	    apd = open (Memc[input], READ_ONLY, TEXT_FILE)
	    call pt_kyinit (key)
	} else {
	    apd = tbtopn (Memc[input], READ_ONLY, 0)
	    key = NULL
	}

	# Open the input image.
	if (Memc[image] == EOS)
	    im = NULL
	else
	    im = immap (Memc[image], READ_ONLY, 0)

	# Allocate buffer space.
	iferr {
	    numrows = min (max_nstars, pt_getphot (px, apd, key, max_nstars,
		first_star))
	    call malloc (deleted, numrows, TY_INT)
	} then
	    call erract (EA_FATAL)

	# Plot the data and enter the interactive cursor loop.
	gd = gopen (Memc[graphics], NEW_FILE, STDGRAPH)
	status = pt_plot (gd, px, apd, key, im, Memi[deleted], numrows,
	    max_nstars, first_star, match_radius, use_display)
	call gclose (gd)

	if (status == PX_EXIT) {

	    # Open the output file.
	    if (Memc[output] != EOS) {
	        if (access (Memc[output], 0, 0) == YES) {
		    call printf ("The catalog %s already exists\n")
			call pargstr (Memc[output])
		    call mktemp ("out", Memc[output], SZ_FNAME)
		    call printf ("The new output catalog is %s\n")
			call pargstr (Memc[output])
	        }
	        if (key == NULL)
	            apout = tbtopn (Memc[output], NEW_COPY, apd)
	        else
		    apout = open (Memc[output], NEW_FILE, TEXT_FILE)
	    } else
		apout = NULL

	    # Open a reject points catalog if required.
	    if (Memc[reject] != EOS) {
	        if (access (Memc[reject], 0, 0) == YES) {
		    call printf ("The catalog %s already exists.\n")
			call pargstr (Memc[reject])
		    call mktemp ("rej", Memc[reject], SZ_FNAME)
		    call printf ("The new rejections catalog is %s\n")
			call pargstr (Memc[reject])
	        }
	        if (key == NULL)
	            aprej = tbtopn (Memc[reject], NEW_COPY, apd)
	        else
		    aprej = open (Memc[reject], NEW_FILE, TEXT_FILE)
	    } else
		aprej = NULL

	    # Write the output catalog file.
	    call pt_wtfile (apd, key, apout, aprej, Memi[deleted], numrows) 

	    # Close the output file.
	    if (apout != NULL) {
	        if (key == NULL)
	            call tbtclo (apout)
 	        else
	            call close (apout)
	    }

	    # Close the rejected points file.
	    if (aprej != NULL) {
	        if (key == NULL)
	            call tbtclo (aprej)
	        else
		    call close (aprej)
	    }

	} else if (status == ERR)
	    call fseti (STDOUT, F_CANCEL, OK)

	# Close the input file.
	if (key != NULL) {
	    call pt_kyfree (key)
	    call close (apd)
	} else if (apd != NULL)
	    call tbtclo (apd)

	# Close the image.
	if (im != NULL)
	    call imunmap(im)

	# Return the buffer space.
	call mfree (deleted, TY_INT)

	# Free the program structures.
	call pt_free (px)
	call sfree (sp)
	if (status == ERR)
	    call erract (EA_ERROR)
end
