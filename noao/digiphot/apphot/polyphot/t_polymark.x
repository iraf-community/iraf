include <gset.h>
include <fset.h>
include "../lib/apphot.h"
include "../lib/polyphot.h"

define	MAX_NVERTICES	100

# T_POLYMARK -- Create a polygons list file interactively.

procedure t_polymark()

pointer	image			# pointer to name of image
pointer	polygons		# pointer to the polygons file
pointer	coords			# pointer to the coords file
pointer	display			# pointer to display device name
pointer	graphics		# pointer to the graphics device

int	limlist, lplist, lclist, stat, pl, cl, root, pid, cid
int	imlist, plist, clist
pointer	sp, cfname, pfname, im, py, id, gd

bool	streq()
int	imtlen(), clplen(), imtgetim(), clgfil(), strncmp(), strlen()
int	fnldir(), ap_mkpylist(), imtopenp(), clpopnu(), open()
pointer	gopen(), immap()
errchk	gopen()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (polygons, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (cfname, SZ_FNAME, TY_CHAR)
	call salloc (pfname, SZ_FNAME, TY_CHAR)

	# Set STDOUT.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get input and output file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	plist = clpopnu ("polygons")
	lplist = clplen (plist)
	clist = clpopnu ("coords")
	lclist = clplen (clist)

	# Check that image input and coords file list lengths match.
	if (lclist != limlist) {
	    call imtclose (imlist)
	    call clpcls (plist)
	    call clpcls (clist)
	    call error (0, "Imcompatible image and coord file list lengths")
	}

	# Check that image input and polygons file list lengths match.
	if (lplist != limlist) {
	    call imtclose (imlist)
	    call clpcls (plist)
	    call clpcls (clist)
	    call error (0, "Imcompatible image and polygons file list lengths")
	}

	# Open the graphics device.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	if (Memc[graphics] == EOS)
	    gd = NULL
	else {
	    iferr {
	        gd = gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
	    } then {
		call eprintf  ("Warning: Error opening graphics device.\n")
		gd = NULL
	    }
	}

	# Open the display device.
	call clgstr ("display", Memc[display], SZ_FNAME)
	if (Memc[display] == EOS)
	    id = NULL
	 else if (streq (Memc[display], Memc[graphics])) {
	    id = gd
	 } else {
	    iferr {
	        id = gopen (Memc[display], APPEND, STDIMAGE)
	    } then {
	        call eprintf (
		"Warning: Graphics overlay not available for display device.\n")
		id = NULL
	    }
	}

	# Open the polymark structure.
	call ap_ymkinit (py)

	# Mark polygons.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    
	    # Open image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    call apsets (py, IMNAME, Memc[image])

	    # Establish the image display viewport and window.
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Set the polygon file name.
	    if (lplist == 0)
		pl = NULL
	    else {
	        stat = clgfil (plist, Memc[polygons], SZ_FNAME)
		root = fnldir (Memc[polygons], Memc[pfname], SZ_FNAME)
		if (strncmp ("default", Memc[polygons+root], 7) == 0 || root ==
		    strlen (Memc[polygons])) {
		    call apoutname (Memc[image], Memc[pfname], "ver",
		        Memc[pfname], SZ_FNAME)
		    lplist = limlist
		} else if (stat != EOF)
		    call strcpy (Memc[polygons], Memc[pfname], SZ_FNAME)
		pl = open (Memc[pfname], NEW_FILE, TEXT_FILE)
		call close (pl)
		pl = open (Memc[pfname], READ_WRITE, TEXT_FILE)
	    }
	    call apsets (py, PYNAME, Memc[pfname])

	    # Set the coord file name.
	    if (lclist == 0)
		cl = NULL
	    else {
	        stat = clgfil (clist, Memc[coords], SZ_FNAME)
		root = fnldir (Memc[coords], Memc[cfname], SZ_FNAME)
		if (strncmp ("default", Memc[coords+root], 7) == 0 || root ==
		    strlen (Memc[coords])) {
		    call apoutname (Memc[image], Memc[cfname], "coo",
		        Memc[cfname], SZ_FNAME)
		    lclist = limlist
		} else if (stat != EOF)
		    call strcpy (Memc[coords], Memc[cfname], SZ_FNAME)
		cl = open (Memc[cfname], NEW_FILE, TEXT_FILE)
		call close (cl)
		cl = open (Memc[cfname], READ_WRITE, TEXT_FILE)
	    }
	    call apsets (py, CLNAME, Memc[cfname])

	    # Mark polygons on each image.
	    pid = 1
	    cid = 1
	    stat = ap_mkpylist (im, py, pl, cl, id, gd, pid, cid)

	    # Unmap the input image.
	    call imunmap (im)

	    # Close the polygon file.
	    if (pl != NULL) {
		call close (pl)
		if (pid <= 1)
		    call delete (Memc[pfname])
	    }

	    # Close the coordinate file.
	    if (cl != NULL) {
		call close (cl)
		if (cid <= 1)
		    call delete (Memc[cfname])
	    }

	    if (stat == YES)
		break
	}

	# Close the graphics and image display devices.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
		call gclose (gd)
	    if (id != NULL)
		call gclose (id)
	}

	# Free the polymark structure.
	call ap_ymkfree (py)

	# Close image, polygon, and coordinate lists.
	call imtclose (imlist)
	call clpcls (plist)
	call clpcls (clist)
	call sfree (sp)
end
