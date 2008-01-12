include <gset.h>
include <fset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/polyphot.h"

define	MAX_NVERTICES	100

# T_POLYMARK -- Create a polygons list file interactively.

procedure t_polymark()

pointer	image			# pointer to name of image
pointer	polygons		# pointer to the polygons file
pointer	coords			# pointer to the coords file
int	cache			# cache the input image pixels in memory 
pointer	display			# pointer to display device name
pointer	graphics		# pointer to the graphics device

pointer	sp, cfname, pfname, im, py, id, gd, str
int	limlist, lplist, lclist, stat, pl, cl, root, pid, cid, wcs
int	imlist, plist, clist, newpy, newcoo, memstat, req_size, old_size
int	buf_size

pointer	gopen(), immap()
int	imtlen(), clplen(), imtgetim(), clgfil(), strncmp(), strlen()
int	fnldir(), ap_mkpylist(), imtopenp(), clpopnu(), open(), clgwrd()
int	access(), btoi(), ap_memstat(), sizeof()
bool	clgetb(), streq()
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
	call salloc (str, SZ_LINE, TY_CHAR)

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
	if (lclist > 0 && lclist != limlist) {
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

	cache = btoi (clgetb ("cache"))

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

        # Get the wcs information.
        wcs = clgwrd ("wcsin", Memc[str], SZ_LINE, WCSINSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the input coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call apseti (py, WCSIN, wcs)
        wcs = clgwrd ("wcsout", Memc[str], SZ_LINE, WCSOUTSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the output coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call apseti (py, WCSOUT, wcs)

	# Mark polygons.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    
	    # Open image.
	    im = immap (Memc[image], READ_ONLY, 0)
            call apimkeys (py, im, Memc[image])

	    # Establish the image display viewport and window.
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = ap_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call ap_pcache (im, INDEFI, buf_size)

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
		if (access (Memc[pfname], READ_WRITE, TEXT_FILE) == YES) {
		    newpy = NO
		    pl = open (Memc[pfname], READ_WRITE, TEXT_FILE)
		} else {
		    newpy = YES
		    pl = open (Memc[pfname], NEW_FILE, TEXT_FILE)
		    call close (pl)
		    pl = open (Memc[pfname], READ_WRITE, TEXT_FILE)
		}
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
		if (access (Memc[cfname], READ_WRITE, TEXT_FILE) == YES) {
		    newcoo = NO
		    cl = open (Memc[cfname], READ_WRITE, TEXT_FILE)
		} else {
		    newcoo = YES
		    cl = open (Memc[cfname], NEW_FILE, TEXT_FILE)
		    call close (cl)
		    cl = open (Memc[cfname], READ_WRITE, TEXT_FILE)
		}
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
		if (newpy == YES && pid <= 1)
		    call delete (Memc[pfname])
	    }

	    # Close the coordinate file.
	    if (cl != NULL) {
		if (cid > 1) {
		    call seek (cl, EOF)
		    call fprintf (cl, ";\n")
		}
		call close (cl)
		if (newcoo == YES && cid <= 1)
		    call delete (Memc[cfname])
	    }

	    # Uncache memory.
	    call fixmem (old_size)

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
