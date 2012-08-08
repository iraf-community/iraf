include <gset.h>
include <fset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/polyphot.h"

# T_POLYPHOT -- Measure the total magnitudes inside a list of polygons.

procedure t_polyphot()

pointer	image			# pointer to name of image
pointer	output			# pointer to the results file
pointer	coords			# pointer to file of coordinates
pointer	polygon			# pointer to file containing polygon
pointer	graphics		# pointer to graphics device name
pointer	display			# pointer to display device name
int	interactive		# mode of use
int	cache			# cache the input image pixels
int	verify			# verify critical parameters
int	update			# update the critical parameters
int	verbose			# print messages

pointer	sp, outfname, cname, im, py, id, gd, str
int	limlist, lplist, lolist, lclist, sid, lid, pid, pl, cl, out, root, stat
int	imlist, plist, olist, clist, memstat, wcs, req_size, old_size, buf_size

pointer	immap(), gopen()
int	imtlen(), imtgetim(), clplen(), clgfil(), btoi(), strncmp()
int	fnldir(), strlen(), ap_yphot(), open(), imtopenp(), clpopnu()
int	clgwrd(), ap_memstat(), sizeof()
bool	clgetb(), streq()
errchk	gopen

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (polygon, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set STDOUT.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get input and output file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	plist = clpopnu ("polygons")
	lplist = clplen (plist)
	olist = clpopnu ("output")
	lolist = clplen (olist)
	clist = clpopnu ("coords")
	lclist = clplen (clist)

	# Check that image and polygon list lengths match.
	if (limlist < 1 || (lplist > 1 && lplist != limlist)) {
	    call imtclose (imlist)
	    call clpcls (plist)
	    call clpcls (olist)
	    call clpcls (clist)
	    call error (0, "Imcompatible image and polygon list lengths")
	}

	# Check that image and coordinates list lengths match. 
	if (limlist < 1 || (lclist > 1 && lclist != limlist)) {
	    call imtclose (imlist)
	    call clpcls (plist)
	    call clpcls (olist)
	    call clpcls (clist)
	    call error (0, "Imcompatible image and coordinate list lengths")
	}

	# Check that image input and output list lengths match.
	if (lolist > 1 && lolist != limlist) {
	    call imtclose (imlist)
	    call clpcls (plist)
	    call clpcls (olist)
	    call clpcls (clist)
	    call error (0, "Imcompatible image and output list lengths")
	}

	call clgstr ("icommands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	else
	    interactive = btoi (clgetb ("interactive"))
	cache = btoi (clgetb ("cache"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))

	# Get polygon fitting parameters.
	call ap_gypars (py)

	# Confirm the algorithm parameters.
	if (verify == YES && interactive == NO) {
	    call ap_yconfirm (py, NULL, 1)
	    if (update == YES)
		call ap_pypars (py)
	}

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

	# Get the graphics and display devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)

	# Open the plot files.
	if (interactive == YES) {
	    if (Memc[graphics] == EOS)
		gd = NULL
	    else {
		iferr {
		    gd = gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
		} then {
		    call eprintf (
			"Warning: Error opening graphics device.\n")
		    gd = NULL
		}
	    }
	    if (Memc[display] == EOS)
		id = NULL
	    else if (streq (Memc[graphics], Memc[display]))
		id = gd
	    else {
		iferr {
		    id = gopen (Memc[display], APPEND, STDIMAGE)
		} then {
		    call eprintf (
		"Warning: Graphics overlay not available for display device.\n")
		    id = NULL
		}
	    }
	} else {
	    gd = NULL
	    id = NULL
	}

	# Measure flux in a polygon.
	sid = 1
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    
	    # Open image.
	    im = immap (Memc[image], READ_ONLY, 0)
	    call apimkeys (py, im, Memc[image])
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = ap_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call ap_pcache (im, INDEFI, buf_size)

	    # Open the polygons file.
	    if (lplist <= 0) {
		pl = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
		stat = clgfil (plist, Memc[polygon], SZ_FNAME)
		root = fnldir (Memc[polygon], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[polygon+root], 7) == 0 || root ==
		    strlen (Memc[polygon])) {
		    call ap_inname (Memc[image], Memc[outfname], "ver",
		        Memc[outfname], SZ_FNAME)
		    lplist = limlist
		    pl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else if (stat != EOF) {
		    call strcpy (Memc[polygon], Memc[outfname], SZ_FNAME)
		    pl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else {
		    call apstats (py, PYNAME, Memc[outfname], SZ_FNAME)
		    call seek (pl, BOF)
		}
	    }
	    call apsets (py, PYNAME, Memc[outfname])
	    call apfroot (Memc[outfname], Memc[str], SZ_LINE)
	    call apsets (py, PYROOT, Memc[str])

	    # Open the coordinates file.
	    if (lclist <= 0) {
		cl = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
		stat = clgfil (clist, Memc[coords], SZ_FNAME)
		root = fnldir (Memc[coords], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[coords+root], 7) == 0 || root ==
		    strlen (Memc[coords])) {
		    call ap_inname (Memc[image], Memc[outfname], "coo",
		        Memc[outfname], SZ_FNAME)
		    lclist = limlist
	            cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else if (stat != EOF) {
		    call strcpy (Memc[coords], Memc[outfname], SZ_FNAME)
	            cl = open (Memc[outfname], READ_ONLY, TEXT_FILE)
		} else {
		    call apstats (py, CLNAME, Memc[outfname], SZ_FNAME)
		    call seek (cl, BOF)
		}
	    }
	    call apsets (py, CLNAME, Memc[outfname])
	    call apfroot (Memc[outfname], Memc[str], SZ_LINE)
	    call apsets (py, CLROOT, Memc[str])

	    # Set output file name.
	    if (lolist == 0) {
		out = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
		    call apoutname (Memc[image], Memc[outfname], "ply",
		        Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		} else
		    call apstats (py, OUTNAME, Memc[outfname], SZ_FNAME)
	    }
	    call apsets (py, OUTNAME, Memc[outfname])

	    # Do the photometry.
	    if (interactive == NO) {
		if (Memc[cname] != EOS)
		    stat = ap_yphot (py, im, cl, pl, NULL, NULL, out, sid, NO,
			cache)
		else if (pl != NULL) {
		    lid = 0
		    pid = 0
		    call ap_ybphot (py, im, cl, pl, out, sid, lid, pid, NULL,
		        verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
		stat = ap_yphot (py, im, cl, pl, gd, id, out, sid, YES, cache)

	    # Unmap the input image.
	    call imunmap (im)

	    # Close the polygon file.
	    if (pl != NULL) {
		if (lplist > 1)
		    call close (pl)
	    }

	    # Close the coordinate file.
	    if (cl != NULL) {
		if (lclist > 1)
		    call close (cl)
	    }

	    # Close the output file.
	    if (out != NULL && lolist != 1) {
		call close (out)
		if (sid <= 1) {
		    call apstats (py, OUTNAME, Memc[outfname], SZ_FNAME)
		    call delete (Memc[outfname])
		}
		sid = 1
	    }

	    # Uncache memory.
	    call fixmem (old_size)

	    if (stat == YES)
		break
	}

	# Close plot files.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
		call gclose (gd)
	   if (id != NULL)
	        call gclose (id)
	}

	# Close the single coords and polygon file.
	if (pl != NULL && lplist == 1)
	    call close (pl)
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# Close the singled output file.
        if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1) {
		call apstats (py, OUTNAME, Memc[outfname], SZ_FNAME)
		call delete (Memc[outfname])
	    }
	}

	# Close up the files.
	call ap_yfree (py)

	# Close image, coord and shift lists.
	call imtclose (imlist)
	call clpcls (plist)
	call clpcls (clist)
	call clpcls (olist)

	call sfree (sp)
end
