include <fset.h>
include <gset.h>
include <imhdr.h>
include "../lib/apphot.h"

# T_RADPROF -- Procedure to compute the radial profiles of a list of
# objects.

procedure t_radprof ()

pointer image		# pointer to the name of the image
pointer output		# pointer to the output file name
pointer coords		# pointer to the coordinate file
pointer plotfile	# pointer to the metacode plot file
pointer graphics	# pointer to the graphics device
pointer display		# pointer to the display device
int	interactive	# interactive mode
int	cache		# cache the input image
int	verify		# verify the critical parameters
int	update		# update the critical parameters
int	verbose		# verbose mode

pointer	sp, cname, outfname, ap, im, id, gd, mgd, str
int	limlist, lclist, lolist, sid, lid, cl, out, pfd, root, stat
int	imlist, clist, olist, wcs, memstat, req_size, old_size, buf_size

pointer	immap(), gopen()
int	imtlen(), imtgetim(), clplen(), clgfil(), btoi(), fnldir(), strncmp()
int	strlen(), ap_radprof(), imtopenp(), clpopnu(), open(), clgwrd()
int	ap_memstat(), sizeof()
bool	clgetb(), streq()
errchk	gopen

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (plotfile, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Set standard output to flush on newline.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Get input and output file names.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	clist = clpopnu ("coords") 
	lclist = clplen (clist)
	olist = clpopnu ("output")
	lolist = clplen (olist)

	# Check that the image and coordinate list lengths match.
	if (limlist < 1 || (lclist > 1 && lclist != limlist)) {
	    call imtclose (imlist)
	    call clpcls (clist)
	    call clpcls (olist)
	    call error (0, "Imcompatible image and coordinate list lengths")
	}

	# Check that image and output list lengths match.
	if (lolist > 1 && lolist != limlist) {
	    call imtclose (imlist)
	    call clpcls (clist)
	    call clpcls (olist)
	    call error (0, "Imcompatible image and output list lengths")
	}

	call clgstr ("icommands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	#else if (lclist == 0)
	    #interactive = YES
	else
	    interactive = btoi (clgetb ("interactive"))
	cache = btoi (clgetb ("cache"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))

	# Get the radial profile fitting parameters.
	call ap_grppars (ap)

	# Verify the radial profile fitting parameters.
	if (verify == YES && interactive == NO) {
	    call ap_rconfirm (ap, NULL, 1)
	    if (update == YES)
		call ap_rpars (ap)
	}

        # Get the wcs information.
        wcs = clgwrd ("wcsin", Memc[str], SZ_LINE, WCSINSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the input coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call apseti (ap, WCSIN, wcs)
        wcs = clgwrd ("wcsout", Memc[str], SZ_LINE, WCSOUTSTR)
        if (wcs <= 0) {
            call eprintf (
                "Warning: Setting the output coordinate system to logical\n")
            wcs = WCS_LOGICAL
        }
        call apseti (ap, WCSOUT, wcs)

	# Get the graphics and display devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)

	# Open the plot files.
	if (interactive == YES) {
	    if (Memc[graphics] == EOS)
		gd = NULL
	    else {
		iferr {
		    gd =  gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
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

	# Open the plot metacode file.
	call clgstr ("plotfile", Memc[plotfile], SZ_FNAME)
	if (Memc[plotfile] == EOS)
	    pfd = NULL
	else
	    pfd = open (Memc[plotfile], APPEND, BINARY_FILE)
	if (pfd != NULL)
	    mgd = gopen (Memc[graphics], NEW_FILE, pfd)
	else
	    mgd = NULL

	# Begin looping over the image list.
	sid = 1
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image.
	    im = immap (Memc[image], READ_ONLY, 0)

	    # Set the image display viewport.
	    call apimkeys (ap, im, Memc[image])
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Cache the input image pixels.
            req_size = MEMFUDGE * IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im))
            memstat = ap_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call ap_pcache (im, INDEFI, buf_size)

	    # Open the coordinate file, where coords is assumed to be a simple
	    # text file in which the x and y positions are in columns 1 and 2
	    # respectively and all remaining fields are ignored.

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
		    call apstats (ap, CLNAME, Memc[outfname], SZ_FNAME)
		    call seek (cl, BOF)
		}
	    }
	    call apsets (ap, CLNAME, Memc[outfname])
	    call apfroot (Memc[outfname], Memc[str], SZ_FNAME)
	    call apsets (ap, CLROOT, Memc[str])

	    # Open output text file, if output is "default", dir$default,
	    # or a directory specification then the extension "prf" is added
	    # to the image name and a suitable version number is appended to
	    # the output name. If the output name is null then no output
	    # file is created.

	    if (lolist == 0) {
		out = NULL
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
	            call apoutname (Memc[image], Memc[outfname], "prf",
		        Memc[outfname], SZ_FNAME)
	            out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		} else
		    call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])

	    # Fit the radial profiles.
	    if (interactive == NO) {
	        if (Memc[cname] != EOS)
		    stat = ap_radprof (ap, im, cl, NULL, mgd, NULL, out,
		        sid, NO, cache)
	        else  if (cl != NULL) {
		    lid = 1
	            call ap_bradprof (ap, im, cl, id, gd, mgd, out, sid, lid,
		        verbose)
		    stat = NO
		} else
		    stat = NO
	    } else
	        stat = ap_radprof (ap, im, cl, gd, mgd, id, out, sid, YES,
		    cache)

	    # Cleanup.
	    call imunmap (im)
	    if (cl != NULL) {
		if (lclist > 1)
	            call close (cl)
	    }
	    if (out != NULL && lolist != 1) {
	        call close (out)
		if (sid <= 1) {
		    call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
		    call delete (Memc[outfname])
		}
		sid = 1
	    }

	    # Uncache memory.
	    call fixmem (old_size)

	    if (stat == YES)
		break

	}

	# If only one coordinate file for a list of images close list.
	if (cl != NULL && lclist == 1)
	    call close (cl)

	# If only one output file for a list of images close list.
	if (out != NULL && lolist == 1) {
	    call close (out)
	    if (sid <= 1) {
		call apstats (ap, OUTNAME, Memc[outfname], SZ_FNAME)
		call delete (Memc[outfname])
	    }
	}

	# Close up plot files.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
	        call gclose (gd)
	    if (id != NULL)
	        call gclose (id)
	}
	if (mgd != NULL)
	    call gclose (mgd)
	if (pfd != NULL)
	    call close (pfd)

	# Free the radial profile fitting structure.
	call ap_rpfree (ap)

	# Close up the lists.
	call imtclose (imlist)
	call clpcls (clist)
	call clpcls (olist)

	call sfree (sp)
end
