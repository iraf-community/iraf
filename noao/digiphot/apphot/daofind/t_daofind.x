include <imhdr.h>
include <gset.h>
include <fset.h>
include <imhdr.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

# T_DAOFIND --  Automatically detect objects in an image given the full
# width half maximum of the image point spread function and a detection
# threshold.

procedure t_daofind ()

pointer	image			# pointer to input image
pointer denimage		# pointer to density enhancement image
pointer	skyimage		# pointer to the sky image
int	output			# the results file descriptor
int	boundary		# type of boundary extension
real	constant		# constant for constant boundary extension
int	interactive		# interactive mode
int	verify			# verify mode
int	update			# update critical parameters
int	verbose			# verbose mode
int	cache			# cache the image pixels

pointer	im, cnv, sky, sp, outfname, denname, skyname, str
pointer	ap, cname, display, graphics, id, gd
int	limlist, lolist, densave, skysave, out, root, stat, imlist, olist
int	wcs, req_size, old_size, buf_size, memstat

real	clgetr()
pointer	gopen(), immap(), ap_immap()
int	imtlen(), clplen(), btoi(), clgwrd(), aptmpimage()
int	open(), strncmp(), strlen(), fnldir(), ap_fdfind()
int	imtopenp(), clpopnu(), clgfil(), imtgetim(), ap_memstat(), sizeof()
bool	clgetb(), streq()

begin
	# Flush STDOUT on a new line.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (denimage, SZ_FNAME, TY_CHAR)
	call salloc (skyimage, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)

	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (denname, SZ_FNAME, TY_CHAR)
	call salloc (skyname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Fetch the image and output file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	olist = clpopnu ("output")
	lolist = clplen (olist)

	# Get prefix for density enhancement and sky images.
	call clgstr ("starmap", Memc[denimage], SZ_FNAME)
	call clgstr ("skymap", Memc[skyimage], SZ_FNAME)

	# Get the image boundary extensions parameters.
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	call clgstr ("icommands.p_filename", Memc[cname], SZ_FNAME)
	if (Memc[cname] != EOS)
	    interactive = NO
	else
	    interactive = btoi (clgetb ("interactive"))
	verbose = btoi (clgetb ("verbose"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	cache = btoi (clgetb ("cache"))

	# Get the parameters.
	call ap_fdgpars (ap)

	# Confirm the algorithm parameters.
	if (verify == YES && interactive == NO)  {
	    call ap_fdconfirm (ap)
	    if (update == YES)
		call ap_fdpars (ap)
	}

	# Get the wcs info.
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

	# Open the graphics and display devices.
	if (interactive == YES) {
	    if (Memc[graphics] == EOS)
		gd = NULL
	    else {
		iferr {
		    gd =  gopen (Memc[graphics], APPEND+AW_DEFER, STDGRAPH)
		} then {
		    call eprintf (
			"Warning: Error opening the graphics device.\n")
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

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image and get the required keywords from the header.

	    im = immap (Memc[image], READ_ONLY, 0)
	    call apimkeys (ap, im, Memc[image])

	    # Set the image display viewport.
	    if ((id != NULL) && (id != gd))
		call ap_gswv (id, Memc[image], im, 4)

	    # Cache the input image pixels.
            req_size = MEMFUDGE * (IM_LEN(im,1) * IM_LEN(im,2) *
                sizeof (IM_PIXTYPE(im)) + 2 * IM_LEN(im,1) * IM_LEN(im,2) *
		sizeof (TY_REAL))
            memstat = ap_memstat (cache, req_size, old_size)
            if (memstat == YES)
                call ap_pcache (im, INDEFI, buf_size)

	    # Determine the results file name. If output is a null string or
	    # a directory name then the extension "coo" is added to the
	    # root image name and the appropriate version number is appended
	    # in order to construct a default output file name.

	    out = NULL
	    if (lolist == 0) {
		call strcpy ("", Memc[outfname], SZ_FNAME)
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
	            call apoutname (Memc[image], Memc[outfname], "coo",
		        Memc[outfname], SZ_FNAME)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		} else {
	            call apoutname (Memc[image], Memc[outfname], "coo",
		        Memc[outfname], SZ_FNAME)
		    lolist = limlist
		}
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])

	    # Set up the directory and name for the density enhancement image.
	    # Note that the default value for denimage is the null string
	    # indicating the current directory. If denimage is null or
	    # contains only a directory specification make a temporary image
	    # name using the prefix "den", otherwise use the user specified
	    # prefix.

	    densave = aptmpimage (Memc[image], Memc[denimage], "den",
	        Memc[denname], SZ_FNAME)

	    # Set up the directory and name for the sky values image. 

	    skysave = aptmpimage (Memc[image], Memc[skyimage], "sky",
	        Memc[skyname], SZ_FNAME)

	    # Find the stars in an image.
	    if (interactive == NO) {

		cnv = NULL
		if (Memc[cname] != EOS) {
		    stat = ap_fdfind (Memc[denname], Memc[skyname], ap, im,
		        NULL, NULL, out, boundary, constant, densave,
			skysave, NO, cache)
		} else {
	            cnv = ap_immap (Memc[denname], im, ap, densave)
                    if (memstat == YES)
                	call ap_pcache (cnv, INDEFI, buf_size)
		    if (skysave == YES) {
			sky = ap_immap (Memc[skyname], im, ap, skysave)
                        if (memstat == YES)
                	    call ap_pcache (sky, INDEFI, buf_size)
		    } else
			sky = NULL
		    if (Memc[outfname] != EOS)
		        out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
			out = NULL
		    call ap_bfdfind (im, cnv, sky, out, ap, boundary,
		        constant, verbose)
	            call imunmap (cnv)
		    if (sky != NULL)
			call imunmap (sky)
	            if (densave == NO)
	                call imdelete (Memc[denname])
		    stat = NO
		}

	    } else
		stat = ap_fdfind (Memc[denname], Memc[skyname], ap, im, gd,
		    id, out, boundary, constant, densave, skysave, YES,
		    cache)

	    # Clean up files.
	    call imunmap (im)
	    call close (out)

	    # Uncache memory
	    call fixmem (old_size)

	    if (stat == YES)
		break
	}

	# Close up the graphics system.
	if (id == gd && id != NULL)
	    call gclose (id)
	else {
	    if (gd != NULL)
	        call gclose (gd)
	    if (id != NULL)
	        call gclose (id)
	}

	# Close image and output file lists.
	call ap_fdfree (ap)
	call imtclose (imlist)
	call clpcls (olist)
	call sfree (sp)
end


# AP_IMMAP -- Map the output image.

pointer procedure ap_immap (outname, im, ap, save)

char	outname[ARB]		# convolved image name
pointer	im			# pointer to input image
pointer	ap			# pointer to the apphot structure
int	save			# save the convolved image

int	newimage
pointer	outim
real	tmp_scale, tmp_fwhmpsf, tmp_ratio, tmp_theta, tmp_nsigma
real	tmp_datamin, tmp_datamax
bool	fp_equalr()
int	imaccess()
pointer	immap()
real	apstatr(), imgetr()
errchk	imgetr()

begin
	# Check to see if the image already exists. If it does not
	# open a new image and write the PSF characteristics into the
	# image user area, otherwise fetch the PSF parameters from the image
	# header.

	if (save == NO || imaccess (outname, READ_ONLY) == NO) {

	    outim = immap (outname, NEW_COPY, im)
	    IM_PIXTYPE(outim) = TY_REAL
	    call imaddr (outim, "SCALE", 1.0 / apstatr (ap, SCALE))
	    call imaddr (outim, "FWHMPSF", apstatr (ap, FWHMPSF))
	    call imaddr (outim, "NSIGMA", apstatr (ap, NSIGMA))
	    call imaddr (outim, "RATIO", apstatr (ap, RATIO))
	    call imaddr (outim, "THETA", apstatr (ap, THETA))
	    call imaddr (outim, "DATAMIN", apstatr (ap, DATAMIN))
	    call imaddr (outim, "DATAMAX", apstatr (ap, DATAMIN))

	} else {

	    outim = immap (outname, READ_ONLY, 0)
	    iferr (tmp_scale = 1.0 / imgetr (outim, "SCALE"))
		tmp_scale = INDEFR
	    iferr (tmp_fwhmpsf = imgetr (outim, "FWHMPSF"))
		tmp_fwhmpsf = INDEFR
	    iferr (tmp_nsigma = imgetr (outim, "NSIGMA"))
		tmp_nsigma = INDEFR
	    iferr (tmp_ratio = imgetr (outim, "RATIO"))
		tmp_ratio = INDEFR
	    iferr (tmp_theta = imgetr (outim, "THETA"))
		tmp_theta = INDEFR
	    iferr (tmp_datamin = imgetr (outim, "DATAMIN"))
		tmp_datamin = INDEFR
	    iferr (tmp_datamax = imgetr (outim, "DATAMAX"))
		tmp_datamax = INDEFR

	    if (IS_INDEFR(tmp_scale) || ! fp_equalr (tmp_scale, apstatr (ap,
	        SCALE)))
		newimage = YES
	    else if (IS_INDEFR(tmp_fwhmpsf) || ! fp_equalr (tmp_fwhmpsf,
	        apstatr (ap, FWHMPSF)))
		newimage = YES
	    else if (IS_INDEFR(tmp_nsigma) || ! fp_equalr (tmp_nsigma,
	        apstatr (ap, NSIGMA)))
		newimage = YES
	    else if (IS_INDEFR(tmp_ratio) || ! fp_equalr (tmp_ratio,
	        apstatr (ap, RATIO)))
		newimage = YES
	    else if (IS_INDEFR(tmp_theta) || ! fp_equalr (tmp_theta,
	        apstatr (ap, THETA)))
		newimage = YES
	    else if (IS_INDEFR(tmp_datamin) || ! fp_equalr (tmp_datamin,
	        apstatr (ap, DATAMIN)))
		newimage = YES
	    else if (IS_INDEFR(tmp_datamax) || ! fp_equalr (tmp_datamax,
	        apstatr (ap, DATAMAX)))
		newimage = YES
	    else
		newimage = NO

	    if (newimage == YES) {
		call imunmap (outim)
		call imdelete (outname)
	        outim = immap (outname, NEW_COPY, im)
		IM_PIXTYPE(outim) = TY_REAL
	        call imaddr (outim, "SCALE", 1.0 / apstatr (ap, SCALE))
	        call imaddr (outim, "FWHMPSF", apstatr (ap, FWHMPSF))
	        call imaddr (outim, "NSIGMA", apstatr (ap, NSIGMA))
	        call imaddr (outim, "RATIO", apstatr (ap, RATIO))
	        call imaddr (outim, "THETA", apstatr (ap, THETA))
	    }
	}

	return (outim)
end


# AP_OUTMAP -- Manufacture an output coordinate file name.

procedure ap_outmap (ap, out, root)

pointer	ap		# pointer to the apphot structure
int	out		# the output file descriptor
char	root[ARB]	# root of the previous output file name, may be
			# updated

int	findex, lindex, version
pointer	sp, image, outname, newoutname
int	strmatch(), gstrmatch(), open(), fnldir(), access()

begin
	if (out != NULL)
	    call close (out)

	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (outname, SZ_FNAME, TY_CHAR)
	call salloc (newoutname, SZ_FNAME, TY_CHAR)

	# Get the old names.
	call apstats (ap, IMNAME, Memc[image], SZ_FNAME)
	call apstats (ap, OUTNAME, Memc[outname], SZ_FNAME)

	# Manufacture a new name. Search first for existing files with
	# the form "outname.coo.*" and create a new version number.
	# If the first search fails look for names containing root
	# and append a version number to create a new output file
	# name. Otherwise simply use the output name.

	if (Memc[outname] == EOS) {
	    Memc[newoutname] = EOS
        } else if (strmatch (Memc[outname], "\.coo\.") > 0) {
	    findex = fnldir (Memc[outname], Memc[newoutname], SZ_FNAME)
	    call apoutname (Memc[image], Memc[newoutname], "coo",
	        Memc[newoutname], SZ_FNAME)
	} else if (gstrmatch (Memc[outname], root, findex, lindex) > 0) {
	    repeat {
	        version = version + 1
	        call strcpy (Memc[outname], Memc[newoutname], SZ_FNAME)
	        call sprintf (Memc[newoutname+lindex], SZ_FNAME, ".%d")
		    call pargi (version)
	    } until (access (Memc[newoutname], 0, 0) == NO)
	} else {
	    version = 1
	    call strcpy (Memc[outname], root, SZ_FNAME)
	    call strcpy (Memc[outname], Memc[newoutname], SZ_FNAME)
	}

	# Open the output image.
	if (Memc[newoutname] == EOS)
	    out = NULL
	else
	    out = open (Memc[newoutname], NEW_FILE, TEXT_FILE)
	call apsets (ap, OUTNAME, Memc[newoutname])

	call sfree (sp)
end
