include <imhdr.h>
include <gset.h>
include <fset.h>
include "../lib/apphot.h"
include "../lib/noise.h"
include "../lib/find.h"

# T_DAOFIND --  Automatically detect objects in an image given the full
# width half maximum of the image PSF and an intensity threshold.

procedure t_daofind ()

pointer	image			# pointer to input image
pointer cnvimage		# pointer to convolved image
pointer	output			# pointer to the results file
int	boundary		# type of boundary extension
real	constant		# constant for constant boundary extension
int	interactive		# interactive mode
int	verify			# verify mode
int	update			# update critical parameters
int	verbose			# verbose mode

int	limlist, lolist, save, out, root, stat
pointer	imlist, olist, im, cnv, sp, outfname, cnvname, str, ap, cname
pointer	display, graphics, id, gd

bool	clgetb(), streq()
int	imtlen(), clplen(), btoi(), clgwrd(), aptmpimage(), access()
int	open(), strmatch(), strncmp(), strlen(), fnldir(), ap_fdfind()
pointer	imtopenp(), clpopnu(), clgfil(), imtgetim(), immap(), ap_cnvmap()
pointer	gopen()
real	clgetr()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (cnvimage, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (cnvname, SZ_FNAME, TY_CHAR)
	call salloc (cname, SZ_FNAME, TY_CHAR)
	call salloc (display, SZ_FNAME, TY_CHAR)
	call salloc (graphics, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Flush STDOUT on a new line.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Fetch the image and output file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	olist = clpopnu ("output")
	lolist = clplen (olist)

	# Check that the input image and output file lists are the same length.
	stat = clgfil (olist, Memc[output], SZ_FNAME)
	if (lolist > 0 && strmatch (Memc[output], "default") == 0 &&
	    lolist != limlist) {
	    call imtclose (imlist)
	    call clpcls (olist)
	    call error (0,
	        "Length of image and output file lists are not the same")
	}
	call clprew (olist)

	# Get prefix for density enhancement image.
	call clgstr ("convolution", Memc[cnvimage], SZ_FNAME)

	# Get the image boundary extensions parameters.
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")

	call clgstr ("commands.p_filename", Memc[cname], SZ_FNAME)
	interactive = btoi (clgetb ("interactive"))
	verify = btoi (clgetb ("verify"))
	update = btoi (clgetb ("update"))
	verbose = btoi (clgetb ("verbose"))

	# Open the graphics and display devices.
	call clgstr ("graphics", Memc[graphics], SZ_FNAME)
	call clgstr ("display", Memc[display], SZ_FNAME)
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

	# Confirm the parameters.
	call ap_fdgpars (ap)
	if (verify == YES && interactive == NO)  {
	    call ap_fdconfirm (ap)
	    if (update == YES)
		call ap_fdpars (ap)
	}

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the image and results file
	    im = immap (Memc[image], READ_ONLY, 0)
	    call ap_rdnoise (im, ap)
	    call ap_padu (im, ap)
	    call ap_itime (im, ap)
	    call ap_airmass (im, ap)
	    call ap_filter (im, ap)
	    call apsets (ap, IMNAME, Memc[image])

	    # Set up the results file. If output is a null string or
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
	            call apoutname (Memc[image], "", "coo", Memc[outfname],
		        SZ_FNAME)
	            #out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		    #out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		}
	    }
	    call apsets (ap, OUTNAME, Memc[outfname])
	    if (access (Memc[outfname], 0, 0) == YES)
		call error (0, "T_DAOFIND: Output file already exists.\n")

	    # Set up the directory and name for the density enhancement image.
	    # Note that the default value for cnvimage is the null string
	    # indicating the current directory. If cnvimage is null or
	    # contains only a directory specification make a temporary image
	    # name using the prefix "cnv, otherwise use the user specified
	    # prefix.

	    save = aptmpimage (Memc[image], Memc[cnvimage], "cnv",
	        Memc[cnvname], SZ_FNAME)

	    # Find the stars in an image.
	    if (interactive == NO) {

		cnv = NULL
		if (Memc[cname] != EOS) {
		    stat = ap_fdfind (Memc[cnvname], ap, im, NULL, NULL, NULL,
		        out, boundary, constant, save, NO)
		} else {
	            cnv = ap_cnvmap (Memc[cnvname], im, ap, save)
		    if (Memc[outfname] != EOS)
		        out = open (Memc[outfname], NEW_FILE, TEXT_FILE)
		    else
			out = NULL
		    call ap_bfdfind (im, cnv, out, ap, boundary, constant,
			verbose)
	            call imunmap (cnv)
	            if (save == NO)
	                call imdelete (Memc[cnvname])
		    stat = NO
		}

	    } else {
		stat = ap_fdfind (Memc[cnvname], ap, im, gd, NULL, id, out,
		    boundary, constant, save, YES)
	    }

	    # Clean up files.
	    call imunmap (im)
	    call close (out)

	    if (stat == YES)
		break
	}

	# Close image and output file lists.
	if (id == gd && id != NULL) {
	    call gclose (id)
	} else {
	    if (gd != NULL)
	        call gclose (gd)
	    if (id != NULL)
	        call gclose (gd)
	}
	call ap_fdfree (ap)
	call imtclose (imlist)
	call clpcls (olist)
	call sfree (sp)
end


# AP_CNVMAP -- Access the convolved image.

pointer procedure ap_cnvmap (cnvname, im, ap, save)

char	cnvname[ARB]		# convolved image name
pointer	im			# pointer to input image
pointer	ap			# pointer to the apphot structure
int	save			# save the convolved image

int	newimage
pointer	cnv
real	tmp_scale, tmp_fwhmpsf, tmp_ratio, tmp_theta, tmp_nsigma
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

	if (save == NO || imaccess (cnvname, READ_ONLY) == NO) {

	    cnv = immap (cnvname, NEW_COPY, im)
	    call imaddr (cnv, "SCALE", 1.0 / apstatr (ap, SCALE))
	    call imaddr (cnv, "FWHMPSF", apstatr (ap, FWHMPSF))
	    call imaddr (cnv, "NSIGMA", apstatr (ap, NSIGMA))
	    call imaddr (cnv, "RATIO", apstatr (ap, RATIO))
	    call imaddr (cnv, "THETA", apstatr (ap, THETA))

	} else {

	    cnv = immap (cnvname, READ_ONLY, 0)
	    iferr (tmp_scale = 1.0 / imgetr (cnv, "SCALE"))
		tmp_scale = INDEFR
	    iferr (tmp_fwhmpsf = imgetr (cnv, "FWHMPSF"))
		tmp_fwhmpsf = INDEFR
	    iferr (tmp_nsigma = imgetr (cnv, "NSIGMA"))
		tmp_nsigma = INDEFR
	    iferr (tmp_ratio = imgetr (cnv, "RATIO"))
		tmp_ratio = INDEFR
	    iferr (tmp_theta = imgetr (cnv, "THETA"))
		tmp_theta = INDEFR

	    newimage = NO
	    if (IS_INDEFR(tmp_scale) || ! fp_equalr (tmp_scale, apstatr (ap,
	        SCALE)))
		newimage = YES
	    if (IS_INDEFR(tmp_fwhmpsf) || ! fp_equalr (tmp_fwhmpsf,
	        apstatr (ap, FWHMPSF)))
		newimage = YES
	    if (IS_INDEFR(tmp_nsigma) || ! fp_equalr (tmp_nsigma,
	        apstatr (ap, NSIGMA)))
		newimage = YES
	    if (IS_INDEFR(tmp_ratio) || ! fp_equalr (tmp_ratio,
	        apstatr (ap, RATIO)))
		newimage = YES
	    if (IS_INDEFR(tmp_theta) || ! fp_equalr (tmp_theta,
	        apstatr (ap, THETA)))
		newimage = YES

	    if (newimage == YES) {
		call imunmap (cnv)
		call imdelete (cnvname)
	        cnv = immap (cnvname, NEW_COPY, im)
	        call imaddr (cnv, "SCALE", 1.0 / apstatr (ap, SCALE))
	        call imaddr (cnv, "FWHMPSF", apstatr (ap, FWHMPSF))
	        call imaddr (cnv, "NSIGMA", apstatr (ap, NSIGMA))
	        call imaddr (cnv, "RATIO", apstatr (ap, RATIO))
	        call imaddr (cnv, "THETA", apstatr (ap, THETA))
	    }
	}

	return (cnv)
end


# AP_OUTMAP -- Manufacture an output coordinate file name.

procedure ap_outmap (ap, out, root)

pointer	ap		# pointer to the apphot structure
int	out		# pointer to the output file
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
	    call apoutname (Memc[image], "", "coo", Memc[newoutname],
		SZ_FNAME)
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
