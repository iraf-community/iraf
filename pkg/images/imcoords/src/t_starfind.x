include <fset.h>

# T_STARFIND --  Automatically detect objects in an image given the full-
# width half-maximum of the image point spread function and a detection
# threshold using a modified version of the daofind algorithm.

procedure t_starfind ()

int	imlist, olist, limlist, lolist, boundary, verbose
int	stat, root, out, nxblock, nyblock
pointer	sp, image, output, outfname, str, wcs, wxformat, wyformat
pointer	im, sf
real	constant

bool	clgetb()
int	imtopenp(), clpopnu(), imtlen(), clplen(), clgwrd(), btoi(), open()
int	clgeti(), imtgetim(), clgfil(), fnldir(), strncmp(), strlen()
pointer	immap()
real	clgetr()

begin
	# Flush STDOUT on a new line.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (outfname, SZ_FNAME, TY_CHAR)
	call salloc (wcs, SZ_FNAME, TY_CHAR)
	call salloc (wxformat, SZ_FNAME, TY_CHAR)
	call salloc (wyformat, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Open the image and output file lists.
	imlist = imtopenp ("image")
	limlist = imtlen (imlist)
	olist = clpopnu ("output")
	lolist = clplen (olist)

	# Test the input and output file list.
	if (lolist > 1 && lolist != limlist) {
	    call imtclose (imlist)
	    call clpcls (olist)
	    call sfree (sp)
	    call error (0, "Imcompatible image and output list lengths")
	}

	# Get the algorithm parameters.
	call sf_gpars (sf)

	# Get the wcs paramaters.
	call clgstr ("wcs", Memc[wcs], SZ_FNAME)
	call clgstr ("wxformat", Memc[wxformat], SZ_FNAME)
	call clgstr ("wyformat", Memc[wyformat], SZ_FNAME)

	# Get the image blocking boundary extensions parameters.
	boundary = clgwrd ("boundary", Memc[str], SZ_LINE,
	    ",constant,nearest,reflect,wrap,")
	constant = clgetr ("constant")
	nxblock = clgeti ("nxblock")
	nyblock = clgeti ("nyblock")

	# Verbose mode ?
	verbose = btoi (clgetb ("verbose"))

	# Loop over the images.
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Open the input image.
	    im = immap (Memc[image], READ_ONLY, 0)

	    # Get the output file name and open the file.
	    if (lolist == 0) {
		call strcpy ("", Memc[outfname], SZ_FNAME)
		out = NULL
	    } else {
	        stat = clgfil (olist, Memc[output], SZ_FNAME)
		root = fnldir (Memc[output], Memc[outfname], SZ_FNAME)
		if (strncmp ("default", Memc[output+root], 7) == 0 || root ==
		    strlen (Memc[output])) {
	            call sf_outname (Memc[image], Memc[outfname], "obj",
		        Memc[outfname], SZ_FNAME)
		    lolist = limlist
		} else if (stat != EOF) {
		    call strcpy (Memc[output], Memc[outfname], SZ_FNAME)
		} else {
	            call sf_outname (Memc[image], Memc[outfname], "obj",
		        Memc[outfname], SZ_FNAME)
		    lolist = limlist
		}
	    }
	    out = open (Memc[outfname], NEW_FILE, TEXT_FILE)

	    # Find the stars in an image.
	    call sf_find (im, out, sf, nxblock, nyblock, Memc[wcs],
	        Memc[wxformat], Memc[wyformat], boundary, constant,
	        verbose)

	    # Close images and files.
	    call imunmap (im)
	    call close (out)

	}


	# Close lists.
	call sf_free (sf)
	call imtclose (imlist)
	call clpcls (olist)
	call sfree (sp)
end


# SF_OUTNAME -- Construct the output file name. If output is null or a
# directory, a name is constructed from the root of the image name and
# the extension. The disk is searched to avoid name collisions.

procedure sf_outname (image, output, ext, name, maxch)

char    image[ARB]              #I image name
char    output[ARB]             #I output directory or name
char    ext[ARB]                #I extension
char    name[ARB]               #O output name
int     maxch                   #I maximum size of name

int     ndir, nimdir, clindex, clsize
pointer sp, root, str
int     fnldir(), strlen()

begin
        call smark (sp)
        call salloc (root, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_FNAME, TY_CHAR)

        ndir = fnldir (output, name, maxch)
        if (strlen (output) == ndir) {
	    call imparse (image, Memc[root], SZ_FNAME, Memc[str], SZ_FNAME,
		Memc[str], SZ_FNAME, clindex, clsize)
            nimdir = fnldir (Memc[root], Memc[str], SZ_FNAME)
	    if (clindex >= 0) {
                call sprintf (name[ndir+1], maxch, "%s%d.%s.*")
                    call pargstr (Memc[root+nimdir])
                    call pargi (clindex)
                    call pargstr (ext)
	    } else {
                call sprintf (name[ndir+1], maxch, "%s.%s.*")
		    call pargstr (Memc[root+nimdir])
                    call pargstr (ext)
	    }
            call sf_oversion (name, name, maxch)
        } else
            call strcpy (output, name, maxch)

        call sfree (sp)
end


## SF_IMROOT -- Fetch the root image name minus the directory specification
## and the section notation. The length of the root name is returned.
#
#int procedure sf_imroot (image, root, maxch)
#
#char    image[ARB]              #I image specification
#char    root[ARB]               #O rootname
#int     maxch                   #I maximum number of characters
#
#int     nchars
#pointer sp, str
#int     fnldir(), strlen()
#
#begin
#        call smark (sp)
#        call salloc (str, SZ_FNAME, TY_CHAR)
#
#        call imgimage (image, root, maxch)
#        nchars = fnldir (root, Memc[str], maxch)
#        call strcpy (root[nchars+1], root, maxch)
#
#        call sfree (sp)
#        return (strlen (root))
#end


# SF_OVERSION -- Compute the next available version number of a given file
# name template and output the new file name.

procedure sf_oversion (template, filename, maxch)

char    template[ARB]                   #I name template
char    filename[ARB]                   #O output name
int     maxch                           #I maximum number of characters

char    period
int     newversion, version, len
pointer sp, list, name
int     fntgfnb() strldx(), ctoi(), fntopnb()

begin
        # Allocate temporary space
        call smark (sp)
        call salloc (name, maxch, TY_CHAR)
        period = '.'
        list = fntopnb (template, NO)

        # Loop over the names in the list searchng for the highest version.
        newversion = 0
        while (fntgfnb (list, Memc[name], maxch) != EOF) {
            len = strldx (period, Memc[name])
            len = len + 1
            if (ctoi (Memc[name], len, version) <= 0)
                next
            newversion = max (newversion, version)
        }

        # Make new output file name.
        len = strldx (period, template)
        call strcpy (template, filename, len)
        call sprintf (filename[len+1], maxch, "%d")
            call pargi (newversion + 1)

        call fntclsb (list)
        call sfree (sp)
end
