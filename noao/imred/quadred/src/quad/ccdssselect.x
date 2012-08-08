# CCDSUBSETSELECT -- Filter a list of image names passing on only those that
# belong to a specified subset.

include "ccdtypes.h"

procedure t_ccdssselect ()

pointer	inlist			#TI List of input image name.
char	output[SZ_FNAME]	#TI List of output image names.
char	instrument[SZ_FNAME]	#TI Instrument translation file.
char	subset[SZ_LINE]		#TI Subset required.
char	ccdtype[SZ_LINE]	#TI ccdtype required.

int	type
char	image[SZ_LINE], buffer[SZ_LINE]
pointer	fdout, im

int	strdic(), imtopenp(), imtgetim(), ccdtypei(), imaccess()
pointer	open(), immap()
bool	strne()

begin
	# Open input and output image lists
	inlist = imtopenp ("input")
	call clgstr ("output", output, SZ_LINE)
	fdout = open (output, APPEND, TEXT_FILE)

	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Get subset required. 
	call clgstr ("subset", subset, SZ_LINE)

	# Get ccdtype required.
	call clgstr   ("ccdtype", ccdtype, SZ_LINE)
	type = strdic (ccdtype, ccdtype, SZ_LINE, CCDTYPES)

	while (imtgetim (inlist, image, SZ_LINE) != EOF) {

	    # Silently skip non-existant images
	    if (imaccess (image, READ_ONLY) == NO)
		next

	    im = immap (image, READ_ONLY, 0)

	    # Skip images of the wrong type
	    if ((ccdtype[1] != EOS) && (type != ccdtypei (im))) {
		call imunmap (im)
		next
	    }
	    
	    # Skip images of the wrong subset
	    if (subset[1] != EOS) {
		call ccdsubset (im, buffer, SZ_LINE)
		if (strne (subset, buffer)) {
		    call imunmap (im)
		    next
		}
	    }

	    # print names of any images which pass the test.
	    call fprintf (fdout, "%s\n")
		call pargstr (image)

	    call imunmap (im)
	}

	# Tidy up
	call close (fdout)
	call hdmclose ()
	call imtclose (inlist)
end
