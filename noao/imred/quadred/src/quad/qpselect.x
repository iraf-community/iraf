# QPSELECT -- Filter a list of image names passing on only those that are of 
# the specified ccdtype -AND-
#
# If stop = yes
#       1) Are multi-readout		-AND-
# 	2) Have not been trimmed
# If stop = no
#	1) Are single-readout		-OR-
#	2) Have been trimmed

include "ccdtypes.h"

procedure t_qpselect ()

pointer	inlist			#TI List of input image name.
char	output[SZ_FNAME]	#TI List of output image names.
char	instrument[SZ_FNAME]	#TI Instrument translation file.
char	ccdtype[SZ_LINE]	#TI ccdtype to select.
bool	stop			#TI stop rather than pass selected images

int	type, nampx, nampy
char	image[SZ_LINE], nampsyx[SZ_LINE]
pointer	fdout, im

int	strdic(), imtopenp(), imtgetim(), hdmaccf(), imaccess()
int	ccdtypei()
bool	clgetb()

pointer	open(), immap()

begin
	# Open input and output image lists
	inlist = imtopenp ("input")
	call clgstr ("output", output, SZ_LINE)
	fdout = open (output, APPEND, TEXT_FILE)

	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Get ccdtype to select.
	call clgstr   ("ccdtype", ccdtype, SZ_LINE)
	type = strdic (ccdtype, ccdtype, SZ_LINE, CCDTYPES)

	# Get stop
	stop = clgetb ("stop")

	while (imtgetim (inlist, image, SZ_LINE) != EOF) {

	    # Silently skip any non-existant images
	    if (imaccess (image, READ_ONLY) == NO)
		next

	    im = immap (image, READ_ONLY, 0)

	    if ((ccdtype[1] != EOS) && (type != ccdtypei (im))) {
		call imunmap (im)
		next
	    }

	    if (stop) {

		if (hdmaccf (im, "trim") == YES) {
		    call fprintf (fdout, "%s\n")
			call pargstr (image)

		} else if (hdmaccf (im, "nampsyx") == NO) {
		    call fprintf (fdout, "%s\n")
			call pargstr (image)

		} else {

		    call hdmgstr (im, "nampsyx", nampsyx, SZ_LINE)
		    call sscan (nampsyx)
			call gargi (nampx)
			call gargi (nampy)

		    if (nampx == 1 && nampy == 1) {
			call fprintf (fdout, "%s\n")
			    call pargstr (image)
		    }
		}

	    } else {

		if ((hdmaccf (im, "trim")    == NO) &&
		    (hdmaccf (im, "nampsyx") == YES)) {

		    call hdmgstr (im, "nampsyx", nampsyx, SZ_LINE)
		    call sscan (nampsyx)
			call gargi (nampx)
			call gargi (nampy)

		    if (nampx != 1 || nampy != 1) {
			call fprintf (fdout, "%s\n")
			    call pargstr (image)
		    }
		}
	    }

	    call imunmap (im)
	}

	# Tidy up
	call close (fdout)
	call hdmclose ()
	call imtclose (inlist)
end
