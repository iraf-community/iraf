# CCD_PRCSELECT -- Filter a list of image names passing on only those that
# do (or don't) have a specified processing flag set.

include "ccdtypes.h"

define	PROCFLAGS "|fixpix|overscan|trim|zerocor|darkcor|flatcor|illumcor\
	|fringecor|ccdproc|"

procedure t_ccdprcselect ()

pointer	inlist			#TI List of input image name.
char	output[SZ_FNAME]	#TI List of output image names.
char	instrument[SZ_FNAME]	#TI Instrument translation file.
char	procflag[SZ_LINE]	#TI List of proc flags.
char	ccdtype[SZ_LINE]	#TI ccdtype to select.

int	flag, ip, type
char	image[SZ_LINE], buffer[SZ_LINE]
pointer	fdout, im

int	strdic(), imtopenp(), imtgetim(), hdmaccf(), ctowrd(), imaccess()
int	ccdtypei()

pointer	open(), immap()

begin
	# Open input and output image lists
	inlist = imtopenp ("input")
	call clgstr ("output", output, SZ_LINE)
	fdout = open (output, APPEND, TEXT_FILE)

	# Open instrument file
	call clgstr    ("instrument",  instrument,  SZ_FNAME)
	call hdmopen   (instrument)

	# Get processing flag. 
	# If the first character is "!" pass all images for which the specified
	# flag is not set. If the processing flag is "" we pass al images.
	flag = 0
	call clgstr ("procflag", buffer, SZ_LINE)
	ip = 1
	if (ctowrd (buffer, ip, procflag, SZ_LINE) != 0) {
	    if (procflag[1] == '!') {
		flag =  -1 * strdic (procflag[2], procflag, SZ_LINE, PROCFLAGS)
	    } else {
		flag = strdic (procflag, procflag, SZ_LINE, PROCFLAGS)
	    }
	    if (flag == 0)
		call error (0, "Unknown processing flag")
	}

	# Get ccdtype to select.
	call clgstr   ("ccdtype", ccdtype, SZ_LINE)
	type = strdic (ccdtype, ccdtype, SZ_LINE, CCDTYPES)

	while (imtgetim (inlist, image, SZ_LINE) != EOF) {

	    # Silently skip any non-existant images
	    if (imaccess (image, READ_ONLY) == NO)
		next

	    im = immap (image, READ_ONLY, 0)

	    if ((ccdtype[1] != EOS) && (type != ccdtypei (im))) {
		call imunmap (im)
		next
	    }

	    if (flag < 0) {
		if (hdmaccf (im, procflag) == NO) {
		    call fprintf (fdout, "%s\n")
			call pargstr (image)
		}
	    } else if (flag > 0) {
		if (hdmaccf (im, procflag) == YES) {
		    call fprintf (fdout, "%s\n")
			call pargstr (image)
		}
	    } else {
		call fprintf (fdout, "%s\n")
		    call pargstr (image)
	    }
	    call imunmap (im)
	}

	# Tidy up
	call close (fdout)
	call hdmclose ()
	call imtclose (inlist)
end
