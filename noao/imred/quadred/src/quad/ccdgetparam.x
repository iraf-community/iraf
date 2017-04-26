procedure ccdgetparam ()

char	image[SZ_FNAME]		# Image whose parameter is to be fetched
char	parameter[SZ_LINE]	# Parameter whose value is required.
char	instrument[SZ_FNAME]	# CCD intrument file.

char	buffer[SZ_LINE]
pointer	im

pointer	immap()
int	hdmaccf()
bool	streq()

begin

	call clgstr ("image", image, SZ_FNAME)
	im = immap (image, READ_ONLY, 0)

	call clgstr ("instrument", instrument, SZ_FNAME)
	call hdmopen (instrument)

	call clgstr ("parameter", parameter, SZ_LINE)

	# Handle special cases where we must translate the parameter value
	# to the corresponding package name.
	if (streq (parameter, "imagetyp")) {
	    call ccdtypes (im, buffer, SZ_LINE)
	    call printf ("%s\n")
		call pargstr (buffer)

	} else if (streq (parameter, "subset")) {
	    call ccdsubset (im, buffer, SZ_LINE)
	    call printf ("%s\n")
		call pargstr (buffer)

	} else {

	    if (hdmaccf (im, parameter) == NO) {
		call printf ("UNDEFINED!\n")
	    } else {
		call hdmgstr (im, parameter, buffer, SZ_LINE)
		call printf ("%s\n")
		    call pargstr (buffer)
	    }
	}

	call imunmap (im)
end
