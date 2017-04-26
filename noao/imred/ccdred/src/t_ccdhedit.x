include	<error.h>

define	TYPES	"|string|real|integer|"
define	SVAL	1	# String value
define	RVAL	2	# Real value
define	IVAL	3	# Integer value

# T_CCDHEDIT -- Add, delete, or change CCD image header parameters.
# This task differs from HEDIT in that it uses the CCD instrument translation
# file.

procedure t_ccdhedit ()

int	list			# List of CCD images
pointer	param			# Parameter name
int	type			# Parameter type
pointer	sval			# Parameter value
pointer	instrument		# Instrument file

int	ip, ival, imtopenp(), imtgetim(), clgwrd(), ctoi(), ctor()
real	rval
bool	streq()
pointer	sp, im, immap()
errchk	hdmpstr, hdmputr, hdmputi

begin
	call smark (sp)
	call salloc (param, SZ_LINE, TY_CHAR)
	call salloc (sval, SZ_LINE, TY_CHAR)
	call salloc (instrument, SZ_FNAME, TY_CHAR)

	# Get the task parameters.
	list = imtopenp ("images")
	call clgstr ("parameter", Memc[param], SZ_LINE)
	type = clgwrd ("type", Memc[sval], SZ_LINE, TYPES)
	call clgstr ("value", Memc[sval], SZ_LINE)
	call clgstr ("instrument", Memc[instrument], SZ_FNAME)
	call xt_stripwhite (Memc[sval])

	# Open the instrument translation file.
	call hdmopen (Memc[instrument])

	# If the parameter is IMAGETYP then change the parameter value from
	# the package form to the image form using the inverse mapping in the
	# translation file.

	if (streq (Memc[param], "imagetyp"))
	    call hdmparm (Memc[sval], Memc[sval], SZ_LINE)

	# Edit each image in the input list.
	while (imtgetim (list, Memc[instrument], SZ_FNAME) != EOF) {
	    iferr (im = immap (Memc[instrument], READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }

	    # If the parameter value is null then delete the entry.
	    if (Memc[sval] == EOS) {
		iferr (call hdmdelf (im, Memc[param]))
		    call erract (EA_WARN)

	    # Otherwise add the parameter of the specified type.
	    } else {
	        switch (type) {
	        case SVAL:
	            call hdmpstr (im, Memc[param], Memc[sval])
	        case RVAL:
		    ip = 1
		    if (ctor (Memc[sval], ip, rval) == 0)
		        call error (0, "Parameter value is not a number")
		    call hdmputr (im, Memc[param], rval)
	        case IVAL:
		    ip = 1
		    if (ctoi (Memc[sval], ip, ival) == 0)
		        call error (0, "Parameter value is not a number")
		    call hdmputi (im, Memc[param], ival)
	        }
	    }

	    call imunmap (im)
	}

	# Finish up.
	call hdmclose ()
	call imtclose (list)
	call sfree (sp)
end
