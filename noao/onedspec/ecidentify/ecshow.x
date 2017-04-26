include	<pkg/center1d.h>
include	"ecidentify.h"

# EC_SHOW -- Show parameter information.

procedure ec_show (ec, file)

pointer	ec			# ID pointer
char	file[ARB]		# File

char	line[SZ_LINE]
int	fd

int	open(), ecf_geti()
double	ecf_getd()
errchk	open()

begin
	fd = open (file, APPEND, TEXT_FILE)

	call sysid (line, SZ_LINE)
	call fprintf (fd, "%s\n")
	call pargstr (line)

	call fprintf (fd, "image %s\n")
	    call pargstr (Memc[EC_IMAGE(ec)])
	switch (EC_FTYPE(ec)) {
	case EMISSION:
	    call fprintf (fd, "ftype emission\n")
	case ABSORPTION:
	    call fprintf (fd, "ftype absorption\n")
	}
	switch (EC_LABELS(ec)) {
	case 2:
	    call fprintf (fd, "labels index\n")
	case 3:
	    call fprintf (fd, "labels pixel\n")
	case 4:
	    call fprintf (fd, "labels user\n")
	default:
	    call fprintf (fd, "labels none\n")
	}
	call fprintf (fd, "maxfeatures %d\n")
	    call pargi (EC_MAXFEATURES(ec))
	call fprintf (fd, "match %g\n")
	    call pargr (EC_MATCH(ec))
	call fprintf (fd, "zwidth %g\n")
	    call pargr (EC_ZWIDTH(ec))
	call fprintf (fd, "fwidth %g\n")
	    call pargr (EC_FWIDTH(ec))
	call fprintf (fd, "database %s\n")
	    call pargstr (Memc[EC_DATABASE(ec)])
	call fprintf (fd, "coordlist %s\n")
	    call pargstr (Memc[EC_COORDLIST(ec)])
	call fprintf (fd, "cradius %g\n")
	    call pargr (EC_CRADIUS(ec))
	call fprintf (fd, "threshold %g\n")
	    call pargr (EC_THRESHOLD(ec))
	call fprintf (fd, "minsep %g\n")
	    call pargr (EC_MINSEP(ec))
	if (EC_ECF(ec) != NULL) {
	    call fprintf (fd, "function = %s\n")
	        call ecf_gets ("function", line, SZ_LINE)
		call pargstr (line)
	    call fprintf (fd, "xorder = %d, yorder = %d\n")
		call pargi (ecf_geti ("xorder"))
		call pargi (ecf_geti ("yorder"))
	    call fprintf (fd,
		"niterate = %d, lowreject = %g, highreject = %g\n")
		call pargi (ecf_geti ("niterate"))
		call pargd (ecf_getd ("low"))
		call pargd (ecf_getd ("high"))
	    call fprintf (fd, "Fit at first pixel = %0.8g\n")
		call pargd (Memd[EC_FITDATA(ec)])
	}

	call close (fd)
end
