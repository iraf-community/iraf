include	"refspectra.h"


# REFMSGS -- Print any verbose messages to log files.  All messages
# except the assignments go through this procedure.  It calls REFPRINT with
# each output stream.

procedure refmsgs (msg, spec, ap, ref1, ref2)

int	msg		# Message code
char	spec[ARB]	# Spectrum
int	ap		# Aperture
char	ref1[ARB]	# Reference spectrum
char	ref2[ARB]	# Reference spectrum

int	fd, clgfil(), open()
pointer	sp, logfile
include	"refspectra.com"

begin
	if (verbose == NO)
	    return

	call smark (sp)
	call salloc (logfile, SZ_FNAME, TY_CHAR)
	while (clgfil (logfiles, Memc[logfile], SZ_FNAME) != EOF) {
	    fd = open (Memc[logfile], APPEND, TEXT_FILE)
	    call refprint (fd, msg, spec, ap, ref1, ref2)
	    call close (fd)
	}
	call clprew (logfiles)

	call sfree (sp)
end


# REFPRINT -- Print requested message with appropriate parameters if non-null
# stream is specified.

procedure refprint (fd, msg, spec, ap, ref1, ref2)

int	fd		# File descriptor
int	msg		# Message code
char	spec[ARB]	# Spectrum
int	ap		# Aperture
char	ref1[ARB]	# Reference spectrum
char	ref2[ARB]	# Reference spectrum

begin
	if (fd == NULL)
	    return

	switch (msg) {
	case NO_SPEC:
	    call fprintf (fd, "[%s] Spectrum not found\n")
		call pargstr (spec)
	case NO_REF:
	    call fprintf (fd, "[%s] Reference spectrum not found\n")
		call pargstr (spec)
	case NOT_REFSPEC:
	    call fprintf (fd, "[%s] Not a reference spectrum\n")
		call pargstr (spec)
	case NO_REFSPEC:
	    call fprintf (fd, "[%s] No reference spectrum found\n")
		call pargstr (spec)
	case DEF_REFSPEC:
	    call fprintf (fd, "[%s] Reference spectra already defined: %s %s\n")
		call pargstr (spec)
		call pargstr (ref1)
		call pargstr (ref2)
	case OVR_REFSPEC:
	    call fprintf (fd, 
		"[%s] Overriding previous reference spectra: %s %s\n")
		call pargstr (spec)
		call pargstr (ref1)
		call pargstr (ref2)
	case BAD_AP:
	    call fprintf (fd, "[%s] Wrong aperture: %d\n")
		call pargstr (spec)
		call pargi (ap)
	case BAD_REFAP:
	    call fprintf (fd, "[%s] Wrong reference aperture: %d\n")
		call pargstr (spec)
		call pargi (ap)
	}
end
