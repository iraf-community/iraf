include	"apertures.h"

# AP_SHOW -- List the apertures to a text file.

procedure ap_show (file, aps, naps)

char	file[ARB]		# Aperture file
pointer	aps[ARB]		# Aperture pointers
int	naps			# Number of apertures

pointer	ap
int	i, apaxis, fd, open()
errchk	open

begin
	if (naps == 0)
	    return

	# Open the output file.  Return if an error occurs.
	fd = open (file, APPEND, TEXT_FILE)

	call fprintf (fd, "# APERTURES\n\n%4s %4s %7s %7s %7s %s\n")
	    call pargstr ("##ID")
	    call pargstr ("BEAM")
	    call pargstr ("CENTER")
	    call pargstr ("LOW")
	    call pargstr ("HIGH")
	    call pargstr ("TITLE")
	for (i = 1; i <= naps; i = i + 1) {
	    ap = aps[i]
	    apaxis = AP_AXIS(ap)
	    call fprintf (fd, "%4d %4d %7.2f %7.2f %7.2f")
		call pargi (AP_ID(ap))
		call pargi (AP_BEAM(ap))
		call pargr (AP_CEN(ap, apaxis))
		call pargr (AP_LOW(ap, apaxis))
		call pargr (AP_HIGH(ap, apaxis))
	    if (AP_TITLE(ap) != NULL) {
		call fprintf (fd, " %s")
		    call pargstr (Memc[AP_TITLE(ap)])
	    }
	    call fprintf (fd, "\n")
	}

	call close (fd)
end
