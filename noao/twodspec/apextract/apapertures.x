include	"apertures.h"

# AP_APERTURES -- List the apertures to a text file.

procedure ap_apertures (file, aps, naps)

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
	fd = open (file, NEW_FILE, TEXT_FILE)

	call fprintf (fd, "#\tAPERTURES\n\n##ID\tBEAM\tCENTER\tLOW\tHIGH\n\n")
	for (i = 1; i <= naps; i = i + 1) {
	    ap = aps[i]
	    apaxis = AP_AXIS(ap)
	    call fprintf (fd, "%d\t%d\t%7.2f\t%7.2f\t%7.2f\n")
		call pargi (AP_ID(ap))
		call pargi (AP_BEAM(ap))
		call pargr (AP_CEN(ap, apaxis))
		call pargr (AP_LOW(ap, apaxis))
		call pargr (AP_HIGH(ap, apaxis))
	}

	call close (fd)
end
