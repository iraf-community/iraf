include "../lib/apphot.h"
include "../lib/fitsky.h"

# AP_PSSKY -- Procedure to write the results of the fitsky task to
# the output file.

procedure ap_pssky (ap, fd, id, ld, ier)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor
int	id		# sequence number of star
int	ld		# list number of star
int	ier		# error code

real	apstatr()

begin
	# Return if NULL file descriptor.
	if (fd == NULL)
	    return

	# Print the object id and computed sky values.
	call ap_wid (ap, fd, apstatr (ap, OSXCUR), apstatr (ap, OSYCUR), id,
	    ld, '\\')
	call ap_wsres (ap, fd, ier, ' ')
end


# AP_QSPSKY -- Procedure to print a quick summary of the fitsky task on the
# standard output.

procedure ap_qspsky (ap, ier)

pointer	ap		# pointer to apphot structure
int	ier		# error code

pointer	sp, imname
int	apstati()
real	apstatr()

begin
	# Print out the results on the standard output.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	call printf ( "%s  %8.2f %8.2f  %8g %8g ") 
	    call pargstr (Memc[imname])
	    call pargr (apstatr (ap, OSXCUR))
	    call pargr (apstatr (ap, OSYCUR))
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	call printf ("%8g  %5d %5d  %s\n")
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))
	    if (ier != AP_OK)
		call pargstr ("err")
	    else
		call pargstr ("ok")
	call sfree (sp)
end


# AP_QASPSKY -- Procedure to print a quick summary of the fitsky task on the
# standard output.

procedure ap_qaspsky (ap, ier)

pointer	ap		# pointer to apphot structure
int	ier		# error code

int	apstati()
real	apstatr()

begin
	# Print out the results on the standard output.
	call printf ( "    Averages  %8g %8g ") 
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	call printf ("%8g  %5d %5d  %s\n\n")
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))
	    if (ier != AP_OK)
		call pargstr ("err")
	    else
		call pargstr ("ok")
end

# AP_SPHDR -- Procedure to write the banner for the fitsky task to the
# output file.

procedure ap_sphdr (ap, fd)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call ap_idhdr (ap, fd)
	call ap_shdr (ap, fd)
end
