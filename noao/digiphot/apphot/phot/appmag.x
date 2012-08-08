include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/photdef.h"
include "../lib/phot.h"

# AP_PMAG -- Procedure to write the results of the phot task to the output 
# file.

procedure ap_pmag (ap, fd, id, lid, cier, sier, pier)

pointer	ap	# pointer to apphot structure
int 	fd	# output text file
int	id	# id number of str
int	lid	# list id of star
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# photometric error

int	i, naperts
int	apstati()
real	apstatr()

begin
	# Initialize.
	if (fd == NULL)
	    return

	# Write out the object id parameters.
	call ap_wid (ap, fd, apstatr (ap, OXINIT), apstatr(ap, OYINIT),
	    id, lid, '\\')

	# Write out the centering results.
	call ap_wcres (ap, fd, cier, '\\')

	# Write out the sky fitting results.
	call ap_wsres (ap, fd, sier, '\\')

	# Write out the photometry results.
	naperts = apstati (ap, NAPERTS)
	if (naperts == 0)
	    call ap_wmres (ap, fd, 0, pier, "  ")
	else {
	    do i = 1, naperts {
	        if (naperts == 1)
		    call ap_wmres (ap, fd, i, pier, "  ")
	        else if (i == naperts)
		    call ap_wmres (ap, fd, i, pier, "* ")
	        else
		    call ap_wmres (ap, fd, i, pier, "*\\")
	    }
	}
end


# AP_QPMAG -- Procedure to print a quick summary of the phot output on the
# standard output.

procedure ap_qpmag (ap, cier, sier, pier)

pointer	ap	# pointer to apphot structure
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# photometry error

int	i
pointer	sp, imname, phot
real	apstatr()

begin
	# Initialize.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	phot = AP_PPHOT(ap)

	# Print the center and sky value.
	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	call printf ("%s  %8.2f %8.2f  %8g  ")
	    call pargstr (Memc[imname])
	    call pargr (apstatr (ap, OPXCUR))
	    call pargr (apstatr (ap, OPYCUR))
	    call pargr (apstatr (ap, SKY_MODE))

	# Print out the magnitudes and errors.
	do i = 1, AP_NAPERTS(phot) {
	    if (i == AP_NAPERTS(phot))
		call printf ("%7.3f  ")
	    else
	        call printf ("%7.3f  ")
		call pargr (Memr[AP_MAGS(phot)+i-1])
	}

	# Print out the error codes.
	if (cier != AP_OK || sier != AP_OK || pier != AP_OK) {
	    call printf ("err\n")
	} else {
	    call printf ("ok\n")
	}

	call sfree (sp)
end


# AP_MAGHDR -- Procedure to print the banner for the phot task on the
# standard output.

procedure ap_maghdr (ap, fd)

pointer	ap		# pointer to apphot strucuture
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call ap_idhdr (ap, fd)
	call ap_chdr (ap, fd)
	call ap_shdr (ap, fd)
	call ap_mhdr (ap, fd)
end
