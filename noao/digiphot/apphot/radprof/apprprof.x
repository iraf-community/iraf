include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/photdef.h"
include "../lib/phot.h"
include "../lib/radprof.h"

# AP_PRPROF -- Procedure to write the results of radprof to the output file.

procedure ap_prprof (ap, fd, id, lid, cier, sier, pier, rier)

pointer	ap	# pointer to apphot structure
int	fd	# output text file descriptor
int	id	# id number of str
int	lid	# list id of star
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# photometric error
int	rier	# radial profile error

int	i, naperts
int	apstati()
real	apstatr()

begin
	if (fd == NULL)
	    return

	# Print the id parameters.
	call ap_wid (ap, fd, apstatr (ap, OXINIT), apstatr (ap, OYINIT),
	    id, lid, '\\')

	# Print the center parameters.
	call ap_wcres (ap, fd, cier, '\\')

	# Print the sky values.
	call ap_wsres (ap, fd, sier, '\\')

	# Print photometry parameters.
	naperts = apstati (ap, NAPERTS)
	do i = 1, naperts {
	    if (naperts == 1)
		call ap_wmres (ap, fd, i, pier, " \\")
	    else
		call ap_wmres (ap, fd, i, pier, "*\\")
	}

	# Print the radprof parameters.
	call ap_wrres (ap, fd, rier)
end


# AP_RPHDR -- Procedure to write the radprof banner header to the output file.

procedure ap_rphdr (ap, out)

pointer	ap		# apphot descriptor
int	out		# output file descriptor

begin
	if (out == NULL)
	    return

	# Print out the keywords.
	call ap_idhdr (ap, out)
	call ap_chdr (ap, out)
	call ap_shdr (ap, out)
	call ap_mhdr (ap, out)
	call ap_rhdr (ap, out)
end


# AP_QPRPROF -- Procedure to print a short version of the radprof results
# on the standard output.

procedure ap_qprprof (ap, cier, sier, pier, rier)

pointer	ap	# pointer to apphot structure
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# phot error
int	rier	# radprof error

pointer	sp, imname, phot
real	apstatr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	phot = AP_PPHOT(ap)

	# Print quick summary of radprof results on the standard output.
	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	call printf ("%s  %8.2f %8.2f  %8g %5.2f  ")
	    call pargstr (Memc[imname])
	    call pargr (apstatr (ap, ORPXCUR))
	    call pargr (apstatr (ap, ORPYCUR))
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, RPFWHM) / apstatr (ap, SCALE))
	call printf ("%7.3f  %s\n")
	    call pargr (Memr[AP_MAGS(phot)+AP_NAPERTS(phot)-1])
	if (cier != AP_OK || sier != AP_OK || pier != AP_OK || rier != AP_OK)
	    call pargstr ("err")
	else
	    call pargstr ("ok")

	call sfree (sp)
end
