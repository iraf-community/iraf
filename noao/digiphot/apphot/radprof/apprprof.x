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
real	xpos, ypos
int	apstati()
real	apstatr()

begin
	if (fd == NULL)
	    return

	# Print the id parameters.
	xpos = apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)
	ypos = apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)
	call ap_wid (ap, fd, xpos, ypos, id, lid, '\\')

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
pointer	out		# output file descriptor

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


# AP_QPRPROF -- Procedure to print short version of the radprof results on
# the standard output.

procedure ap_qprprof (ap, cier, sier, pier, rier)

pointer	ap	# pointer to apphot structure
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# phot error
int	rier	# radprof error

pointer	sp, imname, phot
real	apstatr()

begin
	# Print quick summary of radprof results on the standard output.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	phot = AP_PPHOT(ap)

	call apstats (ap, IMNAME, Memc[imname], SZ_FNAME)
	call printf ("%s x: %0.2f y: %0.2f s: %0.2f fwhm: %0.2f ")
	    call pargstr (Memc[imname])
	    call pargr (apstatr (ap, RPXCUR))
	    call pargr (apstatr (ap, RPYCUR))
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, RPFWHM))

	call printf ("mag: %0.2f err: %s\n")
	    call pargr (Memr[AP_MAGS(phot)+AP_NAPERTS(phot)-1])
	if (cier != AP_OK || sier != AP_OK || pier != AP_OK || rier != AP_OK)
	    call pargstr ("err")
	else
	    call pargstr ("ok")

	call sfree (sp)
end
