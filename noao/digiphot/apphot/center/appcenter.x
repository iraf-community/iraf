include "../lib/apphot.h"
include "../lib/center.h"

# AP_PCENTER -- Procedure to write the output of centering task to a file.

procedure ap_pcenter (ap, fd, id, lid, ier)

pointer	ap	# pointer to apphot structure
int	fd	# output file descriptor
int	id	# id of the star
int	lid	# list number
int	ier	# comment string

real	xpos, ypos
real	apstatr()

begin
	if (fd == NULL)
	    return

	# Print description of object.
	xpos = apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)
	ypos = apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)
	call ap_wid (ap, fd, xpos, ypos, id, lid, '\\')

	# Print the computed centers.
	call ap_wcres (ap, fd, ier, ' ')
end


# AP_QCENTER -- Procedure to print a quick summary of the center task output
# on the standard output.

procedure ap_qcenter (ap, ier)

pointer	ap	# pointer to apphot structure
int	ier	# comment string

pointer	sp, imname
real	apstatr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	call apstats (ap, IMNAME, Memc[imname], SZ_FNAME)
	call printf ( "%s x: %0.2f y: %0.2f xc: %0.2f yc: %0.2f ")
	    call pargstr (Memc[imname])
	    call pargr ((apstatr (ap, XCENTER) - apstatr (ap, XSHIFT)))
	    call pargr ((apstatr (ap, YCENTER) - apstatr (ap, YSHIFT)))
	    call pargr (apstatr (ap, XCENTER))
	    call pargr (apstatr (ap, YCENTER))
	call printf ("xe: %0.2f ye: %0.2f e: %s\n")
	    call pargr (apstatr (ap, XERR))
	    call pargr (apstatr (ap, YERR))
	    if (ier != AP_OK)
		call pargstr ("err")
	    else
		call pargstr ("ok")

	call sfree (sp)
end


# AP_CPHDR -- Procedure to write the banner for the center task to the
# output file.

procedure ap_cphdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call ap_idhdr (ap, fd)
	call ap_chdr (ap, fd)
end
