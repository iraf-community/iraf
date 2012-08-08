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
	xpos = apstatr (ap, OXINIT)
	ypos = apstatr (ap, OYINIT)
	call ap_wid (ap, fd, xpos, ypos, id, lid, '\\')

	# Print the computed centers.
	call ap_wcres (ap, fd, ier, ' ')
end


# AP_QCENTER -- Procedure to print a quick summary of the center task output
# on the standard output.

procedure ap_qcenter (ap, ier)

pointer	ap	# pointer to apphot structure
int	ier	# comment string

real	owx, owy, wx, wy
pointer	sp, imname
real	apstatr()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	owx = apstatr (ap, OXINIT)
	owy = apstatr (ap, OYINIT)
	wx = apstatr (ap, OXCENTER)
	wy = apstatr (ap, OYCENTER)

	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	call printf ( "%s  %7.2f %7.2f  %7.2f %7.2f  ")
	    call pargstr (Memc[imname])
	    call pargr (owx)
	    call pargr (owy)
	    call pargr (wx)
	    call pargr (wy)
	call printf ("%6.2f %6.2f  %s\n")
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
