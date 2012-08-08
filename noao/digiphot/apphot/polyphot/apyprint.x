include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/polyphot.h"

# AP_YPRINT -- Write the results of the polyphot task to the output file.

procedure ap_yprint (py, fd, xver, yver, nver, id, lid, pid, cier, sier, pier)

pointer	py		# pointer to apphot structure
int	fd		# output file descriptor
real	xver[ARB]	# coords of x vertices
real	yver[ARB]	# coords of y vertices
int	nver		# number of vertices
int	id		# id number of str
int	lid		# list id number of star
int	pid		# polygon number
int	cier		# centering error
int	sier		# sky fitting error
int	pier		# photometric error

real	apstatr()

begin
	if (fd == NULL)
	    return

	# Write object id parameters.
	call ap_wid (py, fd, apstatr (py, OXINIT), apstatr (py, OYINIT), id,
	    lid, '\\')

	# Write the centering parameters
	call ap_wcres (py, fd, cier, '\\')

	# Write the fitsky parameters.
	call ap_wsres (py, fd, sier, '\\')

	# Write the polyphot parameters
	call ap_wlres (py, fd, xver, yver, nver, pid, pier)
end


# AP_QYPRINT -- Print a quick summary of the polyphot results on the standard
# output.

procedure ap_qyprint (py, cier, sier, pier)

pointer	py	# pointer to apphot structure
int	cier	# centering error
int	sier	# sky fitting error
int	pier	# photometry error

pointer	sp, imname
real	apstatr()

begin
	# Allocate space.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# Print polyphot magnitudes.
	#call apstats (py, IMNAME, Memc[imname], SZ_FNAME)
	call apstats (py, IMROOT, Memc[imname], SZ_FNAME)
	call printf ("%s  %8.2f %8.2f  %8g  ")
	    call pargstr (Memc[imname])
	    call pargr (apstatr (py, OPYCX))
	    call pargr (apstatr (py, OPYCY))
	    call pargr (apstatr (py, SKY_MODE))
	call printf ("%7.3f  %s\n")
	    call pargr (apstatr (py, PYMAG))
	if (cier != AP_OK || sier != AP_OK || pier != AP_OK)
	    call pargstr ("err")
	else
	    call pargstr ("ok")

	call sfree (sp)
end


# AP_YHDR -- Write the polyphot header banner to the output file.

procedure ap_yhdr (ap, fd)

pointer	ap		# apphot structure
int 	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call ap_idhdr (ap, fd)
	call ap_chdr (ap, fd)
	call ap_shdr (ap, fd)
	call ap_plhdr (ap, fd)
end
