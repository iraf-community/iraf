include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"
include "../lib/apphot.h"
include "../lib/fitpsf.h"

# AP_PPSF -- Procedure to write the results of fitpsf to the output file.

procedure ap_ppsf (ap, fd, id, lid, ier)

pointer	ap	# pointer to apphot structure
int	fd	# output file descriptor
int	id	# sequence number of star
int	lid	# list id of star
int	ier	# comment string

real	apstatr()

begin
	# Initialize.
	if (fd == NULL)
	    return

	# Print the stars id.
	call ap_wid (ap, fd, apstatr (ap, OPFXCUR), apstatr (ap, OPFYCUR), id,
	    lid, '\\')

	# Print the parameters.
	call ap_wfres (ap, fd, ier)
end


# AP_QPPSF -- Procedure to print the results of fitpsf on the standard output
# in short form.

procedure ap_qppsf (ap, ier)

pointer	ap	# pointer to apphot structure
int	ier	# comment string

pointer	sp, imname, psf

begin
	# Initialize.
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	psf = AP_PPSF(ap)

	# Print the parameters on the standard output.
	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	switch (AP_PSFUNCTION(psf)) {
	case AP_RADGAUSS:
	    call printf (
	    "%s  %8.2f %8.2f  %6.3f %8g %8g  %s\n") 
		call pargstr (Memc[imname])
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+4])
		if (ier != AP_OK)
		    call pargstr ("err")
		else
		    call pargstr ("ok")
	case AP_ELLGAUSS:
	    call printf ("%s  %8.2f %8.2f  %6.3f %6.3f  ")
		call pargstr (Memc[imname])
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)+4])
	    call printf ("%6.1f %8g %8g  %s\n")
		call pargr (Memr[AP_PPARS(psf)+5])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+6])
		if (ier != AP_OK)
		    call pargstr ("err")
		else
		    call pargstr ("ok")
	case AP_MOMENTS:
	    call printf ("%s  %8.2f %8.2f  %6.3f %6.3f ")
		call pargstr (Memc[imname])
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)+4])
	    call printf ("%6.1f  %8g %8g  %s\n")
		call pargr (Memr[AP_PPARS(psf)+5])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+6])
		if (ier != AP_OK)
		    call pargstr ("err")
		else
		    call pargstr ("ok")
	}

	call sfree (sp)
end


# AP_PFHDR -- Procedure to write the banner for the fitpsf output file.

procedure ap_pfhdr (ap, fd)

pointer	ap		# pointer to the apphot strucuture
int	fd		# output file descriptor

int	apstati()

begin
	if (fd == NULL)
	    return

	switch (apstati (ap, PSFUNCTION)) {
	case AP_RADGAUSS:
	    call radhdr (ap, fd)
	case AP_ELLGAUSS:
	    call elhdr (ap, fd)
	case AP_MOMENTS:
	    call momhdr (ap, fd)
	default:
	    ;
	}
end
