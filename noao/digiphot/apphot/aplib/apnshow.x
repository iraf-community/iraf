include "../lib/apphot.h"
include "../lib/noise.h"

# AP_NSHOW -- Procedure to display the current data parameters.

procedure ap_nshow (ap)

pointer	ap	# pointer to the apphot structure

pointer	sp, str1, str2
bool	itob()
int	apstati()
real	apstatr()

begin
	call smark (sp)
	call salloc (str1, SZ_LINE, TY_CHAR)
	call salloc (str2, SZ_LINE, TY_CHAR)

	# Set the object charactersitics.
	call printf ("\nData Characteristics\n")
	call apstats (ap, IMNAME, Memc[str1], SZ_FNAME)
	call printf ("    %s: %s (%.2f, %.2f)   %s: %g\n")
	    call pargstr (KY_IMNAME)
	    call pargstr (Memc[str1])
	    call pargr (apstatr (ap, CWX))
	    call pargr (apstatr (ap, CWY))
	    call pargstr (KY_SCALE)
	    call pargr (1.0 / apstatr (ap, SCALE))

	call apstats (ap, OUTNAME, Memc[str1], SZ_FNAME)
	call printf ("    %s: %s")
	    call pargstr (KY_OUTNAME)
	    call pargstr (Memc[str1])

	call apstats (ap, CLNAME, Memc[str1], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_CLNAME)
	    call pargstr (Memc[str1])

	call printf ("    %s = %g %s    %s = %b\n")
	    call pargstr (KY_FWHMPSF)
	    call pargr (apstatr (ap, FWHMPSF))
	    call pargstr (UN_ASCALEUNIT)
	    call pargstr (KY_POSITIVE)
	    call pargb (itob (apstati (ap, POSITIVE)))

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_DATAMIN)
	    call pargr (apstatr (ap, DATAMIN))
	    call pargstr (UN_ACOUNTS)
	    call pargstr (KY_DATAMAX)
	    call pargr (apstatr (ap, DATAMAX))
	    call pargstr (UN_ACOUNTS)

	call apstats (ap, EXPOSURE, Memc[str1], SZ_FNAME)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_EXPOSURE)
	    call pargstr (Memc[str1])
	    call pargstr (KY_ITIME)
	    call pargr (apstatr (ap, ITIME))
	    call pargstr (UN_ATIMEUNIT)

	# Set the filter ID.
	call apstats (ap, FILTER, Memc[str1], SZ_FNAME)
	call apstats (ap, FILTERID, Memc[str2], SZ_FNAME)
	call printf ("    %s = %s    %s = %s\n")
	    call pargstr (KY_FILTER)
	    call pargstr (Memc[str1])
	    call pargstr (KY_FILTERID)
	    call pargstr (Memc[str2])

	# Set the airmass.
	call apstats (ap, AIRMASS, Memc[str1], SZ_FNAME)
	call printf ("    %s = %s    %s = %g\n")
	    call pargstr (KY_AIRMASS)
	    call pargstr (Memc[str1])
	    call pargstr (KY_XAIRMASS)
	    call pargr (apstatr (ap, XAIRMASS))

	# Set the time of observation.
	call apstats (ap, OBSTIME, Memc[str1], SZ_FNAME)
	call apstats (ap, OTIME, Memc[str2], SZ_FNAME)
	call printf ("    %s = %s    %s = %s\n")
	    call pargstr (KY_OBSTIME)
	    call pargstr (Memc[str1])
	    call pargstr (KY_OTIME)
	    call pargstr (Memc[str2])

	# Set the noise model.
	call printf ("\nNoise Model\n")
	call apstats (ap, NSTRING, Memc[str1], SZ_FNAME)
	call printf ("    %s = %s  %s    %s = %g %s\n")
	    call pargstr (KY_NSTRING)
	    call pargstr (Memc[str1])
	    call pargstr (UN_NMODEL)
	    call pargstr (KY_SKYSIGMA)
	    call pargr (apstatr (ap, SKYSIGMA))
	    call pargstr (UN_NCOUNTS)

	call apstats (ap, GAIN, Memc[str1], SZ_LINE)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_GAIN)
	    call pargstr (Memc[str1])
	    call pargstr (KY_EPADU)
	    call pargr (apstatr (ap, EPADU))
	    call pargstr (UN_NEPADU)

	call apstats (ap, CCDREAD, Memc[str1], SZ_LINE)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_CCDREAD)
	    call pargstr (Memc[str1])
	    call pargstr (KY_READNOISE)
	    call pargr (apstatr (ap, READNOISE))
	    call pargstr (UN_NELECTRONS)

	call sfree (sp)
end
