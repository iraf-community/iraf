include "../lib/apphot.h"
include "../lib/noise.h"

# AP_NSHOW -- Procedure to display the current data parameters.

procedure ap_nshow (ap)

pointer	ap	# pointer to the apphot strucuture

pointer	sp, str
int	apstati(), itob()
real	apstatr()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set the object charactersitics.
	call printf ("\nData Characteristics\n")
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	call printf ("    %s: %s (%.2f, %.2f)   %s: %g\n")
	    call pargstr (KY_IMNAME)
	    call pargstr (Memc[str])
	    call pargr (apstatr (ap, CWX))
	    call pargr (apstatr (ap, CWY))
	    call pargstr (KY_SCALE)
	    call pargr (1.0 / apstatr (ap, SCALE))

	call apstats (ap, OUTNAME, Memc[str], SZ_FNAME)
	call printf ("    %s: %s")
	    call pargstr (KY_OUTNAME)
	    call pargstr (Memc[str])

	call apstats (ap, CLNAME, Memc[str], SZ_FNAME)
	call printf ("    %s: %s\n")
	    call pargstr (KY_CLNAME)
	    call pargstr (Memc[str])

	call printf ("    %s = %g %s    %s = %b\n")
	    call pargstr (KY_FWHMPSF)
	    call pargr (apstatr (ap, FWHMPSF))
	    call pargstr (UN_FWHMPSF)
	    call pargstr (KY_POSITIVE)
	    call pargb (itob (apstati (ap, POSITIVE)))

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_DATAMIN)
	    call pargr (apstatr (ap, DATAMIN))
	    call pargstr (UN_DATAMIN)
	    call pargstr (KY_DATAMAX)
	    call pargr (apstatr (ap, DATAMAX))
	    call pargstr (UN_DATAMAX)

	call apstats (ap, EXPOSURE, Memc[str], SZ_FNAME)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_EXPOSURE)
	    call pargstr (Memc[str])
	    call pargstr (KY_ITIME)
	    call pargr (apstatr (ap, ITIME))
	    call pargstr (UN_ITIME)

	# Set the noise model.
	call printf ("\nNoise Model\n")
	call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	call printf ("    %s = %s  %s    %s = %g %s\n")
	    call pargstr (KY_NSTRING)
	    call pargstr (Memc[str])
	    call pargstr (UN_NSTRING)
	    call pargstr (KY_SKYSIGMA)
	    call pargr (apstatr (ap, SKYSIGMA))
	    call pargstr (UN_SKYSIGMA)

	call printf ("    %s = %g %s    %s = %g %s\n")
	    call pargstr (KY_THRESHOLD)
	    call pargr (apstatr (ap, THRESHOLD))
	    call pargstr (UN_THRESHOLD)
	    call pargstr (KY_CTHRESHOLD)
	    call pargr (apstatr (ap, CTHRESHOLD))
	    call pargstr (UN_CTHRESHOLD)

	call apstats (ap, GAIN, Memc[str], SZ_LINE)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_GAIN)
	    call pargstr (Memc[str])
	    call pargstr (KY_EPADU)
	    call pargr (apstatr (ap, EPADU))
	    call pargstr (UN_EPADU)

	call apstats (ap, CCDREAD, Memc[str], SZ_LINE)
	call printf ("    %s = %s    %s = %g %s\n")
	    call pargstr (KY_CCDREAD)
	    call pargstr (Memc[str])
	    call pargstr (KY_READNOISE)
	    call pargr (apstatr (ap, READNOISE))
	    call pargstr (UN_READNOISE)

	call sfree (sp)
end
