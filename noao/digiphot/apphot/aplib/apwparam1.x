include <time.h>
include "../lib/apphotdef.h"
include "../lib/apphot.h"
include "../lib/noise.h"
include	"../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"

# AP_PARAM -- Procedure to write the apphot parameters to a text file.

procedure ap_param (ap, out, task)

pointer	ap		# pointer to the apphot structure
pointer	out		# database descriptor
char	task[ARB]	# task name

int	nchars
pointer	sp, outstr, date, time
int	strmatch(), envfind(), gstrcpy(), apstati(), itob()
real	apstatr()

begin
	if (out == NULL)
	    return
	 
	# Allocate working space.
	call smark (sp)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)

	# Write the id.
	nchars = envfind ("version", Memc[outstr], SZ_LINE)
	if (nchars <= 0)
	    nchars = gstrcpy ("NOAO/IRAF", Memc[outstr], SZ_LINE)
	call ap_sparam (out, "IRAF", Memc[outstr], "version",
	    "current version of IRAF")
	nchars = envfind ("userid", Memc[outstr], SZ_LINE)
	call ap_sparam (out, "USER", Memc[outstr], "name", "user id")
	call gethost (Memc[outstr], SZ_LINE)
	call ap_sparam (out, "HOST", Memc[outstr], "computer",
	    "IRAF host machine")
	call apdate (Memc[date], Memc[time], SZ_DATE)
	call ap_sparam (out, "DATE", Memc[date], "mm-dd-yr", "date")
	call ap_sparam (out, "TIME", Memc[time], "hh:mm:ss", "time")
	call ap_sparam (out, "PACKAGE", "apphot", "name",
	    "name of IRAF package")
	call ap_sparam (out, "TASK", task, "name", "name of apphot task")
	call fprintf (out, "#\n")

	# Write out the apphot parameters.
	call ap_rparam (out, KY_SCALE, 1.0 / apstatr (ap, SCALE), UN_SCALE,
	    "scale in units per pixel")
	call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF), UN_FWHMPSF,
	    "full width half maximum of the psf")
	call ap_bparam (out, KY_POSITIVE, itob (apstati (ap, POSITIVE)),
	    UN_POSITIVE, "positive feature")
	call ap_rparam (out, KY_DATAMIN, apstatr (ap, DATAMIN), UN_DATAMIN,
	    "minimum good data value")
	call ap_rparam (out, KY_DATAMAX, apstatr (ap, DATAMAX), UN_DATAMAX,
	    "maximum good data value")
	call apstats (ap, EXPOSURE, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_EXPOSURE, Memc[outstr], UN_EXPOSURE,
	    "exposure time keyword")
	call apstats (ap, AIRMASS, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_AIRMASS, Memc[outstr], UN_AIRMASS,
	    "airmass keyword")
	call apstats (ap, FILTER, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_FILTER, Memc[outstr], UN_FILTER,
	    "filter keyword")
	call fprintf (out, "#\n")

	# Write the noise model parameters.
	call ap_wnse (ap, out)

	# Write the centering parameters.
	call ap_wctrs (ap, out)

	# Write sky fitting parameters.
	call ap_wskys (ap, out)

	# Write the phot parameters.
	call ap_wwphot (ap, out)

	# Write the polyphot parameters.
	call ap_wpoly (ap, out)

	# Write the radial profile parameters.
	call ap_wprofs (ap, out)

	# Write the psf fitting parameters.
	call ap_wpsf (ap, out)

	# Write the header banner
	if (strmatch ("^center", task) > 0)
	    call ap_cphdr (ap, out)
	else if (strmatch ("^fitsky", task) > 0)
	    call ap_sphdr (ap, out)
	else if (strmatch ("^phot", task) > 0)
	    call ap_maghdr (ap, out)
	else if (strmatch ("^qphot", task) > 0)
	    call ap_maghdr (ap, out)
	else if (strmatch ("^wphot", task) > 0)
	    call ap_maghdr (ap, out)
	else if (strmatch ("^polyphot", task) > 0)
	    call ap_yhdr (ap, out)
	else if (strmatch ("^fitpsf", task) > 0)
	    call ap_pfhdr (ap, out)
	else if (strmatch ("^radprof", task) > 0)
	    call ap_rphdr (ap, out)
	#else if (strmatch ("^daofind", task) > 0)
	    #call ap_fdhdr (ap, out)

	call sfree (sp)
end


# AP_WCTRS -- Procedure to write out the centering parameters.

procedure ap_wctrs (ap, out)

pointer	ap		# apphot pointer
pointer	out		# output file descriptor

pointer	sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_PCENTER(ap) != NULL) {
	    call apstats (ap, CSTRING, Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_CSTRING, Memc[str], UN_CSTRING,
		"centering algorithm")
	    call ap_rparam (out, KY_CAPERT, 2.0* apstatr (ap, CAPERT),
	        UN_CAPERT, "centering box width")
	    call ap_rparam (out, KY_MAXSHIFT, apstatr (ap, MAXSHIFT),
	        UN_MAXSHIFT, "maximum shift")
	    call ap_iparam (out, KY_CMAXITER, apstati (ap, CMAXITER),
	        UN_CMAXITER, "maximum number of iterations")
	    call ap_rparam (out, KY_MINSNRATIO, apstatr (ap, MINSNRATIO),
		UN_MINSNRATIO, "minimum signal to noise ratio")
	    call ap_bparam (out, KY_CLEAN, itob (apstati (ap, CLEAN)),
		UN_CLEAN, "apply clean algorithm before centering")
	    call ap_rparam (out, KY_RCLEAN, apstatr (ap, RCLEAN), UN_RCLEAN,
		"cleaning radius")
	    call ap_rparam (out, KY_RCLIP, apstatr (ap, RCLIP), UN_RCLIP,
		"clipping radius")
	    call ap_rparam (out, KY_SIGMACLEAN, apstatr (ap, SIGMACLEAN),
	        UN_SIGMACLEAN, "k-sigma clean rejection criterion")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WSKYS -- Procedure to write out the sky fitting  parameters.

procedure ap_wskys (ap, out)

pointer	ap		# apphot structure
int	out		# output pointer

pointer sp, str
bool	itob()
int	apstati()
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_PSKY(ap) != NULL) {
	    call apstats (ap, SSTRING, Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_SSTRING, Memc[str], UN_SSTRING,
		" sky fitting algorithm")
	    call ap_rparam (out, KY_ANNULUS, apstatr (ap, ANNULUS), UN_ANNULUS,
		"inner radius of sky annulus")
	    call ap_rparam (out, KY_DANNULUS, apstatr (ap, DANNULUS),
	        UN_DANNULUS, "width of the sky annulus")
	    call ap_iparam (out, KY_SMAXITER, apstati (ap, SMAXITER),
	        UN_SMAXITER, "maximum number of iteration")
	    call ap_rparam (out, KY_K2, apstatr (ap, K2), UN_K2,
		"k-sigma rejection criterion")
	    call ap_iparam (out, KY_SNREJECT, apstati (ap, SNREJECT),
	        UN_SNREJECT, "maximum number of rejection cycles")
	    call ap_rparam (out, KY_K1, apstatr (ap, K1), UN_K1,
		"half width of sky histogram")
	    call ap_rparam (out, KY_BINSIZE, apstatr (ap, BINSIZE),
	        UN_BINSIZE, "width of sky histogram bin")
	    call ap_bparam (out, KY_SMOOTH, itob (apstati (ap, SMOOTH)),
		UN_SMOOTH, "Lucy smooth the histogram")
	    call ap_rparam (out, KY_RGROW, apstatr (ap, RGROW), UN_RGROW,
		"region growing radius")
	    call ap_rparam (out, KY_SKY_BACKGROUND, apstatr (ap,
	        SKY_BACKGROUND), UN_SKY_BACKGROUND, "user supplied sky value")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WNSE -- Porcedure to compute the noise model parameters.

procedure ap_wnse (ap, out)

pointer	ap		# apphot pointer
int	out		# output file descriptor

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	if (AP_NOISE(ap) != NULL) {
	    call apstats (ap, NSTRING, Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_NSTRING, Memc[str], UN_NSTRING,
	        "noise model")
	    call ap_rparam (out, KY_THRESHOLD, apstatr (ap, THRESHOLD),
	        UN_THRESHOLD, "threshold")
	    call ap_rparam (out, KY_CTHRESHOLD, apstatr (ap, CTHRESHOLD),
	        UN_CTHRESHOLD, "threshold for centering")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
	        UN_SKYSIGMA, "standard deviation of 1 sky pixel")
	    call apstats (ap, GAIN, Memc[str], SZ_FNAME)
	    if (Memc[str] == EOS)
	        call strcpy ("\"\"", Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_GAIN, Memc[str], UN_GAIN, "gain keyword")
	    call ap_rparam (out, KY_EPADU, apstatr (ap, EPADU), UN_EPADU,
	        "electrons per adu")
	    call apstats (ap, CCDREAD, Memc[str], SZ_FNAME)
	    if (Memc[str] == EOS)
	        call strcpy ("\"\"", Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_CCDREAD, Memc[str], UN_CCDREAD,
	        "read noise keyword")
	    call ap_rparam (out, KY_READNOISE, apstatr (ap, READNOISE),
	        UN_READNOISE, "electrons")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WWPHOT -- Procedure to write out the photometry parameters.

procedure ap_wwphot (ap, out)

pointer	ap		# apphot structure pointer
pointer	out		# output file descriptor

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (AP_PPHOT(ap) != NULL) {
	    call apstats (ap, PWSTRING, Memc[str], SZ_LINE)
	    call ap_sparam (out, KY_PWSTRING, Memc[str], UN_PWSTRING,
		"photometric weighting scheme")
	    call apstats (ap, APERTS, Memc[str], SZ_LINE)
	    call ap_sparam (out, KY_APERTS, Memc[str], UN_APERTS,
		"list of apertures")
	    call ap_rparam (out, KY_ZMAG, apstatr (ap, ZMAG), UN_ZMAG,
		"zero point of magnitdue scale")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_RPARAM -- Procedure to encode a real apphot parameter.

procedure ap_rparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
real	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-15.7g%32t%-10.10s%42t%-10s\n")
	    call pargstr (keyword)
	    call pargr (value)
	    call pargstr (units)
	    call pargstr ("%-15.7g")
	    call pargstr (comments)
end


# AP_IPARAM -- Procedure to encode an apphot integer parameter.

procedure ap_iparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
int	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-15d%32t%-10.10s%42t%-10s\n")
	    call pargstr (keyword)
	    call pargi (value)
	    call pargstr (units)
	    call pargstr ("%-15d")
	    call pargstr (comments)
end


# AP_BPARAM -- Procedure to encode an apphot boolean parameter.

procedure ap_bparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
bool	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-15b%32t%-10.10s%42t%-10s\n")
	    call pargstr (keyword)
	    call pargb (value)
	    call pargstr (units)
	    call pargstr ("%-15b")
	    call pargstr (comments)
end


# AP_SPARAM -- Procedure to encode an apphot string parameter.

procedure ap_sparam (out, keyword, value, units, comments)

pointer	out		# output file descriptor
char	keyword[ARB]	# keyword string
char	value[ARB]	# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-15.15s%32t%-10.10s%42t%-10s\n")
	    call pargstr (keyword)
	    call pargstr (value)
	    call pargstr (units)
	    call pargstr ("%-15s")
	    call pargstr (comments)
end
