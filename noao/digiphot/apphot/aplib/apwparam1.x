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
int	out		# output file descriptor
char	task[ARB]	# task name

int	nchars
pointer	sp, outstr, date, time
bool	itob()
int	strmatch(), envfind(), gstrcpy(), apstati()
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
	call ap_rmwhite (Memc[outstr], Memc[outstr], SZ_LINE)
	call ap_sparam (out, "IRAF", Memc[outstr], "version",
	    "current version of IRAF")

	nchars = envfind ("userid", Memc[outstr], SZ_LINE)
	call ap_sparam (out, "USER", Memc[outstr], "name", "user id")

	call gethost (Memc[outstr], SZ_LINE)
	call ap_sparam (out, "HOST", Memc[outstr], "computer",
	    "IRAF host machine")

	call apdate (Memc[date], Memc[time], SZ_DATE)
	call ap_sparam (out, "DATE", Memc[date], "yyyy-mm-dd", "date")
	call ap_sparam (out, "TIME", Memc[time], "hh:mm:ss", "time")

	call ap_sparam (out, "PACKAGE", "apphot", "name",
	    "name of IRAF package")
	call ap_sparam (out, "TASK", task, "name", "name of apphot task")
	call fprintf (out, "#\n")

	# Write out the apphot parameters.
	call ap_rparam (out, KY_SCALE, 1.0 / apstatr (ap, SCALE), UN_AUNITS,
	    "scale in units per pixel")
	call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF), UN_ASCALEUNIT,
	    "full width half maximum of the psf")
	call ap_bparam (out, KY_POSITIVE, itob (apstati (ap, POSITIVE)),
	    UN_ASWITCH, "positive feature")
	call ap_rparam (out, KY_DATAMIN, apstatr (ap, DATAMIN), UN_ACOUNTS,
	    "minimum good data value")
	call ap_rparam (out, KY_DATAMAX, apstatr (ap, DATAMAX), UN_ACOUNTS,
	    "maximum good data value")

	# Write out the image header keyword parameters.
	call apstats (ap, EXPOSURE, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_EXPOSURE, Memc[outstr], UN_AKEYWORD,
	    "exposure time keyword")
	call apstats (ap, AIRMASS, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_AIRMASS, Memc[outstr], UN_AKEYWORD,
	    "airmass keyword")
	call apstats (ap, FILTER, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_FILTER, Memc[outstr], UN_AKEYWORD,
	    "filter keyword")
	call apstats (ap, OBSTIME, Memc[outstr], SZ_FNAME)
	if (Memc[outstr] == EOS)
	    call strcpy ("\"\"", Memc[outstr], SZ_FNAME)
	call ap_sparam (out, KY_OBSTIME, Memc[outstr], UN_AKEYWORD,
	    "obstime keyword")
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
int	out		# output file descriptor

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
	    call ap_sparam (out, KY_CSTRING, Memc[str], UN_CALGORITHM,
		"centering algorithm")
	    call ap_rparam (out, KY_CAPERT, 2.0* apstatr (ap, CAPERT),
	        UN_CSCALEUNIT, "centering box width")
	    call ap_rparam (out, KY_CTHRESHOLD, apstatr (ap, CTHRESHOLD),
	        UN_CSIGMA, "threshold for centering")
	    call ap_rparam (out, KY_MINSNRATIO, apstatr (ap, MINSNRATIO),
		UN_CNUMBER, "minimum signal to noise ratio")
	    call ap_iparam (out, KY_CMAXITER, apstati (ap, CMAXITER),
	        UN_CNUMBER, "maximum number of iterations")
	    call ap_rparam (out, KY_MAXSHIFT, apstatr (ap, MAXSHIFT),
	        UN_CSCALEUNIT, "maximum shift")
	    call ap_bparam (out, KY_CLEAN, itob (apstati (ap, CLEAN)),
		UN_CSWITCH, "apply clean algorithm before centering")
	    call ap_rparam (out, KY_RCLEAN, apstatr (ap, RCLEAN),
	        UN_CSCALEUNIT, "cleaning radius")
	    call ap_rparam (out, KY_RCLIP, apstatr (ap, RCLIP), UN_CSCALEUNIT,
		"clipping radius")
	    call ap_rparam (out, KY_SIGMACLEAN, apstatr (ap, SIGMACLEAN),
	        UN_CSIGMA, "k-sigma clean rejection criterion")
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
	    call ap_sparam (out, KY_SSTRING, Memc[str], UN_SALGORITHM,
		" sky fitting algorithm")
	    call ap_rparam (out, KY_ANNULUS, apstatr (ap, ANNULUS),
	        UN_SSCALEUNIT, "inner radius of sky annulus")
	    call ap_rparam (out, KY_DANNULUS, apstatr (ap, DANNULUS),
	        UN_SSCALEUNIT, "width of the sky annulus")
	    call ap_rparam (out, KY_SKY_BACKGROUND, apstatr (ap,
	        SKY_BACKGROUND), UN_SCOUNTS, "user supplied sky value")
	    call ap_rparam (out, KY_K1, apstatr (ap, K1), UN_SSIGMA,
		"half width of sky histogram")
	    call ap_rparam (out, KY_BINSIZE, apstatr (ap, BINSIZE),
	        UN_SSIGMA, "width of sky histogram bin")
	    call ap_bparam (out, KY_SMOOTH, itob (apstati (ap, SMOOTH)),
		UN_SSWITCH, "Lucy smooth the histogram")
	    call ap_iparam (out, KY_SMAXITER, apstati (ap, SMAXITER),
	        UN_SNUMBER, "maximum number of iterations")
	    call ap_rparam (out, KY_SLOCLIP, apstatr (ap, SLOCLIP),
	        UN_SPERCENT, "lower clipping limit")
	    call ap_rparam (out, KY_SHICLIP, apstatr (ap, SHICLIP),
	        UN_SPERCENT, "upper clipping limit")
	    call ap_iparam (out, KY_SNREJECT, apstati (ap, SNREJECT),
	        UN_SNUMBER, "maximum number of rejection cycles")
	    call ap_rparam (out, KY_SLOREJECT, apstatr (ap, SLOREJECT),
	        UN_SSIGMA, "lower k-sigma rejection criterion")
	    call ap_rparam (out, KY_SHIREJECT, apstatr (ap, SHIREJECT),
	        UN_SSIGMA, "upper k-sigma rejection criterion")
	    call ap_rparam (out, KY_RGROW, apstatr (ap, RGROW), UN_SSCALEUNIT,
		"region growing radius")
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
	    call ap_sparam (out, KY_NSTRING, Memc[str], UN_NMODEL,
	        "noise model")
	    call ap_rparam (out, KY_SKYSIGMA, apstatr (ap, SKYSIGMA),
	        UN_NCOUNTS, "standard deviation of 1 sky pixel")
	    call apstats (ap, GAIN, Memc[str], SZ_FNAME)
	    if (Memc[str] == EOS)
	        call strcpy ("\"\"", Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_GAIN, Memc[str], UN_NKEYWORD,
	        "gain keyword")
	    call ap_rparam (out, KY_EPADU, apstatr (ap, EPADU), UN_NEPADU,
	        "electrons per adu")
	    call apstats (ap, CCDREAD, Memc[str], SZ_FNAME)
	    if (Memc[str] == EOS)
	        call strcpy ("\"\"", Memc[str], SZ_FNAME)
	    call ap_sparam (out, KY_CCDREAD, Memc[str], UN_NKEYWORD,
	        "read noise keyword")
	    call ap_rparam (out, KY_READNOISE, apstatr (ap, READNOISE),
	        UN_NELECTRONS, "electrons")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_WWPHOT -- Procedure to write out the photometry parameters.

procedure ap_wwphot (ap, out)

pointer	ap		# apphot structure pointer
int	out		# output file descriptor

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (AP_PPHOT(ap) != NULL) {
	    call apstats (ap, PWSTRING, Memc[str], SZ_LINE)
	    call ap_sparam (out, KY_PWSTRING, Memc[str], UN_PMODEL,
		"photometric weighting scheme")
	    call apstats (ap, APERTS, Memc[str], SZ_LINE)
	    call ap_sparam (out, KY_APERTS, Memc[str], UN_PSCALEUNIT,
		"list of apertures")
	    call ap_rparam (out, KY_ZMAG, apstatr (ap, ZMAG), UN_PZMAG,
		"zero point of magnitdue scale")
	    call fprintf (out, "#\n")
	}

	call sfree (sp)
end


# AP_RPARAM -- Procedure to encode a real apphot parameter.

procedure ap_rparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
real	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23.7g%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargr (value)
	    call pargstr (units)
	    call pargstr ("%-23.7g")
	    call pargstr (comments)
end


# AP_IPARAM -- Procedure to encode an apphot integer parameter.

procedure ap_iparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
int	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23d%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargi (value)
	    call pargstr (units)
	    call pargstr ("%-23d")
	    call pargstr (comments)
end


# AP_BPARAM -- Procedure to encode an apphot boolean parameter.

procedure ap_bparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
bool	value		# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23b%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargb (value)
	    call pargstr (units)
	    call pargstr ("%-23b")
	    call pargstr (comments)
end


# AP_SPARAM -- Procedure to encode an apphot string parameter.

procedure ap_sparam (out, keyword, value, units, comments)

int	out		# output file descriptor
char	keyword[ARB]	# keyword string
char	value[ARB]	# parameter value
char	units[ARB]	# units string
char	comments[ARB]	# comment string

begin
	if (out == NULL)
	    return

	call strupr (keyword)
        call fprintf (out,
	    "#K%4t%-10.10s%14t = %17t%-23.23s%41t%-10.10s%52t%-10s\n")
	    call pargstr (keyword)
	    call pargstr (value)
	    call pargstr (units)
	    call pargstr ("%-23s")
	    call pargstr (comments)
end
