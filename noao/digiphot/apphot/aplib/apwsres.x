include "../lib/fitsky.h"

# define the #N, #U and #K fitsky strings

define	SKY_NSTR  "#N%4tMSKY%19tSTDEV%34tSSKEW%49tNSKY%56tNSREJ%62tSIER%67tSERROR%80t\\\n"
define	SKY_USTR  "#U%4tcounts%19tcounts%34tcounts%49tnpix%56tnpix%62t##%67tserrors%80t\\\n"
define	SKY_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-15.7g%49t%%-7d%56t%%-6d%62t%%-5d%67t%%-13s%80t \n"
define  SKY_WSTR  "%4t%-15.7g%-15.7g%-15.7g%-7d%-6d%-5d%-13s%80t%c\n"


# AP_SHDR -- Print the fitsky header banner strings.

procedure ap_shdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, SKY_NSTR)
	call fprintf (fd, SKY_USTR)
	call fprintf (fd, SKY_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WSRES -- Procedure to write the results of the fitsky task to
# the output file.

procedure ap_wsres (ap, fd, ier, lastchar)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor
int	ier		# error code
int	lastchar	# last character

int	apstati()
real	apstatr()

pointer	sp, str

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call ap_sserrors (ier, Memc[str], SZ_LINE)

	# Print the computed sky value and statistics.
	call fprintf (fd, SKY_WSTR)
	    call pargr (apstatr (ap, SKY_MODE))
	    call pargr (apstatr (ap, SKY_SIGMA))
	    call pargr (apstatr (ap, SKY_SKEW))
	    call pargi (apstati (ap, NSKY))
	    call pargi (apstati (ap, NSKY_REJECT))
	    call pargi (ier)
	    call pargstr (Memc[str])
	    call pargi (lastchar)

	call sfree (sp)
end


# AP_SSERRORS -- Program to encode detailed fitsky error messages in a string.

procedure ap_sserrors (ier, str, maxch)

int	ier		# integer error code
char	str[ARB]	# the output string
int	maxch		# maximum number of characters

begin
	switch (ier) {
	case AP_NOSKYAREA:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_sky_area")
	case AP_SKY_OUTOFBOUNDS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Off_image")
	case AP_NOHISTOGRAM:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_histogram")
	case AP_FLAT_HIST:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Flat_histogram")
	case AP_NSKY_TOO_SMALL:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Npts_too_few")
	case AP_SKY_SINGULAR:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Singular")
	case AP_SKY_NOCONVERGE:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_converge")
	case AP_NOGRAPHICS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_graphics")
	case AP_NOSKYFILE:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_sky_file")
	case AP_EOFSKYFILE:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Sky_file_at_EOF")
	case AP_BADSKYSCAN:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Cannot_decode_sky")
	case AP_BADPARAMS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Bad_parameters")
	default:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_error")
	}
end
