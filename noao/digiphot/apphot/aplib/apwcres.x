include "../lib/center.h"

# define the #N, #U and #K center strings

define	CTR_NSTR  "#N%4tXCENTER%13tYCENTER%22tXSHIFT%29tYSHIFT%36tXERR%43tYERR%50tCIER%55tCERROR%80t\\\n"
define	CTR_USTR  "#U%4tpixels%13tpixels%22tpixels%29tpixels%36tpixels%43tpixels%50t##%55tcerrors%80t\\\n"
define	CTR_FSTR  "#F%4t%%-12.2f%13t%%-9.2f%22t%%-7.2f%29t%%-7.2f%36t%%-7.2f%43t%%-7.2f%50t%%-5d%55t%%-13s%80t \n"
define  CTR_WSTR   "%4t%-9.2f%-9.2f%-7.2f%-7.2f%-7.2f%-7.2f%-5d%-13s%80t%c\n"


# AP_CHDR -- Print the center header banner strings.

procedure ap_chdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call fprintf (fd, CTR_NSTR)
	call fprintf (fd, CTR_USTR)
	call fprintf (fd, CTR_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WCRES -- Procedure to write out the centering results.

procedure ap_wcres (ap, fd, ier, lastchar)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor
int	ier		# error code
int	lastchar	# last character written out

pointer	sp, str
real	apstatr()

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call ap_cserrors (ier, Memc[str], SZ_LINE)

	# Print the computed centers.
	call fprintf (fd, CTR_WSTR)
	    call pargr (apstatr (ap, XCENTER))
	    call pargr (apstatr (ap, YCENTER))
	    call pargr (apstatr (ap, XSHIFT))
	    call pargr (apstatr (ap, YSHIFT))
	    call pargr (apstatr (ap, XERR))
	    call pargr (apstatr (ap, YERR))
	    call pargi (ier)
	    call pargstr (Memc[str])
	    call pargi (lastchar)

	call sfree (sp)
end


# AP_CSERRORS -- Procedure to encode the centering task error messages into
# a string.

procedure ap_cserrors (ier, str, maxch)

int	ier		# error code
char	str[ARB]	# output str
int	maxch		# maximum number of characters

begin
	switch (ier) {
	case AP_CTR_NOAREA:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_ctr_pixels")
        case AP_CTR_OUTOFBOUNDS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Out_of_bounds")
	case AP_CTR_NTOO_SMALL:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Npts_too_few")
	case AP_CTR_SINGULAR:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Singular")
	case AP_CTR_NOCONVERGE:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_converge")
	case AP_CTR_BADSHIFT:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Shift_too_big")
	case AP_CTR_LOWSNRATIO:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Low_snr")
	case AP_CTR_BADDATA:
	    call sprintf (str, maxch, "%s")
		call pargstr ("Bad_pixels")
	default:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_error")
	}
end
