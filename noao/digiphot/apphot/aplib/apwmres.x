include "../lib/apphotdef.h"
include "../lib/photdef.h"
include "../lib/apphot.h"
include "../lib/phot.h"

# define the #N, #U and #K phot/wphot strings

define	MAG1_NSTR  "#N%4tITIME%19tXAIRMASS%34tIFILTER%80t\\\n"
define	MAG1_USTR  "#U%4ttimeunit%19tnumber%34tname%80t\\\n"
define	MAG1_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%80t \n"
define	MAG1_WSTR  "%4t%-15.7g%-15.7g%-23s%80t\\\n"

define  MAG2_NSTR  "#N%4tRAPERT%13tSUM%28tAREA%43tMAG%50tMERR%58tPIER%63tPERROR%80t\\\n"
define  MAG2_USTR  "#U%4tscale%13tcounts%28tpixels%43tmag%50tmag%58t##%63tperrors%80t\\\n"
define  MAG2_FSTR  "#F%4t%%-12.2f%13t%%-15.7f%28t%%-15.7f%43t%%-7.3f%50t%%-6.3f%58t%%-5d%63t%%-13s%80t \n"
define  MAG2_WSTR  "%4t%-9.2f%-15.7g%-15.7g%-7.3f%-6.3f%-5d%-13s%79t%2s\n"


# AP_MHDR -- Print the phot/wphot header banner strings.

procedure ap_mhdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, MAG1_NSTR)
	call fprintf (fd, MAG1_USTR)
	call fprintf (fd, MAG1_FSTR)
	call fprintf (fd, "#\n")

	call fprintf (fd, MAG2_NSTR)
	call fprintf (fd, MAG2_USTR)
	call fprintf (fd, MAG2_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WMRES -- Procedure to write the results of the phot task to the output 
# file.

procedure ap_wmres (ap, fd, i, pier, endstr)

pointer	ap		# pointer to apphot structure
int 	fd		# output text file
int	i		# index of variable length field
int	pier		# photometric error
char	endstr[ARB]	# termination string

int	ier
pointer	sp, str, phot
real	apstatr()

begin
	# Initialize.
	if (fd == NULL)
	    return

	phot = AP_PPHOT(ap)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Write out the exposure time, airmass and filter information.
	if (i <= 1) {
	    call fprintf (fd, MAG1_WSTR)
	        call pargr (apstatr (ap, ITIME))
	        call pargr (apstatr (ap, XAIRMASS))
	    call apstats (ap, FILTERID, Memc[str], SZ_FNAME)
	        call pargstr (Memc[str])
	}

	if (IS_INDEFR(Memr[AP_MAGS(phot)+i-1]))
	    ier = pier
	else if (i >= AP_NMINAP(phot))
	    ier = AP_APERT_BADDATA
	else
	    ier = AP_OK
	call ap_pserrors (ier, Memc[str], SZ_LINE)

	# Write out the photometry results.
	call fprintf (fd, MAG2_WSTR)
	if (i == 0) {
	    call pargr (0.0)
	    call pargr (0.0)
	    call pargr (0.0)
	    call pargr (INDEFR)
	    call pargr (INDEFR)
	    call pargi (ier)
	    call pargstr (Memc[str])
	    call pargstr (endstr)
	} else {
	    call pargr (Memr[AP_APERTS(phot)+i-1] * AP_SCALE(ap))
	    call pargr (Memr[AP_SUMS(phot)+i-1])
	    call pargr (Memr[AP_AREA(phot)+i-1])
	    call pargr (Memr[AP_MAGS(phot)+i-1])
	    call pargr (Memr[AP_MAGERRS(phot)+i-1])
	    call pargi (ier)
	    call pargstr (Memc[str])
	    call pargstr (endstr)
	}

	call sfree (sp)
end


# AP_PSERRORS -- Procedure to encode the photometric errors string.

procedure ap_pserrors (ier, str, maxch)

int	ier		# photometry error code
char	str[ARB]	# output string
int	maxch		# maximum length of string

begin
	switch (ier) {
	case AP_APERT_NOAPERT:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("Off_image")
	case AP_APERT_OUTOFBOUNDS:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("Out_of_bounds")
	case AP_APERT_NOSKYMODE:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("Unknown_sky")
	case AP_APERT_NEGMAG:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("Negative_flux")
	case AP_APERT_BADDATA:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("Bad_pixels")
	default:
	    call sprintf (str, maxch, "%s")
	    	call pargstr ("No_error")
	}
end
