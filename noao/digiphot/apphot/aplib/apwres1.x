include "../lib/apphotdef.h"
include "../lib/photdef.h"

include "../lib/apphot.h"
include "../lib/center.h"
include "../lib/fitsky.h"
include "../lib/phot.h"


# define the #N, #U and #K id strings

define	ID_NSTR	"#N%4tIMAGE%24tXINIT%34tYINIT%44tID%49tCOORDS%72tLID%80t\\\n"
define	ID_USTR	"#U%4timagename%24tpixels%34tpixels%44t##%49tfilename%72t\
##%80t\\\n"
define	ID_FSTR	"#F%4t%%-23s%24t%%-10.2f%34t%%-10.2f%44t%%-5d%49t%%-23s%72t\
%%-5d%80t \n"
define	ID_WSTR "%-23.23s%24t%-10.2f%34t%-10.2f%44t%-5d%49t%-23.23s%72t\
%-5d%80t%c\n"

# AP_IDHDR -- Print the id column header strings.

procedure ap_idhdr (ap, fd)

pointer	ap		# apphot descriptor (unused)
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call fprintf (fd, ID_NSTR)
	call fprintf (fd, ID_USTR)
	call fprintf (fd, ID_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WID -- Write the id record to a file.

procedure ap_wid (ap, fd, xpos, ypos, id, lid, lastchar)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor
real	xpos		# x position
real	ypos		# y position
int	id		# id of the star
int	lid		# list number
int	lastchar	# last character in record

pointer	sp, imname, clname 

begin
	if (fd == NULL)
	    return

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (clname, SZ_FNAME, TY_CHAR)

	# Print description of object.
	call apstats (ap, IMNAME, Memc[imname], SZ_FNAME)
	call apstats (ap, CLNAME, Memc[clname], SZ_FNAME)
	if (Memc[clname] == EOS)
	    call strcpy ("nullfile", Memc[clname], SZ_FNAME)

	call fprintf (fd, ID_WSTR)
	    call pargstr (Memc[imname])
	    call pargr (xpos)
	    call pargr (ypos)
	    call pargi (id)
	    call pargstr (Memc[clname])
	    call pargi (lid)
	    call pargi (lastchar)

	call sfree (sp)
end


# define the #N, #U and #K center strings

define	CTR_NSTR  "#N%4tXCENTER%13tYCENTER%24tXSHIFT%32tYSHIFT%40tXERR%48t\
YERR%56tCIER%61tCERROR%80t\\\n"
define	CTR_USTR  "#U%4tpixels%13tpixels%24tpixels%32tpixels%40tpixels%48t\
pixels%56t##%61tcerrors%80t\\\n"
define	CTR_FSTR  "#F%4t%%-12.2f%13t%%-11.2f%24t%%-8.2f%32t%%-8.2f%40t\
%%-8.2f%48t%%-8.2f%56t%%-5d%61t%%-13s%80t \n"
define  CTR_WSTR   "%4t%-9.2f%-11.2f%-8.2f%-8.2f%-8.2f%-8.2f%-5d%-13s%80t%c\n"


# AP_CHDR -- Print the center algorithm column header strings.

procedure ap_chdr (ap, fd)

pointer	ap		# apphot descriptor (unused)
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call fprintf (fd, CTR_NSTR)
	call fprintf (fd, CTR_USTR)
	call fprintf (fd, CTR_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WCRES -- Write out the centering algorithm results to a file.

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


# AP_CSERRORS -- Encode the centering task error messages into a string.

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


# define the #N, #U and #K fitsky strings

define	SKY_NSTR  "#N%4tMSKY%19tSTDEV%34tSSKEW%49tNSKY%56tNSREJ%62tSIER\
%67tSERROR%80t\\\n"
define	SKY_USTR  "#U%4tcounts%19tcounts%34tcounts%49tnpix%56tnpix%62t##\
%67tserrors%80t\\\n"
define	SKY_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-15.7g%49t%%-7d%56t\
%%-6d%62t%%-5d%67t%%-13s%80t \n"
define  SKY_WSTR  "%4t%-15.7g%-15.7g%-15.7g%-7d%-6d%-5d%-13s%80t%c\n"

# AP_SHDR -- Print the sky fitting column header strings.

procedure ap_shdr (ap, fd)

pointer	ap		# apphot descriptor (unused)
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, SKY_NSTR)
	call fprintf (fd, SKY_USTR)
	call fprintf (fd, SKY_FSTR)
	call fprintf (fd, "#\n")
end


# AP_WSRES -- Write the results of the sky fitting algorithms to the output
# file.

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


# AP_SSERRORS -- Encode the sky fitting error messages in a string.

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



# define the #N, #U and #K phot/wphot strings

define	MAG1_NSTR  "#N%4tITIME%19tXAIRMASS%34tIFILTER%57tOTIME%80t\\\n"
define	MAG1_USTR  "#U%4ttimeunit%19tnumber%34tname%57ttimeunit%80t\\\n"
define	MAG1_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%57t%%-23s%80t \n"
define	MAG1_WSTR  "%4t%-15.7g%-15.7g%-23s%-23s%80t\\\n"

define  MAG2_NSTR  "#N%4tRAPERT%13tSUM%28tAREA%43tMAG%50tMERR%58tPIER%63t\
PERROR%80t\\\n"
define  MAG2_USTR  "#U%4tscale%13tcounts%28tpixels%43tmag%50tmag%58t##%63t\
perrors%80t\\\n"
define  MAG2_FSTR  "#F%4t%%-12.2f%13t%%-15.7f%28t%%-15.7f%43t%%-7.3f%50t\
%%-6.3f%58t%%-5d%63t%%-13s%80t \n"
define  MAG2_WSTR  "%4t%-9.2f%-15.7g%-15.7g%-7.3f%-6.3f%-5d%-13s%79t%2s\n"


# AP_MHDR -- Print the phot/wphot/qphot column header strings.

procedure ap_mhdr (ap, fd)

pointer	ap		# apphot descriptor (unused)
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


# AP_WMRES -- Write the results of the phot/qphot/wphot tasks to the output 
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
	    call apstats (ap, OTIME, Memc[str], SZ_FNAME)
	        call pargstr (Memc[str])
	}

	# Write out the error code.
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
	    call pargr (Memr[AP_APERTS(phot)+i-1])
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


# AP_PSERRORS -- Encode the photometric errors string.

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
