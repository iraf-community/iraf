include "../lib/apphotdef.h"
include "../lib/photdef.h"

include "../lib/apphot.h"
include "../lib/fitsky.h"
include "../lib/phot.h"
include "../lib/polyphot.h"

# define the #N, #U and #K phot/wphot strings

define	MAG1_NSTR  "#N%4tITIME%19tXAIRMASS%34tIFILTER%57tOTIME%80t\\\n"
define	MAG1_USTR  "#U%4ttimeunit%19tnumber%34tname%57ttimeunit%80t\\\n"
define	MAG1_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%57t%%-23s%80t \n"
define	MAG1_WSTR  "%4t%-15.7g%-15.7g%-23.23s%-23.23s%80t\\\n"

define  MAG2_NSTR  "#N%4tRAPERT%13tSUM%27tAREA%38tFLUX%52tMAG%59tMERR%66t\
PIER%71tPERROR%80t\\\n"
define  MAG2_USTR  "#U%4tscale%13tcounts%27tpixels%38tcounts%52tmag%59t\
mag%66t##%71tperrors%80t\\\n"
define  MAG2_FSTR  "#F%4t%%-12.2f%13t%%-14.7g%27t%%-11.7g%38t%%-14.7g%52t\
%%-7.3f%59t%%-6.3f%66t%%-5d%71t%%-9s%80t \n"
define  MAG2_WSTR  "%4t%-9.2f%-14.7g%-11.7g%-14.7g%-7.3f%-6.3f%-5d%-9.9s\
%79t%2s\n"


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
real	sky_val
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
	if (IS_INDEFR(Memr[AP_MAGS(phot)+i-1])) {
	    if (pier != AP_APERT_OUTOFBOUNDS)
		ier = pier
	    else if (i > AP_NMAXAP(phot))
	        ier = AP_APERT_OUTOFBOUNDS
	    else
	        ier = AP_OK
	} else if (i >= AP_NMINAP(phot)) {
	    ier = AP_APERT_BADDATA
	} else {
	    ier = AP_OK
	}
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
	    call pargd (Memd[AP_SUMS(phot)+i-1])
	    call pargd (Memd[AP_AREA(phot)+i-1])
	    sky_val = apstatr (ap, SKY_MODE)
	    if (IS_INDEFR(sky_val))
		call pargr (0.0)
	    else
		call pargr (real (Memd[AP_SUMS(phot)+i-1] - sky_val *
		    Memd[AP_AREA(phot)+i-1]))
	    call pargr (Memr[AP_MAGS(phot)+i-1])
	    if (Memr[AP_MAGERRS(phot)+i-1] > 99.999)
		call pargr (INDEFR)
	    else
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
	    call strcpy ("OffImage", str, maxch)
	case AP_APERT_OUTOFBOUNDS:
	    call strcpy ("EdgeImage", str, maxch)
	case AP_APERT_NOSKYMODE:
	    call strcpy ("NoSky", str, maxch)
	case AP_APERT_NEGMAG:
	    call strcpy ("NoFlux", str, maxch)
	case AP_APERT_BADDATA:
	    call strcpy ("BadPixels", str, maxch)
	default:
	    call strcpy ("NoError", str, maxch)
	}
end


# define the #N, #U and #K polyphot strings

define	PY_NSTR1  "#N%4tITIME%19tXAIRMASS%34tIFILTER%57tOTIME%80t\\\n"
define	PY_USTR1  "#U%4ttimeunit%19tnumber%34tname%57ttimeunit%80t\\\n"
define	PY_FSTR1  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%57t%%-23s%80t \n"
define	PY_WSTR1  "%4t%-15.7g%-15.7g%-23.23s%-23.23s%80t\\\n"

define  PY_NSTR2  "#N%4tSUM%19tAREA%34tFLUX%49tMAG%58tMERR%66tPIER%71t\
PERROR%80t\\\n"
define  PY_USTR2  "#U%4tcounts%19tpixels%34tcounts%49tmag%58tmag%66t##%71t\
perrors%80t\\\n"
define	PY_FSTR2  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-15.7g%49t%%-9.3f%58t\
%%-7.3f%66t%%-5d%71t%%-9s%80t \n"
define  PY_WSTR2  "%4t%-15.7g%-15.7g%-15.7g%-9.3f%-7.3f%-5d%-9.9s%80t\\\n"

define	PY_NSTR3  "#N%4tPOLYGONS%24tPID%29tOLDXMEAN%38tOLDYMEAN%47t\
XMEAN%56tYMEAN%65tMINRAD%74tNVER%80t\\\n"
define	PY_USTR3  "#U%4tfilename%24t##%29tpixels%38tpixels%47t\
pixels%56tpixels%65tpixels%74t##%80t\\\n"
define	PY_FSTR3  "#F%4t%%-23s%24t%%-5d%29t%%-9.2f%38t%%-9.2f%47t\
%%-9.2f%56t%%-9.2f%65t%%-9.2f%74t%%-5d%80t \n"
define  PY_WSTR3  "%4t%-20.20s%-5d%-9.2f%-9.2f%-9.2f%-9.2f%-9.2f%-5d%80t\\\n"

define  PY_NSTR4  "#N%4tXVERTEX%13tYVERTEX%80t\\\n"
define  PY_USTR4  "#U%4tpixels%13tpixels%80t\\\n"
define	PY_FSTR4  "#F%4t%%-12.2f%13t%%-9.2f%80t \n"
define  PY_WSTR4  "%4t%-9.2f%-9.2f%79t%2s\n"


# AP_PLHDR -- Print the polyphot column header strings.

procedure ap_plhdr (ap, fd)

pointer	ap		# apphot descriptor (unused)
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return

	call fprintf (fd, PY_NSTR1)
	call fprintf (fd, PY_USTR1)
	call fprintf (fd, PY_FSTR1)
	call fprintf (fd, "#\n")

	call fprintf (fd, PY_NSTR2)
	call fprintf (fd, PY_USTR2)
	call fprintf (fd, PY_FSTR2)
	call fprintf (fd, "#\n")

	call fprintf (fd, PY_NSTR3)
	call fprintf (fd, PY_USTR3)
	call fprintf (fd, PY_FSTR3)
	call fprintf (fd, "#\n")

	call fprintf (fd, PY_NSTR4)
	call fprintf (fd, PY_USTR4)
	call fprintf (fd, PY_FSTR4)
	call fprintf (fd, "#\n")
end


# AP_WLRES -- Write the results of the polyphot task to the output file.

procedure ap_wlres (py, fd, xver, yver, nver, pid, pier)

pointer	py		# pointer to apphot structure
int	fd		# output file descriptor
real	xver[ARB]	# coords of x vertices
real	yver[ARB]	# coords of y vertices
int	nver		# number of vertices
int	pid		# polygon number
int	pier		# photometric error

int	i
pointer	sp, str, pyname
real	sky_val
int	apstati()
double	apstatd()
real	apstatr()

begin
	if (fd == NULL)
	    return

	# Allocate space.
	call smark (sp)
	call salloc (pyname, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Write out the exposure time, airmass and filter id.
	call fprintf (fd, PY_WSTR1)
	    call pargr (apstatr (py, ITIME))
	    call pargr (apstatr (py, XAIRMASS))
	call apstats (py, FILTERID, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])
	call apstats (py, OTIME, Memc[str], SZ_FNAME)
	    call pargstr (Memc[str])

	# Write the photometry results.
	call ap_spyerrors (pier, Memc[str], SZ_LINE)
	sky_val = apstatr (py, SKY_MODE)
	call fprintf (fd, PY_WSTR2)
	    call pargd (apstatd (py, PYFLUX))
	    call pargd (apstatd (py, PYNPIX))
	    if (IS_INDEFR(sky_val))
		call pargr (0.0)
	    else
		call pargr (real (apstatd (py, PYFLUX) - sky_val *
		    apstatd (py, PYNPIX)))
	    call pargr (apstatr (py, PYMAG))
	    if (apstatr (py, PYMAGERR) > 99.999)
		call pargr (INDEFR)
	    else
	        call pargr (apstatr (py, PYMAGERR))
	    call pargi (pier)
	    call pargstr (Memc[str])

	# Write the polygon characteristics
	#call apstats (py, PYNAME, Memc[pyname], SZ_FNAME)
	call apstats (py, PYROOT, Memc[pyname], SZ_FNAME)
	call fprintf (fd, PY_WSTR3)
	    if (Memc[pyname] == EOS)
		call pargstr ("nullfile")
	    else
	        call pargstr (Memc[pyname])
	    call pargi (pid)
	    call pargr (apstatr (py, OPYXMEAN))
	    call pargr (apstatr (py, OPYYMEAN))
	    call pargr (apstatr (py, OPYCX))
	    call pargr (apstatr (py, OPYCY))
	    call pargr (apstatr (py, PYMINRAD))
	    call pargi (apstati (py, PYNVER))

	# Write out the vertices of the polygon.
	if (nver == 0) {
	    call fprintf (fd, PY_WSTR4)
		call pargr (INDEFR)
		call pargr (INDEFR)
		call pargstr ("  ")
	} else {
	    do i = 1, nver {
	        call fprintf (fd, PY_WSTR4)
	        if (nver == 1) {
		    call pargr (xver[i])
		    call pargr (yver[i])
		    call pargstr ("  ")
	        } else if (i == nver) {
		    call pargr (xver[i])
		    call pargr (yver[i])
		    call pargstr ("* ")
	        } else {
		    call pargr (xver[i])
		    call pargr (yver[i])
		    call pargstr ( "*\\")
	        }
	    }
	}

	call sfree (sp)
end


# AP_SPYERRORS -- Encode the polygon fitting error in a string.

procedure ap_spyerrors (ier, str, maxch)

int	ier		# error code
char	str[ARB]	# output string
int	maxch		# maximum number of characters

begin
	switch (ier) {
	case PY_NOPOLYGON:
	    call strcpy ("NoPolygon", str, maxch)
	case PY_OUTOFBOUNDS:
	    call strcpy ("EdgeImage", str, maxch)
	case PY_NOPIX:
	    call strcpy ("NoPixels", str, maxch)
	case PY_NOSKYMODE:
	    call strcpy ("NoSky", str, maxch)
	case PY_BADDATA:
	    call strcpy ("BadPixels", str, maxch)
	default:
	    call strcpy ("NoError", str, maxch)
	}
end
