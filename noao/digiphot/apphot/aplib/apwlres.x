include "../lib/apphot.h"
include "../lib/polyphot.h"

# define the #N, #U and #K polyphot strings

define	PY_NSTR1  "#N%4tITIME%19tXAIRMASS%34tIFILTER%80t\\\n"
define	PY_USTR1  "#U%4ttimeunit%19tnumber%34tname%80t\\\n"
define	PY_FSTR1  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%80t \n"
define	PY_WSTR1  "%4t%-15.7g%-15.7g%-23s%80t\\\n"

define  PY_NSTR2  "#N%4tSUM%19tAREA%34tMAG%43tMERR%51tPIER%58tPERROR%80t\\\n"
define  PY_USTR2  "#U%4tcounts%19tpixels%34tmag%43tmag%51t##%58tperrors%80t\\\n"
define	PY_FSTR2  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-9.3f%43t%%-8.3f%51t%%-7d%58t%%-13s%80t \n"
define  PY_WSTR2  "%4t%-15.7g%-15.7g%-9.3f%-8.3f%-7d%-13s%80t\\\n"

define	PY_NSTR3  "#N%4tPOLYGONS%24tPID%29tOLDXMEAN%38tOLDYMEAN%47tXMEAN%56tYMEAN%65tMINRAD%74tNVER%80t\\\n"
define	PY_USTR3  "#U%4tfilename%24t##%29tpixels%38tpixels%47tpixels%56tpixels%65tpixels%74t##%80t\\\n"
define	PY_FSTR3  "#F%4t%%-23s%24t%%-5d%29t%%-9.2f%38t%%-9.2f%47t%%-9.2f%56t%%-9.2f%65t%%-9.2f%74t%%-5d%80t \n"
define  PY_WSTR3  "%4t%-20.20s%-5d%-9.2f%-9.2f%-9.2f%-9.2f%-9.2f%-5d%80t\\\n"

define  PY_NSTR4  "#N%4tXVERTEX%13tYVERTEX%80t\\\n"
define  PY_USTR4  "#U%4tpixels%13tpixels%80t\\\n"
define	PY_FSTR4  "#F%4t%%-12.2f%13t%%-9.2f%80t \n"
define  PY_WSTR4  "%4t%-9.2f%-9.2f%79t%2s\n"


# AP_PLHDR -- Print the polyphot header banner strings.

procedure ap_plhdr (ap, fd)

pointer	ap		# apphot descriptor
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


# AP_WLRES -- Procedure to write the results of the polyphot task to the
# output file.

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
int	apstati()
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

	# Write the photometry results.
	call ap_spyerrors (pier, Memc[str], SZ_LINE)
	call fprintf (fd, PY_WSTR2)
	    call pargr (apstatr (py, PYFLUX))
	    call pargr (apstatr (py, PYNPIX))
	    call pargr (apstatr (py, PYMAG))
	    call pargr (apstatr (py, PYMAGERR))
	    call pargi (pier)
	    call pargstr (Memc[str])

	# Write the polygon characteristics
	call apstats (py, PYNAME, Memc[pyname], SZ_FNAME)
	call fprintf (fd, PY_WSTR3)
	    if (Memc[pyname] == EOS)
		call pargstr ("nullfile")
	    else
	        call pargstr (Memc[pyname])
	    call pargi (pid)
	    call pargr (apstatr (py, PYXMEAN))
	    call pargr (apstatr (py, PYYMEAN))
	    call pargr (apstatr (py, PYCX))
	    call pargr (apstatr (py, PYCY))
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


# AP_SPYERRORS -- Procedure to encode the polygon fitting errors in a string.

procedure ap_spyerrors (ier, str, maxch)

int	ier		# error code
char	str[ARB]	# output string
int	maxch		# maximum number of characters

begin
	switch (ier) {
	case PY_NOPOLYGON:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_polygon")
	case PY_OUTOFBOUNDS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Out_of_bounds")
	case PY_NOPIX:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Off_image")
	case PY_NOSKYMODE:
	    call sprintf (str, maxch, "%s")
		call pargstr ("Unknown_sky")
	case PY_BADDATA:
	    call sprintf (str, maxch, "%s")
		call pargstr ("Bad_pixels")
	default:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_error")
	}
end
