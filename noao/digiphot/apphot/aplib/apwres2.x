
include "../lib/apphotdef.h"
include "../lib/radprofdef.h"
include "../lib/apphot.h"
include "../lib/find.h"
include "../lib/polyphot.h"
include "../lib/radprof.h"


define	FD_NSTR	"#N%4tXCENTER%13tYCENTER%22tMAG%31tSHARPNESS%43t\
ROUNDNESS%55tID%80t\\\n"
define	FD_USTR "#U%4tpixels%13tpixels%22t#%31t#%43t#%55t#%80t\\\n"
define	FD_FSTR	"#F%4t%%-12.2f%13t%%-9.2f%22t%%-9.3f%31t%%-12.3f%43t\
%%-12.3f%55t%%-6d%80t\\\n"
define	FD_WSTR	"%4t%-9.2f%-9.2f%-9.3f%-12.3f%-12.3f%-6d\n"

# AP_WFDPARAM -- Write the daofind parameters to the output file.

procedure ap_wfdparam (out, ap)

pointer	out			# database descriptor
pointer	ap			# pointer to the apphot strucuture

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Write out the parameters.
	call ap_param (ap, out, "daofind")
	call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	call ap_sparam (out, KY_IMNAME, Memc[str], "imagename", "image name")
	call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF), UN_FWHMPSF,
	    "fwhm of the psf")
	call ap_rparam (out, KY_NSIGMA, apstatr (ap, NSIGMA), UN_NSIGMA,
	    "size of the kernel in fwhmpsf")
	call ap_rparam (out, KY_RATIO, apstatr (ap, RATIO), UN_RATIO,
	    "ratio of ysigma to xsigma")
	call ap_rparam (out, KY_THETA, apstatr (ap, THETA), UN_THETA,
	    "position angle in degrees")
	call fprintf (out, "#\n")
	call ap_rparam (out, KY_SHARPLO, apstatr (ap, SHARPLO), UN_SHARPLO,
	    "lower sharpness bound")
	call ap_rparam (out, KY_SHARPHI, apstatr (ap, SHARPHI), UN_SHARPHI,
	    "higher sharpness bound")
	call ap_rparam (out, KY_ROUNDLO, apstatr (ap, ROUNDLO), UN_ROUNDLO,
	    "lower roundness bound")
	call ap_rparam (out, KY_ROUNDHI, apstatr (ap, ROUNDHI), UN_ROUNDHI,
	    "higher roundness bound")
	call fprintf (out, "#\n")

	# Write out the header banner.
	call ap_fdhdr (ap, out)

	call sfree (sp)
end


# AP_FDHDR -- Write the daofind column banner to the output file.

procedure ap_fdhdr (ap, out)

pointer	ap	# pointer to the apphot structure (unused)
pointer	out	# file descriptor

begin
	if (out == NULL)
	    return

	call fprintf (out, FD_NSTR)
	call fprintf (out, FD_USTR)
	call fprintf (out, FD_FSTR)
	call fprintf (out, "#\n")
end


# AP_PFDHDR -- Print the daofind column banner on the standard output.

procedure ap_pfdhdr ()

begin
	call printf (" XCENTER  YCENTER     MAG    SHARP   ROUND    ID\n\n")
end


# APSTDOUT -- Print the daofind results on the standard output.

procedure apstdout (density, ptrs, ncols, nbox, cols, x, y, sharp, round,
    nstars, ntotal, threshold)

real	density[ncols, nbox]		# densities
int	ptrs[ARB]			# array pf pointers
int	ncols, nbox			# dimensions of the data
int	cols[ARB]			# column numbers
real	x[ARB]				# xcoords
real	y[ARB]				# y coords
real	sharp[ARB]			# sharpnesses
real	round[ARB]			# roundness
int	nstars				# number of detected stars in the line
int	ntotal				# total number of detected objects
real	threshold			# threshold for detection

int	i, middle
real	den

begin
	middle = 1 + nbox / 2
	do i = 1, nstars {
	    call printf (" %7.2f  %7.2f  %7.3f  %6.3f  %6.3f  %4d\n")
		call pargr (x[i])
		call pargr (y[i])
		if (threshold <= 0.0)
		    den = INDEFR
		else
		    den = -2.5 * log10 (density[cols[i],ptrs[middle]] /
		        threshold)
		call pargr (den)
		call pargr (sharp[i])
		call pargr (round[i])
		call pargi (ntotal + i)
	}
end


# APDTFOUT -- Write the daofind results to the output file.

procedure apdtfout (fd, density, ptrs, ncols, nbox, cols, x, y, sharp, round,
	nstars, ntotal, threshold, stid)

pointer	fd				# file descriptor
real	density[ncols, nbox]		# densities
int	ptrs[ARB]			# array pf pointers
int	ncols, nbox			# dimensions of the data
int	cols[ARB]			# column numbers
real	x[ARB]				# xcoords
real	y[ARB]				# y coords
real	sharp[ARB]			# sharpnesses
real	round[ARB]			# roundness
int	nstars				# number of detected stars in the line
int	ntotal				# total number of detected objects
real	threshold			# threshold for detection
int	stid				# output file sequence number

int	i, middle
real	den

begin
	if (fd == NULL)
	    return

	middle = 1 + nbox / 2
	do i = 1, nstars {
	    call fprintf (fd, FD_WSTR)
		call pargr (x[i])
		call pargr (y[i])
		if (threshold <= 0.0)
		    den = INDEFR
		else
		    den = -2.5 * log10 (density[cols[i],ptrs[middle]] /
		        threshold)
		call pargr (den)
		call pargr (sharp[i])
		call pargr (round[i])
		call pargi (stid + ntotal + i - 1)
	}
end



# define the #N, #U and #K polyphot strings

define	PY_NSTR1  "#N%4tITIME%19tXAIRMASS%34tIFILTER%57tOTIME%80t\\\n"
define	PY_USTR1  "#U%4ttimeunit%19tnumber%34tname%57ttimeunit%80t\\\n"
define	PY_FSTR1  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-23s%57t%%-23s%80t \n"
define	PY_WSTR1  "%4t%-15.7g%-15.7g%-23.23s%-23.23s%80t\\\n"

define  PY_NSTR2  "#N%4tSUM%19tAREA%34tMAG%43tMERR%51tPIER%58tPERROR%80t\\\n"
define  PY_USTR2  "#U%4tcounts%19tpixels%34tmag%43tmag%51t##%58tperrors%80t\\\n"
define	PY_FSTR2  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-9.3f%43t%%-8.3f%51t\
%%-7d%58t%%-13s%80t \n"
define  PY_WSTR2  "%4t%-15.7g%-15.7g%-9.3f%-8.3f%-7d%-13s%80t\\\n"

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
	call apstats (py, OTIME, Memc[str], SZ_FNAME)
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


# AP_SPYERRORS -- Encode the polygon fitting error in a string.

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



# define the #N, #U and #K radprof strings

define  RP_NSTR1  "#N%4tPFWHM%14tINORM%29tTINORM%44tRIER%49tRERROR%80t\\\n"
define  RP_USTR1  "#U%4tScale%14tcounts%29tcounts%44t##%49trerrors%80t\\\n"
define  RP_FSTR1  "#F%4t%%-13.3f%14t%%-15.7f%29t%%-15.7f%44t%%-5d%49t\
%%-13s%80t \n"
define  RP_WSTR1  "%4t%-10.3f%-15.7g%-15.7g%-5d%-13s%80t\\\n"

define  RP_NSTR2  "#N%4tPRADIUS%14tINTENSITY%29tTINTENSITY%80t\\\n"
define  RP_USTR2  "#U%4tscale%14tcounts%29tcounts%80t\\\n"
define  RP_FSTR2  "#F%4t%%-13.3f%14t%%-15.7f%29t%%-15.7f%80t \n"
define  RP_WSTR2  "%4t%-10.3f%-15.7g%-15.7g%79t%2s\n"


# AP_RHDR -- Print the radprof column header strings.

procedure ap_rhdr (ap, fd)

pointer	ap		# apphot descriptor
int	fd		# output file descriptor

begin
	if (fd == NULL)
	    return
	call fprintf (fd, RP_NSTR1)
	call fprintf (fd, RP_USTR1)
	call fprintf (fd, RP_FSTR1)
	call fprintf (fd, "#\n")
	call fprintf (fd, RP_NSTR2)
	call fprintf (fd, RP_USTR2)
	call fprintf (fd, RP_FSTR2)
	call fprintf (fd, "#\n")
end


# AP_WRRES -- Write the results of the radprof task to the output file.

procedure ap_wrres (ap, fd, ier)

pointer	ap	# pointer to apphot structure
int	fd	# output text file descriptor
int	ier	# radial profile error

int	i, nrpts
pointer	sp, str, rprof
real	apstatr()

begin
	# Initialize.
	if (fd == NULL)
	    return
	rprof = AP_RPROF(ap)
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the radprof parameters.
	call ap_srerrors (ier, Memc[str], SZ_LINE)
	call fprintf (fd, RP_WSTR1)
	    call pargr (apstatr (ap, RPFWHM) / apstatr (ap, SCALE))
	    call pargr (apstatr (ap, INORM))
	    call pargr (apstatr (ap, TNORM))
	    call pargi (ier)
	    call pargstr (Memc[str])

	# Print the radial profile.
	nrpts = apstatr (ap, RPRADIUS) / apstatr (ap, RPSTEP) + 1
	if (nrpts == 0) {
	    call fprintf (fd, RP_WSTR2)
	        call pargr (INDEFR)
	        call pargr (INDEFR)
	        call pargr (INDEFR)
	        call pargstr ("  ")
	} else {
	    do i = 1, nrpts {
		if (nrpts == 1) {
	            call fprintf (fd, RP_WSTR2)
		        call pargr (Memr[AP_RPDIST(rprof)+i-1] / AP_SCALE(ap))
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("  ")
	        } if (i == nrpts) {
	            call fprintf (fd, RP_WSTR2)
		        call pargr (Memr[AP_RPDIST(rprof)+i-1] / AP_SCALE(ap))
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("* ")
	        } else {
	            call fprintf (fd, RP_WSTR2)
		        call pargr (Memr[AP_RPDIST(rprof)+i-1] / AP_SCALE(ap))
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("*\\")
	        }
	    }
	}

	call sfree (sp)
end


# AP_SRERRORS -- Encode the radial profile error message in a string.

procedure ap_srerrors (ier, str, maxch)

int	ier		# error code
char	str[ARB]	# encoded error string
int	maxch		# maximum number of characters

begin
    	switch (ier) {
	case AP_RP_NOPROFILE:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Off_image")
	case AP_RP_OUTOFBOUNDS:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Out_of_bounds")
	case AP_RP_NPTS_TOO_SMALL:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Too_few_points")
	case AP_RP_SINGULAR:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("Singular")
	default:
	    call sprintf (str, maxch, "%s")
	        call pargstr ("No_error")
	}
end
