include "../lib/apphotdef.h"
include "../lib/radprofdef.h"
include "../lib/radprof.h"

# define the #N, #U and #K radprof strings

define  RP_NSTR1  "#N%4tPFWHM%14tINORM%29tTINORM%44tRIER%49tRERROR%80t\\\n"
define  RP_USTR1  "#U%4tpixels%14tcounts%29tcounts%44t##%49trerrors%80t\\\n"
define  RP_FSTR1  "#F%4t%%-13.3f%14t%%-15.7f%29t%%-15.7f%44t%%-5d%49t%%-13s%80t \n"
define  RP_WSTR1  "%4t%-10.3f%-15.7g%-15.7g%-5d%-13s%80t\\\n"

define  RP_NSTR2  "#N%4tPRADIUS%14tINTENSITY%29tTINTENSITY%80t\\\n"
define  RP_USTR2  "#U%4tpixels%14tcounts%29tcounts%80t\\\n"
define  RP_FSTR2  "#F%4t%%-13.3f%14t%%-15.7f%29t%%-15.7f%80t \n"
define  RP_WSTR2  "%4t%-10.3f%-15.7g%-15.7g%79t%2s\n"


# AP_RHDR -- Print the radprof header banner strings.

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


# AP_WRRES -- Procedure to write the results of radprof to the output file.

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
	call pargr (apstatr (ap, RPFWHM))
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
		        call pargr (Memr[AP_RPDIST(rprof)+i-1])
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("  ")
	        } if (i == nrpts) {
	            call fprintf (fd, RP_WSTR2)
		        call pargr (Memr[AP_RPDIST(rprof)+i-1])
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("* ")
	        } else {
	            call fprintf (fd, RP_WSTR2)
		        call pargr (Memr[AP_RPDIST(rprof)+i-1])
		        call pargr (Memr[AP_INTENSITY(rprof)+i-1])
		        call pargr (Memr[AP_TINTENSITY(rprof)+i-1])
		        call pargstr ("*\\")
	        }
	    }
	}

	call sfree (sp)
end


# AP_SRERRORS -- Procedure to print out the radial profiles

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
