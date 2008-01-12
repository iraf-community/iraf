include "../lib/apphotdef.h"
include "../lib/fitpsfdef.h"
include "../lib/apphot.h"
include "../lib/fitpsf.h"

# define the #N, #U and #K fitpsf strings

define  PSF_RNSTR1  "#N%4tXCENTER%14tYCENTER%24tRSIGMA%33tAMPLITUDE%48t\
SKY%80t\\\n"
define  PSF_RUSTR1  "#U%4tpixels%14tpixels%24tpixels%33tcounts%48t\
counts%80t\\\n"
define  PSF_RFSTR1  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.2f%33t%%-15.7g%48t\
%%-15.7g%80t \n"
define  PSF_RWSTR1   "%4t%-10.3f%14t%-10.3f%24t%-9.2f%33t%-15.7g%48t\
%-15.7g%80t\\\n"

define  PSF_RNSTR2  "#N%4tEXCENTER%14tEYCENTER%24tERSIGMA%33tEAMPLITUDE%48t\
ESKY%63tIER%68tERROR%80t\\\n"
define  PSF_RUSTR2  "#U%4tpixels%14tpixels%24tpixels%33tcounts%48t\
counts%63t##%68terrors%80t\\\n"
define  PSF_RFSTR2  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.3f%33t%%-15.7g%48t\
%%-15.7g%63t%%-5d%68t%%-13s%80t \n"
define  PSF_RWSTR2  "%4t%-10.3f%14t%-10.3f%24t%-9.3f%33t%-15.7g%48t\
%-15.7g%63t%-5d%68t%-13.13s\n"

define  PSF_ENSTR1  "#N%4tXCENTER%14tYCENTER%24tXSIGMA%33tYSIGMA%42t\
ROT%49tAMPLITUDE%64tSKY%80t\\\n"
define  PSF_EUSTR1  "#U%4tpixels%14tpixels%24tpixels%33tpixels%42t\
deg%49tcounts%64tcounts%80t\\\n"
define  PSF_EFSTR1  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.2f%33t%%-9.2f%42t\
%%-7.2f%49t%%-15.7g%64t%%-15.7g%80t \n"
define  PSF_EWSTR1  "%4t%-10.3f%-10.3f%-9.2f%-9.2f%-7.2f%-15.7g%-15.7g%80t\\\n"

define  PSF_ENSTR2  "#N%4tEXCENTER%14tEYCENTER%24tEXSIGMA%33tEYSIGMA%42t\
EROT%49tEAMPLITDE%64tESKY%80t\\\n"
define  PSF_EUSTR2  "#U%4tpixels%14tpixels%24tpixels%33tpixels%42t\
deg%49tcounts%64tcounts%80t\\\n"
define  PSF_EFSTR2  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.3f%33t%%-9.3f%42t\
%%-7.2f%49t%%-15.7g%64t%%-15.7g%80t \n"
define  PSF_EWSTR2  "%4t%-10.3f%-10.3f%-9.3f%-9.3f%-7.2f%-15.7g%-15.7g%80t\\\n"

define  PSF_ENSTR3  "#N%4tIER%9tERROR%80t\\\n"
define  PSF_EUSTR3  "#U%4t##%9terrors%80t\\\n"
define  PSF_EFSTR3  "#F%4t%%-8d%9t%%-13s%80t \n"
define  PSF_EWSTR3  "%4t%-5d%-13.13s%80t \n"


define  PSF_MNSTR1   "#N%4tXCENTER%14tYCENTER%24tRGYRAT%33tELLIP%42t\
ROT%49tAMPLITUDE%64tSKY%80t\\\n"
define  PSF_MUSTR1  "#U%4tpixels%14tpixels%24tpixels%33tratio%42tdeg%49t\
counts%64tcounts%80t\\\n"
define  PSF_MFSTR1  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.2f%33t%%-9.2f%42t\
%%-7.2f%49t%%-15.7g%64t%%-15.7f%80t \n"
define  PSF_MWSTR1  "%4t%-10.3f%-10.3f%-9.2f%-9.2f%-7.2f%-15.7g%-15.7g%80t\\\n"

define  PSF_MNSTR2  "#N%4tEXCENTER%14tEYCENTER%24tERGYRAT%33tEELLIP%42t\
EROT%49tEAMPLITUDE%64tESKY%80t\\\n"
define  PSF_MUSTR2  "#U%4tpixels%14tpixels%24tpixels%33tratio%42tdeg%49t\
counts%64tcounts%80t\\\n"
define  PSF_MFSTR2  "#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.3f%33t%%-9.3f%42t\
%%-7.2f%49t%%-15.7g%64t%%-15.7g%80t \n"
define  PSF_MWSTR2  "%4t%-10.3f%-10.3f%-9.3f%-9.3f%-7.2f%-15.7g%-15.7g%80t\\\n"

define  PSF_MNSTR3  "#N%4tIER%9tERROR%80t\\\n"
define  PSF_MUSTR3  "#U%4t##%9terrors%80t\\\n"
define  PSF_MFSTR3  "#F%4t%%-8d%9t%%-13s%80t \n"
define  PSF_MWSTR3  "%4t%-5d%-13.13s%80t \n"


# AP_WFRES -- Write the results of the fitpsf task to the output file.

procedure ap_wfres (ap, fd, ier)

pointer	ap	# pointer to apphot structure
int	fd	# output file descriptor
int	ier	# comment string

pointer	psf

begin
	# Initialize.
	if (fd == NULL)
	    return
	psf = AP_PPSF(ap)

	# Print the parameters.
	switch (AP_PSFUNCTION(psf)) {
	case AP_RADGAUSS:
	    call fprintf (fd, PSF_RWSTR1)
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+4])
	    call fprintf (fd, PSF_RWSTR2)
		call pargr (Memr[AP_PPERRS(psf)+1])
		call pargr (Memr[AP_PPERRS(psf)+2])
		call pargr (Memr[AP_PPERRS(psf)+3])
		call pargr (Memr[AP_PPERRS(psf)])
		call pargr (Memr[AP_PPERRS(psf)+4])
		call pargi (ier)
	case AP_ELLGAUSS:
	    call fprintf (fd, PSF_EWSTR1)
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)+4])
		call pargr (Memr[AP_PPARS(psf)+5])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+6])
	    call fprintf (fd, PSF_EWSTR2)
		call pargr (Memr[AP_PPERRS(psf)+1])
		call pargr (Memr[AP_PPERRS(psf)+2])
		call pargr (Memr[AP_PPERRS(psf)+3])
		call pargr (Memr[AP_PPERRS(psf)+4])
		call pargr (Memr[AP_PPERRS(psf)+5])
		call pargr (Memr[AP_PPERRS(psf)])
		call pargr (Memr[AP_PPERRS(psf)+6])
	    call fprintf (fd, PSF_EWSTR3)
		call pargi (ier)
	case AP_MOMENTS:
	    call fprintf (fd, PSF_MWSTR1)
		call pargr (Memr[AP_PPARS(psf)+1])
		call pargr (Memr[AP_PPARS(psf)+2])
		call pargr (Memr[AP_PPARS(psf)+3])
		call pargr (Memr[AP_PPARS(psf)+4])
		call pargr (Memr[AP_PPARS(psf)+5])
		call pargr (Memr[AP_PPARS(psf)])
		call pargr (Memr[AP_PPARS(psf)+6])
	    call fprintf (fd, PSF_MWSTR2)
		call pargr (Memr[AP_PPERRS(psf)+1])
		call pargr (Memr[AP_PPERRS(psf)+2])
		call pargr (Memr[AP_PPERRS(psf)+3])
		call pargr (Memr[AP_PPERRS(psf)+4])
		call pargr (Memr[AP_PPERRS(psf)+5])
		call pargr (Memr[AP_PPERRS(psf)])
		call pargr (Memr[AP_PPERRS(psf)+6])
	    call fprintf (fd, PSF_MWSTR3)
		call pargi (ier)
	default:
		;
	}

	# Print the error message.
	switch (ier) {
	case AP_NOPSFAREA:
	    call pargstr ("OffImage")
	case AP_PSF_OUTOFBOUNDS:
	    call pargstr ("EdgeImage")
	case AP_NPSF_TOO_SMALL:
	    call pargstr ("TooFewPts")
	case AP_PSF_SINGULAR:
	    call pargstr ("Singular")
	case AP_PSF_NOCONVERGE:
	    call pargstr ("BadFit")
	default:
	    call pargstr ("NoError")
	}
end


# RADHDR -- Write the column headers for the radial gaussian function.

procedure radhdr (ap, fd)

pointer	ap	# pointer to apphot structure
int	fd	# output file descriptor

begin
	# Print the keyword names.
	call ap_idhdr (ap, fd)

	call fprintf (fd, PSF_RNSTR1)
	call fprintf (fd, PSF_RUSTR1)
	call fprintf (fd, PSF_RFSTR1)
	call fprintf (fd, "#\n")

	call fprintf (fd, PSF_RNSTR2)
	call fprintf (fd, PSF_RUSTR2)
	call fprintf (fd, PSF_RFSTR2)
	call fprintf (fd, "#\n")
end


# ELHDR -- Write the column headers for the elliptical gaussian function.

procedure elhdr (ap, fd)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor

begin
	# Print the keywords.
	call ap_idhdr (ap, fd)

	call fprintf (fd, PSF_ENSTR1)
	call fprintf (fd, PSF_EUSTR1)
	call fprintf (fd, PSF_EFSTR1)
	call fprintf (fd, "#\n")

	call fprintf (fd, PSF_ENSTR2)
	call fprintf (fd, PSF_EUSTR2)
	call fprintf (fd, PSF_EFSTR2)
	call fprintf (fd, "#\n")

	call fprintf (fd, PSF_ENSTR3)
	call fprintf (fd, PSF_EUSTR3)
	call fprintf (fd, PSF_EFSTR3)
	call fprintf (fd, "#\n")
end


# MOMHDR -- Write the column headers for the moments function.

procedure momhdr (ap, fd)

pointer	ap		# pointer to apphot structure
int	fd		# output file descriptor

begin
	# Print the keywords.
	call ap_idhdr (ap, fd)

	call fprintf (fd, PSF_MNSTR1)
	call fprintf (fd, PSF_MUSTR1)
	call fprintf (fd, PSF_MFSTR1)
	call fprintf (fd, "#\n")

	call fprintf (fd, PSF_MNSTR2)
	call fprintf (fd, PSF_MUSTR2)
	call fprintf (fd, PSF_MFSTR2)
	call fprintf (fd, "#\n")

	call fprintf (fd, PSF_MNSTR3)
	call fprintf (fd, PSF_MUSTR3)
	call fprintf (fd, PSF_MFSTR3)
	call fprintf (fd, "#\n")
end
