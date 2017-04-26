include "../lib/apphot.h"
include "../lib/find.h"
include "../lib/center.h"
include "../lib/fitsky.h"

# define the #N, #U, and #F daofind strings

define	FD_NSTR	"#N%4tXCENTER%14tYCENTER%24tMAG%33tSHARPNESS%45t\
SROUND%57tGROUND%69tID%80t\\\n"
define	FD_USTR "#U%4tpixels%14tpixels%24t#%33t#%45t#%57t#%69t#%80t\\\n"
define	FD_FSTR	"#F%4t%%-13.3f%14t%%-10.3f%24t%%-9.3f%33t%%-12.3f%45t\
%%-12.3f%57t%%-12.3f%69t%%-6d%80t\\\n"
define	FD_WSTR	"%4t%-10.3f%-10.3f%-9.3f%-12.3f%-12.3f%-12.3f%-6d\n"

# AP_WFDPARAM -- Write the daofind parameters to the output file.

procedure ap_wfdparam (out, ap)

int	out	# the output file descriptor
pointer	ap	# pointer to the apphot structure

pointer	sp, str
real	apstatr()

begin
	if (out == NULL)
	    return

	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Write out the parameters.
	call ap_param (ap, out, "daofind")
	#call apstats (ap, IMNAME, Memc[str], SZ_FNAME)
	call apstats (ap, IMROOT, Memc[str], SZ_FNAME)
	call ap_sparam (out, KY_IMNAME, Memc[str], "imagename", "image name")
	call ap_rparam (out, KY_FWHMPSF, apstatr (ap, FWHMPSF), UN_ASCALEUNIT,
	    "fwhm of the psf")
	call ap_rparam (out, KY_THRESHOLD, apstatr (ap, THRESHOLD),
	    UN_FSIGMA, "detection threshold in sigma")
	call ap_rparam (out, KY_NSIGMA, apstatr (ap, NSIGMA), UN_FSIGMA,
	    "size of the kernel in fwhmpsf")
	call ap_rparam (out, KY_RATIO, apstatr (ap, RATIO), UN_FNUMBER,
	    "ratio of ysigma to xsigma")
	call ap_rparam (out, KY_THETA, apstatr (ap, THETA), UN_FDEGREES,
	    "position angle in degrees")
	call fprintf (out, "#\n")
	call ap_rparam (out, KY_SHARPLO, apstatr (ap, SHARPLO), UN_FNUMBER,
	    "lower sharpness bound")
	call ap_rparam (out, KY_SHARPHI, apstatr (ap, SHARPHI), UN_FNUMBER,
	    "higher sharpness bound")
	call ap_rparam (out, KY_ROUNDLO, apstatr (ap, ROUNDLO), UN_FNUMBER,
	    "lower roundness bound")
	call ap_rparam (out, KY_ROUNDHI, apstatr (ap, ROUNDHI), UN_FNUMBER,
	    "higher roundness bound")
	call fprintf (out, "#\n")

	# Write out the header banner.
	call ap_fdhdr (ap, out)

	call sfree (sp)
end


# AP_FDHDR -- Write the daofind column banner to the output file.

procedure ap_fdhdr (ap, out)

pointer	ap	# pointer to the apphot structure (unused)
int	out	# output file descriptor

begin
	if (out == NULL)
	    return

	call fprintf (out, FD_NSTR)
	call fprintf (out, FD_USTR)
	call fprintf (out, FD_FSTR)
	call fprintf (out, "#\n")
end


# APSTDOUT -- Print the daofind results on the standard output.

procedure apstdout (density, ptrs, ncols, nbox, cols, x, y, sharp, round1,
        round2, nstars, ntotal, threshold)

real	density[ncols,nbox]	# array of densities
int	ptrs[ARB]		# array of line pointers 
int	ncols, nbox		# dimensions of the data
int	cols[ARB]		# array of column numbers
real	x[ARB]			# x coordinates
real	y[ARB]			# y coordinates
real	sharp[ARB]		# sharpness
real	round1[ARB]		# first roundness parameter
real	round2[ARB]		# second roundness parameter
int	nstars			# number of detected stars in the line
int	ntotal			# total number of detected objects
real	threshold		# threshold for detection

int	i, middle
real	den

begin
	middle = 1 + nbox / 2
	do i = 1, nstars {
	    call printf (" %7.2f  %7.2f  %7.3f  %6.3f  %6.3f  %6.3f  %4d\n")
		call pargr (x[i])
		call pargr (y[i])
		if (threshold <= 0.0)
		    den = INDEFR
		else
		    den = -2.5 * log10 (density[cols[i],ptrs[middle]] /
		        threshold)
		call pargr (den)
		call pargr (sharp[i])
		call pargr (round1[i])
		call pargr (round2[i])
		call pargi (ntotal + i)
	}
end


# APDTFOUT -- Write the daofind results to the output file.

procedure apdtfout (fd, density, ptrs, ncols, nbox, cols, x, y, sharp, round1,
	round2, nstars, ntotal, threshold, stid)

int	fd				# the output file descriptor
real	density[ncols, nbox]		# densities
int	ptrs[ARB]			# array of line pointers
int	ncols, nbox			# dimensions of the data
int	cols[ARB]			# column numbers
real	x[ARB]				# xcoords
real	y[ARB]				# y coords
real	sharp[ARB]			# sharpnesses
real	round1[ARB]			# first roundness
real	round2[ARB]			# second roundness
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
		call pargr (round1[i])
		call pargr (round2[i])
		call pargi (stid + ntotal + i - 1)
	}
end


# define the #N, #U and #K id strings

define	ID_NSTR	"#N%4tIMAGE%24tXINIT%34tYINIT%44tID%50tCOORDS%73tLID%80t\\\n"
define	ID_USTR	"#U%4timagename%24tpixels%34tpixels%44t##%50tfilename%73t\
##%80t\\\n"
define	ID_FSTR	"#F%4t%%-23s%24t%%-10.3f%34t%%-10.3f%44t%%-6d%50t%%-23s%73t\
%%-6d%80t \n"
define	ID_WSTR "%-23.23s%24t%-10.3f%34t%-10.3f%44t%-6d%50t%-23.23s%73t\
%-6d%80t%c\n"

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
	call apstats (ap, IMROOT, Memc[imname], SZ_FNAME)
	call apstats (ap, CLROOT, Memc[clname], SZ_FNAME)
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

define	CTR_NSTR  "#N%4tXCENTER%15tYCENTER%26tXSHIFT%34tYSHIFT%42tXERR%50t\
YERR%66tCIER%71tCERROR%80t\\\n"
define	CTR_USTR  "#U%4tpixels%15tpixels%26tpixels%34tpixels%42tpixels%50t\
pixels%66t##%71tcerrors%80t\\\n"
define	CTR_FSTR  "#F%4t%%-14.3f%15t%%-11.3f%26t%%-8.3f%34t%%-8.3f%42t\
%%-8.3f%50t%%-15.3f%66t%%-5d%71t%%-9s%80t \n"
define  CTR_WSTR   "%4t%-11.3f%-11.3f%-8.3f%-8.3f%-8.3f%-15.3f%-5d%-9.9s\
%80t%c\n"


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
	    call pargr (apstatr (ap, OXCENTER))
	    call pargr (apstatr (ap, OYCENTER))
	    call pargr (apstatr (ap, OXSHIFT))
	    call pargr (apstatr (ap, OYSHIFT))
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
	    call strcpy ("OffImage", str, maxch)
        case AP_CTR_OUTOFBOUNDS:
	    call strcpy ("EdgeImage", str, maxch)
	case AP_CTR_NTOO_SMALL:
	    call strcpy ("TooFewPts", str, maxch)
	case AP_CTR_SINGULAR:
	    call strcpy ("Singular", str, maxch)
	case AP_CTR_NOCONVERGE:
	    call strcpy ("BadFit", str, maxch)
	case AP_CTR_BADSHIFT:
	    call strcpy ("BigShift", str, maxch)
	case AP_CTR_LOWSNRATIO:
	    call strcpy ("LowSnr", str, maxch)
	case AP_CTR_BADDATA:
	    call strcpy ("BadPixels", str, maxch)
	default:
	    call strcpy ("NoError", str, maxch)
	}
end


# define the #N, #U and #K fitsky strings

define	SKY_NSTR  "#N%4tMSKY%19tSTDEV%34tSSKEW%49tNSKY%56tNSREJ%66tSIER\
%71tSERROR%80t\\\n"
define	SKY_USTR  "#U%4tcounts%19tcounts%34tcounts%49tnpix%56tnpix%66t##\
%71tserrors%80t\\\n"
define	SKY_FSTR  "#F%4t%%-18.7g%19t%%-15.7g%34t%%-15.7g%49t%%-7d%56t\
%%-9d%66t%%-5d%71t%%-9s%80t \n"
define  SKY_WSTR  "%4t%-15.7g%-15.7g%-15.7g%-7d%-9d%-5d%-9.9s%80t%c\n"

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
	    call strcpy ("OffImage", str, maxch)
	case AP_SKY_OUTOFBOUNDS:
	    call strcpy ("EdgeImage", str, maxch)
	case AP_NOHISTOGRAM:
	    call strcpy ("NoHist", str, maxch)
	case AP_FLAT_HIST:
	    call strcpy ("FlatHist", str, maxch)
	case AP_NSKY_TOO_SMALL:
	    call strcpy ("TooFewPts", str, maxch)
	case AP_SKY_SINGULAR:
	    call strcpy ("Singular", str, maxch)
	case AP_SKY_NOCONVERGE:
	    call strcpy ("BadFit", str, maxch)
	case AP_NOGRAPHICS:
	    call strcpy ("NoGraph", str, maxch)
	case AP_NOSKYFILE:
	    call strcpy ("NoFile", str, maxch)
	case AP_EOFSKYFILE:
	    call strcpy ("ShortFile", str, maxch)
	case AP_BADSKYSCAN:
	    call strcpy ("BadRecord", str, maxch)
	case AP_BADPARAMS:
	    call strcpy ("BadParams", str, maxch)
	default:
	    call strcpy ("NoError", str, maxch)
	}
end
