include "../lib/apphot.h"
include "../lib/find.h"

define	FD_NSTR	"#N%4tXCENTER%13tYCENTER%22tMAG%31tSHARP%39tROUND%47tID%80t\\\n"
define	FD_USTR "#U%4tpixels%13tpixels%22t#%31t#%39t#%47t#%80t\\\n"
define	FD_FSTR	"#F%4t%%-12.2f%13t%%-9.2f%22t%%-9.3f%31t%%-8.3f%39t%%-8.3f%47t%%-6d%80t\\\n"
define	FD_WSTR	"%4t%-9.2f%-9.2f%-9.3f%-8.3f%-8.3f%-6d\n"

# AP_WFDPARAM -- Procedure to write the daofind parameters to the output file.

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


# AP_FDHDR -- Procedure to write the column banner to the output file.

procedure ap_fdhdr (ap, out)

pointer	ap	# pointer to the apphot structure
pointer	out	# file descriptor

begin
	if (out == NULL)
	    return

	call fprintf (out, FD_NSTR)
	call fprintf (out, FD_USTR)
	call fprintf (out, FD_FSTR)
	call fprintf (out, "#\n")
end


# AP_PFDHDR -- Procedure to print the column banner on the standard output.

procedure ap_pfdhdr ()

begin
	call printf (" XCENTER  YCENTER     MAG    SHARP   ROUND    ID\n\n")
end


# APSTDOUT -- Procedure to print the daofind results on the standard output.

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


# APDTFOUT -- Procedure to write the daofind results to the output file.

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
