include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include	"oned.h"
include	"idsmtn.h"

# T_COINCOR -- Apply coincidence corrections to spectra

procedure t_coincor ()

char	image[SZ_FNAME], ofile[SZ_FNAME]
char	rec_numbers[SZ_LINE], newfile[SZ_FNAME], ccoptions[SZ_LINE]
int	root, nfiles, start_rec, nrecs
int	ccmode, npts
int	records[3, MAX_RANGES]
real	dtime, power, expo
pointer	sp, imin, imout, ids, pixin, pixout

int	clpopni(), clplen(), clgeti(), clgwrd()
int	get_next_image(), decode_ranges(), strlen()
real	clgetr()
pointer	immap(), imgl1r(), impl1r()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	call clgstr ("output", ofile, SZ_FNAME)
	if (ofile[1] != EOS)
	    start_rec = clgeti ("start_rec")

	# Get mode of correction
	ccmode = clgwrd ("ccmode", ccoptions, SZ_LINE, ",photo,iids,")

	# And dead time and power law coefficient
	dtime = clgetr ("deadtime")
	power = clgetr ("power")

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	# Mark dynamic space
	call smark (sp)

	# Allocate space for IDS header
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Loop over all input images by subsets
	while (get_next_image (root, records, nrecs, image, SZ_FNAME) != EOF) {

	    # Open input image
	    iferr (imin = immap (image, READ_WRITE, 0)) {
		call eprintf ("Header info not available for [%s]")
		    call pargstr (image)
	        start_rec = start_rec + 1
		next
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids, imin, 1)

	    if (CO_FLAG(ids) > 0) {
		call printf ("[%s] already coincidence corrected\n")
		call pargstr (IM_HDRFILE(imin))
	    } else {
	        # Open output image
	        if (ofile[1] != EOS) {
	            call strcpy (ofile, newfile, SZ_FNAME)
	            call sprintf (newfile[strlen (newfile) + 1], SZ_FNAME,
			".%04d")
	                call pargi (start_rec)
	            start_rec = start_rec + 1

	            imout = immap (newfile, NEW_COPY, imin)
	            IM_PIXTYPE (imout) = TY_REAL
	        } else
		    imout = imin

		pixin = imgl1r (imin)
		pixout = impl1r (imout)
		npts = IM_LEN (imin, 1)
		expo = ITM (ids)
		call coincor (Memr[pixin], Memr[pixout], npts, ids, expo,
		    dtime, power, ccmode)

	        # Update keyword
	        call store_keywords (ids, imout)

	        # Send user report
	        call printf ("[%s] --> [%s] %s\n")
	            call pargstr (IM_HDRFILE(imin))
	            call pargstr (IM_HDRFILE(imout))
	            call pargstr (IM_TITLE(imout))

		if (imout != imin)
	            call imunmap (imout)
	    }
	    call imunmap (imin)
	}

	call clputi ("onedspec.next_rec", start_rec)
	call sfree (sp)
	call clpcls (root)
end
