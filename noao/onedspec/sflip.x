include	<imhdr.h>
include	<error.h>
include	<fset.h>
include "idsmtn.h"

# T_SFLIP -- Flip the spectra about the dispersion axis.

procedure t_sflip ()

int	rootin			# List of input root names
char	rec_numbers[SZ_LINE]	# List of input record numbers
char	rootout[SZ_FNAME]	# Output root name
int	start_rec		# Starting output record number

char	image[SZ_FNAME]
int	i, nlen, nrecs, records[3, MAX_RANGES]
pointer	sp, ids, in, out, pixin, pixout

int	clpopni(), clgeti()
int	get_next_image(), decode_ranges()
pointer	immap(), imgl1r(), impl1r()

begin
	# Open input file name template
	rootin = clpopni ("input")

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get rootname for output files and starting record
	call clgstr ("output", rootout, SZ_FNAME)
	start_rec = clgeti ("start_rec")

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	# Allocate space for IDS header
	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Loop over all input images - flip and make new image.
	while (get_next_image(rootin, records, nrecs, image, SZ_FNAME) != EOF) {
	    # Open the input and output images.
	    iferr (in = immap (image, READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    }

	    call sprintf (image, SZ_FNAME, "%s.%04d")
	        call pargstr (rootout)
	        call pargi (start_rec)
	    start_rec = start_rec + 1

	    call printf ("[%s]: %s\n")
	        call pargstr (image)
	        call pargstr (IM_TITLE (in))

	    iferr (out = immap (image, NEW_COPY, in)) {
		call imunmap (in)
		call erract (EA_WARN)
		next
	    }

	    # Load header data into input and output header arrays
	    call load_ids_hdr (ids, in, 1)

	    # Map image pixels
	    pixin  = imgl1r (in, 1)
	    pixout = impl1r (out, 1)

	    # Flip input pixels.
	    nlen = IM_LEN (in, 1)
	    do i = 1, nlen
	        Memr[pixout+i-1] = Memr[pixin+nlen-i]

	    # Fix the header parameters.
	    if (!IS_INDEF (W0(ids))) {
	        W0(ids) = W0(ids) + (nlen - 1) * WPC(ids)
	        WPC(ids) = -WPC(ids)
	    }
	    i = NP1(ids)
	    NP1(ids) = nlen - NP2(ids)
	    NP2(ids) = nlen - i

	    call store_keywords (ids, out)
	    call imunmap (in)
	    call imunmap (out)
	}

	# Update record number
	call clputi ("onedspec.next_rec", start_rec)

	# Free space
	call sfree (sp)
	call clpcls (rootin)
end
