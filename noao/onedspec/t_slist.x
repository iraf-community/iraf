include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include "oned.h"
include "idsmtn.h"

# SLIST -- Spectrum Lister 
#
# Lists header information from IIDS/IRS format header

procedure t_slist ()

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE]
int	root, nfiles
int	nrecs, records[3, MAX_RANGES]
int	long_header
pointer	ids, im, sp

int	i, clpopni(), clplen(), btoi()
int	get_next_image(), decode_ranges()
bool	clgetb()
pointer	immap()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Long or short header
	long_header = btoi (clgetb ("long_header"))

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
	while (get_next_image (root, records, nrecs, image, 
	    SZ_FNAME) != EOF) {

	    # Open image
	    ifnoerr (im = immap (image, READ_ONLY, 0)) {

		if (long_header == YES)
		    do i = 1, IM_LEN(im,2) {
			call load_ids_hdr (ids, im, i)
			call print_header (im, ids, long_header, image)
		    }
		else {
		    call load_ids_hdr (ids, im, 1)
		    call print_header (im, ids, long_header, image)
		}

		call imunmap (im)
	    }
	}

	# Free space
	call sfree (sp)
	call clpcls (root)

	# Null out record string to avoid learn mode
	call clpstr ("records", "")
end

# PRINT_HEADER -- print the ids header in either long or short mode.

procedure print_header (im, ids, long_header, image)
 
pointer	im		# Pointer to image
pointer	ids		# Pointer to program data structure
int	long_header	# Print header in long format (YES/NO)?
char	image[SZ_FNAME]	# Image name

int	i

begin
	if (long_header == YES) {
	    call printf ("[%s]: %4dpts %s\n")
	        call pargstr (image)
		call pargi (IM_LEN (im,1))
	        call pargstr (IM_TITLE(im))

	    if (OFLAG(ids) == 1) {
		call printf ("oflag = OBJECT, beam_number = %d")
		    call pargi (BEAM(ids))
	    } else if (OFLAG (ids) == 0) {
		call printf ("oflag = SKY,    beam_number = %d")
		    call pargi (BEAM(ids))
	    }
	    call printf (",\n")

	    # Airmass may not be in header. It could be computed if
	    # if the observatory latitude were available.

	    call printf ("airmass = %5.3f,%25tW0 = %0.3f,")
	        call pargr (AIRMASS(ids))
	        call pargr (W0(ids))
	    call printf ("   WPC = %0.5g,      ITM = %.2f,\n")
	        call pargr (WPC(ids))
		call pargr (ITM(ids))
	    call printf ("NP1 = %d, NP2 = %d,")
		call pargi (NP1(ids))
		call pargi (NP2(ids))
	    call printf ("    UT = %0.1h,   ST = %0.1h,\n")
		call pargr (UT(ids))
		call pargr (ST(ids))
	    call printf ("HA = %0.2h,")
		call pargr (HA(ids))
	    call printf ("        RA = %0.2h,   DEC = %0.1h,\n")
		call pargr (RA(ids))
		call pargr (DEC(ids))
	    call printf ("df = %d, sm = %d, qf = %d, dc = %d, qd = %d, ") 
		call pargi (DF_FLAG(ids))
		call pargi (SM_FLAG(ids))
		call pargi (QF_FLAG(ids))
		call pargi (DC_FLAG(ids))
		call pargi (QD_FLAG(ids))
	    call printf ("ex = %d, bs = %d, ca = %d, co = %d")
		call pargi (EX_FLAG(ids))
		call pargi (BS_FLAG(ids))
		call pargi (CA_FLAG(ids))
		call pargi (CO_FLAG(ids))

	    # The df coeffecients are printed out in the case where the df
	    # flag is set, and the first coefficient is nonzero.  The later
	    # condition is a test for IDSOUT data, where the df coeffecients
	    # have been applied but not stored in the header.

	    if (DF_FLAG(ids) != -1 && Memr[POINT(ids)] != 0.) {
		call printf (",\n")
		do i = 1, DF_FLAG(ids) {
		    call printf ("df[%d] = %10.8g")
			call pargi(i)
			call pargr(Memr[POINT(ids)+i-1])
		    if (i != DF_FLAG(ids))
		        call printf (", ")
		    if (mod (i, 4) == 0)
		        call printf ("\n")
		}
		call printf ("\n")
	    } else
		call printf ("\n")
	    call printf ("\n")
	} else {
	    if (IM_NDIM(im) == 1)
	        call printf ("[%s]:%s %4ds %4dpts   %s\n")
	    else
	        call printf ("[%s]:%s %6.2fs %4dpts %dspectra   %s\n")

	        call pargstr (image)

		if (OFLAG(ids) == 1)
		    call pargstr ("o")
		else
		    call pargstr ("s")

		call pargr (ITM(ids))
		call pargi (IM_LEN(im,1))
	    if (IM_NDIM(im) > 1)
		call pargi (IM_LEN(im,2))
	        call pargstr (IM_TITLE(im))
	}
end
