include	<mach.h>
include	<ctype.h>
include	<error.h>
include	<imhdr.h>
include	<fset.h>
include "oned.h"
include	"idsmtn.h"

procedure t_coefs ()

char	image[SZ_FNAME]
char	rec_numbers[SZ_LINE]
char	database[SZ_LINE]
int	root, nfiles
int	nrecs, records[3, MAX_RANGES]
int	i, ncoefs
pointer	im, dt, sp, ids, dtname

int	clpopni(), clplen()
int	get_next_image(), decode_ranges()
pointer	immap(), dtmap1()

begin
	# Open input file name template
	root   = clpopni ("input")
	nfiles = clplen (root)

	# Get range specification if any
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Add text to file name?
	call clgstr ("database", database, SZ_LINE)

	# Force STDOUT flush
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Initialize range decoder
	call reset_next_image ()

	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)
	call salloc (dtname, SZ_FNAME, TY_CHAR)

	# Loop over all input images - print name on STDOUT
	while (get_next_image (root, records, nrecs, image, SZ_FNAME) != EOF) {

	    ifnoerr (im = immap (image, READ_ONLY, 0)) {
		call load_ids_hdr (ids, im, 1)
		ncoefs = DF_FLAG (ids)
		if (ncoefs > 1) {

		    call strcpy ("id", Memc[dtname], SZ_FNAME)
		    call imgcluster (image, Memc[dtname+2], SZ_FNAME)
		    dt = dtmap1 (database, Memc[dtname], APPEND)


		    call dtptime (dt)
		    call dtput (dt, "begin\tidentify %s\n")
	    		call pargstr (image)
		    call dtput (dt, "\tid\t%s\n")
	    		call pargstr (image)
		    call dtput (dt, "\ttask\tidentify\n")
		    call dtput (dt, "\timage\t%s\n")
	    		call pargstr (image)

		    # Coefficients count + 4
		    call dtput (dt, "\tcoefficients\t%d\n")
			call pargi (ncoefs+4)

		    # Legendre flag
		    call dtput (dt, "\t\t2\n")

		    # Nr coefficients
		    call dtput (dt, "\t\t%1d\n")
			call pargi (ncoefs)

		    # Start pixel
		    call dtput (dt, "\t\t1\n")
		    # End pixel
		    call dtput (dt, "\t\t1024\n")

		    # Individual coeffs
		    do i = 1, ncoefs {
			call dtput (dt, "\t\t%10.4f\n")
			    call pargr (Memr[POINT(ids)+i-1])
		    }

		    call dtput (dt, "\n")
		    call dtunmap (dt)
		}
		call printf ("[%s] %d coefficients written\n")
		    call pargstr (image)
		    call pargi (ncoefs)
		call imunmap (im)
	    }
	}

	call sfree (sp)
	call clpcls (root)
end
