include	<mach.h>
include	<imhdr.h>
include	<error.h>
include	"rrcopy.h"

# T_RRCOPY -- The main procedure for the RCOPY reader.  The RCOPY reader
# converts IPPS rasters written in RCOPY format to IRAF images.  All IPPS
# rasters on an RCOPY tape are in a single file.  Each raster header must
# be read before the image can be either skipped or read.  T_RRCOPY gets
# parameters from the cl and decodes the string of rasters to be read.  It
# then calls READ_HEADER for each raster on the tape.  The header information 
# is printed if print_header=true.  If make_image = true, the image is 
# converted to an IRAF image by READ_IMAGE.  Otherwise, the image is skipped 
# with SKIP_IMAGE.  T_RRCOPY terminates when the raster list is depleted or
# the tape is at EOT.
#
# Modified 26-JULY-88 to allow for multiple rcopy files on a single tape.
# This allows for rcopy format data to be archived in multiple files on
# one tape.  The task is still run once per input file.  The user is queried
# (hidden parameter) for the data file to be read.  The tape file is actually
# datafile + 1 because of the ANSI label on each rrcopy tape.  (ShJ)

procedure t_rrcopy ()

pointer	sp, rp
bool	make_image, print_header, bad_header
char 	rcopy_file[SZ_FNAME], iraf_file[SZ_FNAME]
char 	out_fname[SZ_FNAME], raster_list[SZ_LINE]
int 	rd, ras_number, current_ras, nras, stat, tapefile
int	ranges[3, MAX_RANGES], data_type, init

bool	clgetb()
char	clgetc()
int	get_data_type(), position_rcopy(), rc_read_cyber_init(), clgeti()
int	mtopen(), decode_ranges(), get_next_number(), rc_header_read(), strlen()
int	mtfile()

begin
	# Allocate space on stack for program data structure
	call smark (sp)
	call salloc (rp, LEN_RP, TY_STRUCT)

	# Get input filename and open tape drive to second file, skipping label
	call clgstr ("rcopy_file", rcopy_file, SZ_FNAME)
	if (mtfile (rcopy_file) == YES) {
	    tapefile = clgeti ("datafile") + 1
	    call mtfname (rcopy_file, tapefile, rcopy_file, SZ_FNAME)
	}
	rd = mtopen (rcopy_file, READ_ONLY, SZ_BUFFER)
	init = rc_read_cyber_init()

	# Get output root filename if it will be needed
	make_image = clgetb ("make_image")
	if (make_image) {
	    call clgstr ("iraf_file", iraf_file, SZ_FNAME)
	    data_type = get_data_type (clgetc ("data_type"))
	    if (data_type == ERR)
		data_type = NOT_SET
	} 

	# Set options
	print_header = clgetb ("print_header")

	# Expand list of rasters to be read from tape
	call clgstr ("raster_list", raster_list, SZ_LINE)
	if (decode_ranges (raster_list, ranges, MAX_RANGES, nras) == ERR)
	    call error (0, "Illegal raster number list")

	ras_number = 0
	current_ras = 1
	while (get_next_number (ranges, ras_number) != EOF) {
	    # Position tape to first record of ras_number
	    if (current_ras != ras_number) {
	        iferr (stat = position_rcopy (rd, current_ras, ras_number, rp))
		    call erract (EA_FATAL)
		if (stat == EOF)
		    break
	    }
	    
	    # Assume header is good
	    bad_header = false
 	    iferr {
 	        stat = rc_header_read (rd, rp)
	    } then {
		# Error reading header; will attempt to skip raster
		bad_header = true
		call erract (EA_WARN)
	    }

	    if (stat == EOF) {
	        call printf ("\nRCOPY tape at End of Tape\n")
		break
	    }

	    if (print_header)
		call rc_list_header (rp, ras_number)
	    call flush (STDOUT)

	    if (make_image && ! bad_header) {
		# Generate output filename
	        call strcpy (iraf_file, out_fname, SZ_FNAME)
		if (nras > 1) {
		    call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
				  "%03d")
		        call pargi (ras_number)
		}
		iferr (call rc_read_image (rd, out_fname, data_type, rp))
		    call erract (EA_FATAL)
	    } else
		iferr (call rc_skip_image (rd, rp))
		    call erract (EA_FATAL)
		
	    # Increment tape position
	    current_ras = current_ras + 1
	}

	# Return space allocated for rp, close tape unit
	call close (rd)
	call sfree (sp)
end


# POSITION_RCOPY -- Position the tape to the first
# record of the next raster to be read.  Each raster header must
# be read; each image can then be skipped.

int procedure position_rcopy (rd, current_ras, ras_number, rp)

int	rd, current_ras, ras_number
pointer	rp
int	nras_skip, i, stat
int	rc_header_read()
errchk	rc_skip_image

begin
        nras_skip = ras_number - current_ras
        for (i=1; i <= nras_skip; i=i+1) {
	    stat = rc_header_read (rd, rp)
	    if (stat == EOF) {
		call printf ("Cannot position RCOPY tape beyond EOF\n")
		return (EOF)
	    }
            call rc_skip_image (rd, rp)
	    current_ras = current_ras + 1
        }
	return (OK)
end
