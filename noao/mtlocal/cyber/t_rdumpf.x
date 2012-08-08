include	<mach.h>
include	<error.h>
include	<fset.h>
include	"cyber.h"

# T_RDUMPF-- The main procedure for the DUMPF reader.  Permanent files
# containing IPPS rasters are read in dumpf format and optionally
# converted to IRAF images.  Each permanent file is a seperate tape file;
# the IPPS rasters are sequentially stored in the permanent file, seperated
# by "zero length PRU's".  The first 48 words of each permanent file
# contain the Cyber permanent file table, catalog and file header information
# for the file.  This information is listed with task LDUMPF.  For each
# file in file_list, the file is opened.  Then for each raster in the file,
# READ_HEADER must be called, followed by either READ_IMAGE or SKIP_IMAGE.

procedure t_rdumpf()

pointer	sp, dt, dummy
bool	make_image, print_header, bad_header
char	dumpf_file[SZ_FNAME], iraf_file[SZ_FNAME], file_list[SZ_LINE]
char	out_fname[SZ_FNAME], raster_list[SZ_LINE], in_fname[SZ_FNAME]
int	fd, file_number, ras_number, current_ras
int	stat, nfile, nras
int	rasters[3, MAX_RANGES], files[3, MAX_RANGES], data_type

bool	clgetb()
char	clgetc()
int	get_data_type(), strlen(), mtfile()
int	get_cyber_words()
int	get_cyber_words_init(), read_dumpf_init(), position_dumpf()
int	mtopen(), decode_ranges(), get_next_number(), cy_read_header()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate space for program data structure and buffers
	call smark (sp)
	call salloc (dt, LEN_DT, TY_STRUCT)
	call salloc (dummy, NINT_CYBER_WRD * LEN_PFT, TY_INT)

	# Get paramters from cl
	call clgstr ("dumpf_file", dumpf_file, SZ_FNAME)
	if (mtfile (dumpf_file) == YES)
	    call clgstr ("file_list", file_list, SZ_LINE)
	else
	    call strcpy ("1", file_list, SZ_LINE)
	if (decode_ranges (file_list, files, MAX_RANGES, nfile) == ERR)
	    call error (0, "Illegal file list")

	call clgstr ("raster_list", raster_list, SZ_LINE)
	if (decode_ranges (raster_list, rasters, MAX_RANGES, nras) == ERR)
	    call error (1, "Illegal raster list")

	print_header = clgetb ("print_header")
	make_image = clgetb ("make_image")
	if (make_image) {
	    call clgstr ("iraf_file", iraf_file, SZ_FNAME)
	    data_type = get_data_type (clgetc ("data_type"))
	    if (data_type == ERR)
		data_type = NOT_SET
	}

	# Expand file_list and open dumpf_file
	file_number = 0
	while (get_next_number (files, file_number) != EOF) {

	    # Get the file name and open file.
	    if (mtfile (dumpf_file) == YES)
		call mtfname (dumpf_file, file_number + 1, in_fname, SZ_FNAME)
	    else
	        call strcpy (dumpf_file, in_fname, SZ_FNAME)
	    fd = mtopen (in_fname, READ_ONLY, SZ_TAPE_BUFFER)

	    # Position to first IPPS raster in file, skipping Cyber PFT etc.
	    stat = get_cyber_words_init()
	    stat = read_dumpf_init()

	    if (get_cyber_words (fd, Memi[dummy], LEN_PFT) == EOF) {
		call printf ("DUMPF Tape at EOF\n")
		call close (fd)
		call sfree (sp)
	        return
	    }


	    ras_number = 0
	    current_ras = 1
	    while (get_next_number (rasters, ras_number) != EOF) {
		# Position to first record of ras_number
		if (current_ras != ras_number) {
		    iferr (stat = position_dumpf (fd, current_ras, ras_number, 
			dt))
			call erract (EA_FATAL)
		    if (stat == EOF)
			break
		}

		bad_header = false
		iferr {
		    stat = cy_read_header (fd, dt)
		} then {
		    # Error reading header; will attempt to skip raster
		    bad_header = true
		    call erract (EA_WARN)
		}

		if (stat == EOF) {
		    call printf ("DUMPF Tape at End of File%d\n\n")
		    call pargi (file_number)
		    break
		}

		if (print_header)
		    call cy_list_header (dt, file_number, ras_number)

		if (make_image && ! bad_header) {
		    call strcpy (iraf_file, out_fname, SZ_FNAME)
		    if (nras > 1 || nfile > 1) {
			call sprintf (out_fname[strlen(out_fname)+1], SZ_FNAME,
				      "%d.%03d")
			    call pargi (file_number)
			    call pargi (ras_number)
		    }
		    iferr (call read_ipps_rows (fd, out_fname, data_type, dt))
			call erract (EA_FATAL)
		} else  
		    iferr (call cy_skip_image (fd, dt))
			call erract (EA_FATAL)

		current_ras = current_ras + 1
	    }
	    call close (fd)
	}
	call sfree (sp)
	return
end


# POSITION_DUMPF -- Position the tape to the first
# record of the next raster to be read.  Each raster header must
# be read; each image can then be skipped.

int procedure position_dumpf (rd, current_ras, ras_number, dt)

int	rd, current_ras, ras_number
pointer	dt
int	nras_skip, i
int	cy_read_header()
errchk	cy_skip_image

begin
        nras_skip = ras_number - current_ras
        for (i=1; i <= nras_skip; i=i+1) {
	    if (cy_read_header (rd, dt) == EOF) {
		call printf ("Cannot position DUMPF tape beyond EOF\n")
		return (EOF)
	    }
            call cy_skip_image (rd, dt)
	    current_ras = current_ras + 1
        }
	return (OK)
end
