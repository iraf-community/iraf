include <error.h>
include <mach.h>
include "vt.h"

define	MAX_RANGES	100

# MSCAN -- Read vacuum telescope area scans.

procedure t_mscan()

char	input[SZ_FNAME]		# input file template
char	files[SZ_LINE]		# file list to process
bool	verbose			# verbose flag
bool	makeimage		# flag to make an image
bool	bright			# flag to make a brightness image
bool	velocity		# flag to make a velocity image
bool	select			# flag to make a select image
bool	brief			# flag to make brief file names

char	tapename[SZ_FNAME]
char	diskfile[SZ_LINE]
int	filerange[2 * MAX_RANGES + 1]
int	nfiles, filenumber, recsize, listin

bool	clgetb()
int	decode_ranges(), get_next_number(), mscan()
int	fntopnb(), clgfil(), mtneedfileno()
int	mtfile()
errchk	mscan

begin
	# CLIO for parameters.
	verbose = clgetb ("verbose")
	makeimage = clgetb ("makeimage")
	bright = clgetb ("bright")
	velocity = clgetb ("velocity")
	select = clgetb ("select")
	brief = clgetb ("brief")

	# If the user hasn't asked for ANY of the images, just return.
	if (!bright && !velocity && !select)
	    return

        # Get input file(s).
        call clgstr ("input", input, SZ_FNAME)
        if (mtfile (input) == NO) {

	    # This is not a tape file, expand as a list template.
	    listin = fntopnb (input, 0)
	    filenumber = 1

	    while (clgfil (listin, diskfile, SZ_FNAME) != EOF) {
		iferr (recsize =  mscan (diskfile, filenumber, brief,
		    verbose, makeimage, select, bright, velocity)) {
		    call eprintf ("Error reading file %s\n")
		    	call pargstr (diskfile)
	    	}
	        if (recsize == EOF) {
		    call printf ("Tape at EOT\n")
		    break
	        }
		filenumber = filenumber + 1
	    }
	    call clpcls (listin)

	} else if (mtneedfileno(input) == NO) {
	    
	    # This is a tape file and the user specified which file.
	    iferr (recsize = mscan (input, 0, brief, verbose,
	    	    makeimage, select, bright, velocity)) {
		call eprintf ("Error reading file %s\n")
		    call pargstr (input)
	    }
        } else {

	    # This is a tape file or files and the user did not specify
	    # which file.
	    call clgstr ("files", files, SZ_LINE)

            if (decode_ranges (files, filerange, MAX_RANGES, nfiles) == ERR)
	        call error (0, "Illegal file number list.")

	    if (verbose)
	        call printf ("\n")

            # Loop over files.
	    filenumber = 0
            while (get_next_number (filerange, filenumber) != EOF) {

	        # Assemble the appropriate tape file name.
		call mtfname (input, filenumber, tapename, SZ_FNAME)

	        # Read this file.
	        iferr {
	            recsize = mscan (tapename, filenumber, brief,
			verbose, makeimage, select, bright, velocity)
	        } then {
		    call eprintf ("Error reading file: %s\n")
		        call pargstr (tapename)
		    call erract (EA_WARN)
		    next
	        }
	        if (recsize == EOF) {
		    call printf ("Tape at EOT\n")
		    break
	        }

            }  # End while.
	}
end


# MSCAN -- Read in the next sector scan file from tape.  First read the file
# header to determine what type scan it is and then call the appropriate
# subroutime for that type of scan.

int procedure mscan (input, filenumber, brief, verbose, makeimage, select, 
		    bright, velocity)

char	input[SZ_FNAME]		# input file name
int	filenumber		# file number
bool	brief			# brief disk file names?
bool	verbose			# print header info?
bool	makeimage		# make images?
bool	select			# make select image?
bool	bright			# make bright image?
bool	velocity		# make velocity image?

int	in
int	lastrecsize
int	recsize
bool	selfbuf
pointer	sp, hbuf, hs

int	mtopen()
int	readheader()
define	nexit_ 10
errchk	mtopen, close, readheader

begin
	call smark (sp)
	call salloc (hbuf, SZ_VTHDR, TY_SHORT)
	call salloc (hs, VT_LENHSTRUCT, TY_STRUCT)

	in = mtopen (input, READ_ONLY, 0)

	call printf ("File %s:  ")
	    call pargstr (input)

	lastrecsize = 0

	# First, read the header file
	selfbuf = FALSE
	recsize = readheader (in, hbuf, selfbuf)
	if (recsize == EOF)
	    return (recsize)

	# Decode the header and jump if '!makeimage'.
	lastrecsize = recsize
	call decodeheader (hbuf, hs, verbose)
	if (verbose) {
	    call printf ("\n")
	    call flush (STDOUT)
	}
	if (!makeimage)
	    goto nexit_

	# Call the appropriate area scan reader.
	switch (VT_HOBSTYPE(hs)) {
	case 1:
	    call readss1 (in, filenumber, brief, select, bright, velocity, hs)
	case 2:
	    call readss2 (in, filenumber, brief, select, bright, velocity, hs)
	case 3:
	    call readss3 (in, filenumber, brief, select, bright, velocity, hs)
	case 4:
	    call readss4 (in, filenumber, brief, select, bright, velocity, hs)
	case 0:
	    call printf ("Observation type zero encountered, image skipped.\n")
	default:
	    call error (0, "unknown observation type, image skipped")
	} # End of switch case.

nexit_
	call sfree (sp)
	call close (in)
	return (recsize)
end
