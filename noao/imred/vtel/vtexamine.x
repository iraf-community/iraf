include <error.h>
include <fset.h>
include <printf.h>
include <mach.h>
include "vt.h"

define	MAX_RANGES	100

# VTEXAMINE -- Examine a vacuum telescope tape.  Decode and print the
# header and tell the user info about number and length of records
# on the tape.

procedure t_vtexamine()

char	input[SZ_FNAME]		# input template
char	files[SZ_LINE]		# which files to examine
bool	headers			# print headers?

char	tapename[SZ_FNAME]
int	filerange[2 * MAX_RANGES + 1]
int	nfiles, filenumber, nrecords

bool	clgetb()
int	decode_ranges(), get_next_number()
int	vtexamine(), mtfile(), mtneedfileno()
errchk	vtexamine

begin
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Find out if user wants to see header info.
	headers = clgetb ("headers")
	
        # Get input file(s)
        call clgstr ("input", input, SZ_FNAME)
	if (mtfile (input) == NO || mtneedfileno (input) == NO)
	    call strcpy ("1", files, SZ_LINE)
        else
	    call clgstr ("files", files, SZ_LINE)

        if (decode_ranges (files, filerange, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list.")
	call printf ("\n")

        # Loop over files.
	filenumber = 0
        while (get_next_number (filerange, filenumber) != EOF) {

	    # Assemble the appropriate tape file name.
	    call strcpy (input, tapename, SZ_FNAME)
	    if (mtfile(input) == YES && mtneedfileno (input) == YES)
		call mtfname (input, filenumber, tapename, SZ_FNAME)

	    iferr {
	        nrecords = vtexamine (tapename, headers)
	    } then {
		call eprintf ("Error reading file: %s\n")
		    call pargstr (tapename)
		call erract (EA_WARN)
		next
	    } else if (nrecords == 0) {
	        call printf ("Tape at EOT\n")
		break
	    }

        }  # End while.
end


# VTEXAMINE -- examine a tape (or disk) file.  Report about size and
# number of records and, if requested, decode and print the header
# information.

int procedure vtexamine (input, headers)

char	input[ARB]		# input file name
bool	headers

int	in, bufsize, totrecords
int	nrecords, totbytes, lastrecsize
int	recsize
bool	trufls
pointer	hs, sp
pointer	pchar, hpchar

int	mtopen(), fstati(), get_next_record()
errchk	mtopen, close, get_next_record

begin
	call smark (sp)
	call salloc (hs, VT_LENHSTRUCT, TY_STRUCT)

	in = mtopen (input, READ_ONLY, 0)
	bufsize = fstati (in, F_BUFSIZE)

	call malloc (pchar, bufsize, TY_CHAR)
	call malloc (hpchar, bufsize, TY_SHORT)

	call printf ("File %s:  ")
	    call pargstr (input)

	totrecords = 0
	nrecords = 0
	totbytes = 0
	lastrecsize = 0


	# First read the header file.
	recsize = get_next_record (in, Memc[pchar], bufsize, recsize,
	    SZ_VTHDR * SZB_SHORT/SZB_CHAR)
	if (recsize == EOF)
	    return (totrecords)
	call amovs (Memc[pchar], Mems[hpchar], SZ_VTHDR * SZB_SHORT/SZB_CHAR)

	nrecords = nrecords + 1
	totrecords = totrecords + 1
	totbytes = totbytes + recsize
	lastrecsize = recsize
	trufls = TRUE
	if (headers)
	    call decodeheader (hpchar, hs, trufls)
	call printf ("\n")

	# Loop through the rest of the records.
	while (get_next_record (in, Memc[pchar], bufsize, recsize,
	    lastrecsize) != EOF) {

	    if (recsize == lastrecsize)
		nrecords = nrecords + 1
	    else {
		call printf ("\t      %d %d-byte records\n")
		    call pargi (nrecords)
		    call pargi (lastrecsize)
		nrecords = 1
		lastrecsize = recsize
	    }

	    totrecords = totrecords + 1
	    totbytes = totbytes + recsize

	}  # End while.

	if (nrecords > 0 ) {
	    call printf ("\t      %d %d-byte records\n")
		call pargi (nrecords)
		call pargi (lastrecsize)
	}

	# Print total number of records and bytes.
	call printf ("\t      Total %d records, %d bytes\n")
	    call pargi (totrecords)
	    call pargi (totbytes)

	call mfree (pchar, TY_CHAR)
	call mfree (hpchar, TY_SHORT)
	call sfree (sp)
	call close (in)

	return (totrecords)
end


# GET_NEXT_RECORD -- Read the next record from tape (or disk) and,
# if an error is found, patch up the data as best we can and use it.
# Also, tell the user about the error.

int procedure get_next_record(fd, buffer, bufsize, recsize, lastbufsize)

int	bufsize
char	buffer[bufsize]
int	recsize, lastbufsize
pointer	fd

int	read(), fstati()
bool	eofflag
errchk	read

begin
	eofflag = false
	iferr {
	    if (read (fd, buffer, bufsize) == EOF)
		eofflag = true
	    recsize = fstati (fd, F_SZBBLK)
	} then {
	    call fseti (fd, F_VALIDATE, lastbufsize)
	    recsize = read (fd, buffer, bufsize)
	    recsize = fstati (fd, F_SZBBLK)
	}
	if (BYTE_SWAP2 == YES)
	    call bswap2 (buffer, 1, buffer, 1, SZ_VTHDR*SZB_SHORT)
	if (eofflag)
	    return (EOF)
	else
	    return (recsize)
end
