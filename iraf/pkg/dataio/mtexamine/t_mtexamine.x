# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include <printf.h>
include <mach.h>

define	MAX_RANGES	100
define	LEN_LINE	80
define	TAPE_BYTE	8
define	TWO_TO_EIGHT	256
define	FIELD_INDEX	5
define	NFORMATS	5


# MTEXAMINE -- Examine one or more magtape files, counting the number and size
# of the records in a file, and the number of files on the tape.

procedure t_mtexamine()

int	nfiles, file_number, ndumps, nrecords
int	file_range[2*MAX_RANGES+1], rec_range[2*MAX_RANGES+1]
pointer	sp, tape_name, tape_file, file_list, rec_list

bool	clgetb()
char	clgetc()
int	fstati(), mtfile(), mtneedfileno(), decode_ranges(), get_next_number()
int	mt_examine(), mt_get_format(), clgeti(), btoi()
include "mtexamine.com"

begin
	# Allocate working space.
	call smark (sp)
	call salloc (tape_name, SZ_FNAME, TY_CHAR)
	call salloc (tape_file, SZ_FNAME, TY_CHAR)
	call salloc (file_list, SZ_LINE, TY_CHAR)
	call salloc (rec_list,  SZ_LINE, TY_CHAR)

	# Flush STDOUT on a newline only if output is not redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

        # Get input file(s).
        call clgstr ("tape_file", Memc[tape_file], SZ_FNAME)
        if (mtfile (Memc[tape_file]) == NO)
	    call strcpy ("1", Memc[file_list], SZ_LINE)
	else if (mtneedfileno (Memc[tape_file]) == NO)
	    call strcpy ("1", Memc[file_list], SZ_LINE)
        else
	    call clgstr ("file_list", Memc[file_list], SZ_LINE)
        if (decode_ranges (Memc[file_list],file_range,MAX_RANGES,nfiles) == ERR)
	    call error (0, "Illegal file number list.")

        # Get dump parameters
        dump_records = btoi (clgetb ("dump_records"))
        if (dump_records == YES) {
	    call clgstr ("rec_list", Memc[rec_list], SZ_LINE)
	    if (decode_ranges (Memc[rec_list], rec_range, MAX_RANGES,
		ndumps) == ERR)
	        call error (0, "Illegal record list.")
	    byteswap = btoi (clgetb ("swapbytes"))
	    byte_chunk = clgeti ("byte_chunk")
	    if (byte_chunk < 1 || byte_chunk > (SZ_LONG * SZB_CHAR))
		call error (0, "Illegal byte chunk size.")
	    output_format = mt_get_format (clgetc ("output_format"))
	    if (output_format == ERR)
		call error (0, "Illegal format.")
	    if (byte_chunk != 1 && output_format == FMT_CHAR)
		call error (0, "Cannot output integers as chars.")
        }

        # Loop over files
	file_number = 0
        while (get_next_number (file_range, file_number) != EOF) {

	    if (mtfile (Memc[tape_file]) == YES &&
		mtneedfileno (Memc[tape_file]) == YES)
		call mtfname (Memc[tape_file], file_number, Memc[tape_name],
		    SZ_FNAME)
	    else
	        call strcpy (Memc[tape_file], Memc[tape_name], SZ_FNAME)

	    iferr {
	        nrecords = mt_examine (Memc[tape_name], rec_range)
	    } then {
		call eprintf ("Error reading file: %s\n")
		    call pargstr (Memc[tape_name])
		call erract (EA_WARN)
		break
	    } else if (nrecords == 0) {
	        call printf ("Tape at EOT\n")
		break
	    }

        }

	call sfree (sp)
end


# MT_EXAMINE -- Procedure to examine a tape file. If dump_record is
# no mtexamine gives a summary of the record structure of the file,
# otherwise the specified records are dumped.

int procedure mt_examine (tape_file, dump_range)

char	tape_file[ARB]		# input file name
int	dump_range[ARB]		# range of records to be dumped

pointer	sp, inbuf, pchar, junk
int	in, bufsize, totrecords, nrecords, totbytes, last_recsize, nbadrec
int	stat, rec_number, next_dump, recsize, nelems, vals_per_line, field_len
long	maxval, max_plusint, twice_max_plusint

int	mtopen(), fstati(), get_next_number(), read(), gltoc()
errchk	mtopen, malloc, read, mfree, close
include "mtexamine.com"

begin
	call smark (sp)
	call salloc (junk, SZ_FNAME, TY_CHAR)

	in = mtopen (tape_file, READ_ONLY, 0)
	bufsize = fstati (in, F_BUFSIZE)
	call salloc (pchar, bufsize, TY_CHAR)

	call printf ("File %s:\n")
	    call pargstr (tape_file)

	totrecords = 0
	nrecords = 0
	totbytes = 0
	nbadrec  = 0
	last_recsize = 0

	# Prepare formatting parameters for dumping records.
	if (dump_records == YES) {
	    call salloc (inbuf, bufsize * SZB_CHAR, TY_LONG)
	    rec_number = 0
	    next_dump = get_next_number (dump_range, rec_number)
	    maxval = 2 ** (byte_chunk * TAPE_BYTE - 1) - 1
	    field_len = gltoc (maxval, Memc[junk], SZ_FNAME, TAPE_BYTE) + 1
	    vals_per_line = (LEN_LINE - FIELD_INDEX) / (field_len + 1)
	    if (output_format == FMT_DECIMAL && byte_chunk > 1 &&
	    		byte_chunk < (SZ_LONG * SZB_CHAR)) {
	        max_plusint = maxval + 1
	        twice_max_plusint = 2 * max_plusint
	    }
	}

	# Loop through the records.
	repeat {
	    iferr (stat = read (in, Memc[pchar], bufsize)) {
		call fseti (in, F_VALIDATE, last_recsize / SZB_CHAR)
		nbadrec = nbadrec + 1
	        call printf ("\tRead error on record: %d\n")
		    call pargi (totrecords + 1)
		stat = read (in, Memc[pchar], bufsize)
	    }
	    if (stat == EOF)
		break

	    recsize = fstati (in, F_SZBBLK)
	    if (dump_records == NO) {
	        if (nrecords == 0) {
		    nrecords = 1
		    last_recsize = recsize
		} else if (recsize == last_recsize) {
		    nrecords = nrecords + 1
		} else {
		    call printf ("\t%d %d-byte records\n")
		        call pargi (nrecords)
		        call pargi (last_recsize)
		    nrecords = 1
		    last_recsize = recsize
		}
	    } else if (next_dump != EOF && rec_number == totrecords + 1) {
	        call printf ("    Record %d,")
		    call pargi (totrecords + 1)
		call printf (" %d bytes,")
		    call pargi (recsize)
		nelems = recsize / byte_chunk
		call printf (" %d elements")
		    call pargi (nelems)
		call mt_bytupkl (Memc[pchar], Meml[inbuf], recsize, byte_chunk,
		    byteswap)
		call mt_dump (Meml[inbuf], nelems, field_len, vals_per_line,
		    max_plusint, twice_max_plusint)
		next_dump = get_next_number (dump_range, rec_number)
	    }

	    totrecords = totrecords + 1
	    totbytes = totbytes + recsize
	 }

	if (nrecords > 0 && dump_records == NO) {
	    call printf ("\t%d %d-byte records\n")
	        call pargi (nrecords)
	        call pargi (last_recsize)
 	}

	# Print total number of records and bytes
	if (dump_records == YES) {
	    call printf ("    Total %d records, %d bytes\n")
	        call pargi (totrecords)
	        call pargi (totbytes)
	} else { 
	    call printf ("\tTotal %d records, %d bytes")
	        call pargi (totrecords)
	        call pargi (totbytes)
	    if (nbadrec > 0) {
		call printf (" [%d bad records]")
		    call pargi (nbadrec)
	    }
	    call printf ("\n")
	}

	call close (in)

	call sfree (sp)
	return (totrecords)
end


# MT_DUMP -- Procedure to format and dump a tape record in chars, shorts or
# longs in char, decimal, octal, unsigned decimal or hexadecimal format.

procedure mt_dump (buffer, nelems, field_len, vals_per_line, max_plusint,
	twice_max_plusint)

int	nelems, field_len, vals_per_line
long	buffer[ARB], max_plusint, twice_max_plusint

int	i, nchars
char	ch, outstr[SZ_FNAME]
int	ctocc()
include "mtexamine.com"

begin
	for (i = 1; i <= nelems; i = i + 1) {
	    if (mod (i, vals_per_line) == 1) {
		call printf ("\n%*d:")
		    call pargi (FIELD_INDEX)
		    call pargi (i)
	    }
	    if (output_format == FMT_CHAR) {
		ch = buffer[i]
		nchars = ctocc (ch, outstr, SZ_FNAME)
		call printf ("%*s")
		    call pargi (field_len)
		    call pargstr (outstr)
	    } else {
		if (output_format == FMT_DECIMAL && byte_chunk > 1
			        && byte_chunk < (SZ_LONG * SZB_CHAR))
		    call mt_sign_convert (buffer[i], 1, max_plusint,
		    		    twice_max_plusint)
		call printf ("%**")
		    call pargi (field_len)
		    call pargc (output_format)
		    call pargl (buffer[i])
	    }
	}

	call printf ("\n")
end


# MT_GET_FORMAT -- Procedure to return the appropriate output format.

int procedure mt_get_format (c)

char	c
int	i, format_code[NFORMATS]
int	stridx()
string	formats "cdoxu"
data	format_code /FMT_CHAR, FMT_DECIMAL, FMT_OCTAL, FMT_HEX, FMT_UNSIGNED/

begin
	i = stridx (c, formats)
	if ( i == 0)
	    return (ERR)
	else
	    return (format_code[i])
end


# MT_BYTUPKL -- Procedure to unpack an  array in chunks byte_chunk bytes long
# into a long array with optional byteswapping.

procedure mt_bytupkl (a, b, nbytes, byte_chunk, byteswap)

char	a[ARB]		# input buffer
long	b[ARB]		# output array
int	nbytes		# number of bytes
int	byte_chunk	# number of bytes to be formatted, swapped etc.
int	byteswap	# swap bytes

int	op, i, j, rem
long	sum

begin
	op = 1

	# Unpack unsigned bytes into a long integer array
	call achtbl (a, b, nbytes)

	# Flip bytes if necessary
	if (byteswap == YES && byte_chunk > 1) {
	    for (i = 1; i <= nbytes - byte_chunk + 1; i = i + byte_chunk)
		call mt_aflipl (b[i], byte_chunk)
	}

	# Convert the bytes into unsigned integers
	for (i = 1; i <= nbytes - byte_chunk + 1; i = i + byte_chunk) {
	    sum = 0
	    for (j = 1; j <= byte_chunk; j = j + 1) {
		sum = sum + TWO_TO_EIGHT ** (byte_chunk - j) *
			b[i + j - 1]
	    }
	    b[op] = sum
	    op = op + 1
	}

	# Convert remaining bytes
	rem = nbytes -  i + 1
	if (rem > 0) {
	    if (byteswap == YES && byte_chunk > 1)
	        call mt_aflipl (b[i], rem)
	    sum = 0
	    for (j = 1; j <= rem; j = j + 1)
	        sum = sum + TWO_TO_EIGHT ** (rem - j) *
			b[i + j - 1]
	    b[op] = sum
	}
end


# MT_AFLIPL -- Procedure to flip a long integer array in place.

procedure mt_aflipl (buf, npix)

long	buf[npix]	# array to be flipped
int	npix		# number of elements in array

int	n_total, n_half, i, j

begin
	n_half = npix / 2
	n_total = npix + 1
	for (i = 1; i <= n_half; i = i + 1) {
	    j = buf[i]
	    buf[i] = buf[n_total - i]
	    buf[n_total - i] = j
	}
end


# MT_SIGN_CONVERT -- Procedure to convert unsigned long integers in the range
# 0 to twice_max_plusint - 1 to integers in the range  - max_plusint
# to max_plusint - 1.

procedure mt_sign_convert (b, nelems, max_plusint, twice_max_plusint)

long	b[nelems]		# array of long integers to be converted
int	nelems			# number of elements in the array
long	max_plusint		# 0 <= b[i] <= max_plusint - 1
long	twice_max_plusint	# twice max_plusint

int	i

begin
	for (i = 1; i <= nelems; i = i + 1) {
	    if (b[i] >= max_plusint)
		b[i] = b[i] - twice_max_plusint
	}
end
