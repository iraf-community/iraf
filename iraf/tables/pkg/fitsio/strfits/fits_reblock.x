include <fset.h>
include <mach.h>

# B.Simon	14-Mar-97	modified from reb_reblock

# FITS_REBLOCK -- Copy and reblock input fits file to output file

procedure fits_reblock (in_fname, out_fname)

char	in_fname[ARB]		# i: input file name
char	out_fname[ARB]		# i: output file name
#--

int	in, out, sz_charsin, szb_inrecord, sz_charsout, szb_outrecord
int	rem_in, rem_out, ip, op, sz_inblock, bytes_read, mov_nbytes, nchars
long	offset
pointer	inbuf, outbuf

int	mtfile(), mtopen(), read(), fstati(), open(), awaitb()
errchk	open, mtopen, read, awriteb, awaitb, close, mfree, malloc, flush

begin
	# Open input and output files
	if (mtfile (in_fname) == NO) {
	    in = open (in_fname, READ_ONLY, BINARY_FILE)
	} else {
	    in = mtopen (in_fname, READ_ONLY, 0)
	}

	if (mtfile (out_fname) == NO) {
	    out = open (out_fname, NEW_FILE, BINARY_FILE)
	} else {
	    out = mtopen (out_fname, WRITE_ONLY, 0)
	}

	# Initialize block and record sizes
	# and allocate space for input and output buffers
	sz_charsin = fstati (in, F_BUFSIZE)
	szb_inrecord = sz_charsin * SZB_CHAR
	call malloc (inbuf, sz_charsin, TY_CHAR)

	sz_charsout = fstati (out, F_BUFSIZE)
	szb_outrecord = sz_charsout * SZB_CHAR
	call malloc (outbuf, sz_charsout, TY_CHAR)

	# Intialize the record remainder counters
	rem_in = szb_inrecord
	rem_out = szb_outrecord

	# Initialize input and output buffer pointers
	# Set of the offset in output file for asyncrhronous i/o
	ip = 1
	op = 1
	offset = 1
	sz_inblock = fstati (in, F_SZBBLK)

	# Loop over the input blocks.
	repeat {

	    # Read a block and update block counter.
	    nchars = read (in, Memc[inbuf], sz_charsin)
	    if (nchars == EOF)
		break

	    bytes_read = nchars * SZB_CHAR
	    if (mod (sz_inblock, SZB_CHAR) != 0)
		bytes_read = bytes_read - mod (sz_inblock, SZB_CHAR)

	    repeat {

		# Calculate the number of bytes to be moved.
		mov_nbytes = min (bytes_read - ip + 1,
				  rem_in, rem_out, szb_outrecord - op + 1)
		call bytmov (Memc[inbuf], ip, Memc[outbuf], op, mov_nbytes)

		# Update the remainders
		rem_in = rem_in - mov_nbytes
		if (rem_in == 0)
		    rem_in = szb_inrecord
		rem_out = rem_out - mov_nbytes
		if (rem_out == 0)
		    rem_out = szb_outrecord

		# Update pointers
		ip = ip + mov_nbytes
		op = op + mov_nbytes

		# If the output buffer is exhausted, output block of data.
		if (op > szb_outrecord) {
		    call awriteb (out, Memc[outbuf], szb_outrecord, offset)
		    offset = offset + awaitb (out)
		    op = 1
		}

	    } until (ip > bytes_read)

	    # reset the input pointer
	    ip = 1
	} 

	# Output remainder of data
	if (op > 1) {
	    call awriteb (out, Memc[outbuf], op - 1, offset)
	    offset = offset + awaitb (out)
	}

	call mfree (inbuf, TY_CHAR)
	call mfree (outbuf, TY_CHAR)
	call close (in)
	call close (out)
end
