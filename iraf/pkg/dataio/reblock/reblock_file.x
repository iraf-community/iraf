# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include <mach.h>
include "reblock.h"

# REB_REBLOCK_FILE -- Copy and optionally reblock files.

procedure reb_reblock_file (in_fname, out_fname, outparam)

char	in_fname[ARB]		# input file name
char	out_fname[ARB]		# output file name
int	outparam[ARB]		# output parameters

char	padchar
int	in, out, sz_charsin, sz_charsout, mov_nbytes, rem_in, rem_out
int	bytes_read, ip, op, i, first_byte, nchars, rec_count, ntrim
long	offset
pointer	inbuf, outbuf

int	mtopen(), reb_roundup(), read(), reb_skipover(), fstati(), open()
errchk	open, mtopen, read, awriteb, awaitb, close, mfree, malloc, flush
errchk	reb_write_block, reb_pad_block, reb_pad_record, reb_skipover
include "reblock.com"

begin
	# Open input and output files
	in = mtopen (in_fname, READ_ONLY, 0)
	out = NULL

	# Allocate space for input buffer.
	sz_charsin = fstati (in, F_BUFSIZE)
	call malloc (inbuf, sz_charsin, TY_CHAR)
	outbuf = NULL

	# Skip over n input blocks (tape) or records (disk).
	first_byte = 1
	if (intape == YES) {
	    for (i=1; i <= nskip; i = i + 1) {
		nchars = read (in, Memc[inbuf], sz_charsin)
		if (nchars == EOF)
		    call error (1,"Skipped past EOF on input.")
	    }
	} else {
    	    first_byte = reb_skipover (in, szb_inrecord, nskip)
	    if (first_byte == EOF)
		call error (2, "Skipped past EOF on input.")
	}

	# Initialize the input and output block and record counters
	BLKS_RD(outparam) = 0
	BLKS_WRT(outparam) = 0
	RECS_RD(outparam) = 0
	RECS_WRT(outparam) = 0

	# Initalize the record counter.
	rec_count = 0

	# Set of the offset in output file for asyncrhronous i/o.
	offset = 1

	# Loop over the input blocks.
	repeat {

	    # Read a block and update block counter.
	    nchars = read (in, Memc[inbuf], sz_charsin)
	    if (nchars == EOF)
		break
	    bytes_read = nchars * SZB_CHAR
	    if (mod (fstati (in, F_SZBBLK), SZB_CHAR) != 0)
		bytes_read = bytes_read - mod (fstati (in, F_SZBBLK), SZB_CHAR)
	    BLKS_RD(outparam) = BLKS_RD(outparam) + 1

	    # Align to first byte.
	    if (rec_count == 0 && first_byte > 1) {
		bytes_read = bytes_read - first_byte + 1
		call bytmov (Memc[inbuf],first_byte, Memc[inbuf],1, bytes_read)
	    }

	    # Open the output file. This has been moved from the beginning 
	    # of the routine to avoid a magtape problem.
	    # driver problem.
	    if (BLKS_RD(outparam) == 1) {
		if (outtape == NO)
	    	    out = open (out_fname, NEW_FILE, BINARY_FILE)
		else
	    	    out = mtopen (out_fname, WRITE_ONLY, 0)
	    }

	    # Binary copy.
	    if (reblock == NO) {

		RECS_RD(outparam) = BLKS_RD(outparam)
		call reb_write_block  (out, Memc[inbuf], bytes_read, offset,
		    byteswap, wordswap)
		BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
		RECS_WRT(outparam) = BLKS_WRT(outparam)

	    # Reblock.
	    } else {

		# Initialize reblocking parameters after first read.
		if (BLKS_RD(outparam) == 1) {

	            # Initialize block and record sizes
	            if (IS_INDEFI(szb_inrecord))
	                szb_inrecord = sz_charsin * SZB_CHAR
	            if (IS_INDEFI(szb_outblock))
	                szb_outblock = fstati (out, F_BUFSIZE) * SZB_CHAR
	            if (IS_INDEFI(szb_outrecord))
	                szb_outrecord = szb_outblock

	            # Set pad character.
	            if (pad_record == YES || pad_block == YES) {
		        padchar = char (padvalue)
		        call chrpak (padchar, 1, padchar, 1, 1)
	            }

	            # Allocate space for the output buffer.
	            sz_charsout = reb_roundup (szb_outblock, SZB_CHAR) /
		        SZB_CHAR
	            call malloc (outbuf, sz_charsout, TY_CHAR)

	            # Intialize the record remainder counters
	            rem_in = szb_inrecord
	            rem_out = szb_outrecord

	            # Initialize input and output buffer pointers
	            ip = 1
	            op = 1
		}

		# Loop over the input buffer.
	        repeat {

		    # Calculate the number of bytes to be moved.
		    mov_nbytes = min (bytes_read - ip + 1,
			rem_in, rem_out, szb_outblock - op + 1)
		    call bytmov (Memc[inbuf], ip, Memc[outbuf], op, mov_nbytes)

		    # Update the remainders
		    rem_in = rem_in - mov_nbytes
		    if (rem_in == 0)
		        rem_in = szb_inrecord
		    rem_out = rem_out - mov_nbytes
		    if (rem_out == 0)
		        rem_out = szb_outrecord

		    # Update the input and output buffer pointers.
		    ip = ip + mov_nbytes
		    op = op + mov_nbytes

		    # Pad records.
		    if (pad_record == YES && rem_in == szb_inrecord) {

			# Do the padding.
			if (mov_nbytes != 0) {
		            RECS_RD(outparam) = RECS_RD(outparam) + 1
		            call reb_pad_record (Memc[outbuf], op, rem_out,
		                szb_outblock, szb_outrecord, padchar)
			} else if (rem_out < szb_outrecord)
		            call reb_pad_record (Memc[outbuf], op, rem_out,
		                szb_outblock, szb_outrecord, padchar)

		        # Increment the output record counter.
		        if (rem_out ==  szb_outrecord) 
		            RECS_WRT(outparam) = RECS_WRT(outparam) + 1
			 else if (rem_out < szb_outrecord)
			    rem_in = 0
		    }

		    # If the output buffer is exhausted, output block of data.
		    if (op > szb_outblock) {
		        call reb_write_block (out, Memc[outbuf], szb_outblock,
		            offset, byteswap, wordswap)
		        BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
		        op = 1
		    }

		    # Trim records.
		    if (trim_record == YES && rem_out == szb_outrecord) {

			# Do the trimming.
			if (mov_nbytes != 0)
			    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
			ntrim = min (rem_in, bytes_read - ip + 1)
			ip = ip + ntrim
			rem_in = rem_in - ntrim
			if (rem_in == 0)
			    rem_in = szb_inrecord

			# Increment the record counter.
			if (rem_in == szb_inrecord)
			    RECS_RD(outparam) = RECS_RD(outparam) + 1
			else if (rem_in < szb_inrecord)
			    rem_out = 0
		    }

		    # Count the records.
		    if (pad_record == NO && trim_record == NO) {
			if (szb_inrecord == sz_charsin * SZB_CHAR)
			    RECS_RD(outparam) = BLKS_RD(outparam)
			else if (rem_in == szb_inrecord)
			    RECS_RD(outparam) = RECS_RD(outparam) + 1
			if (rem_out == szb_outrecord)
			    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
		    }

		    # Quit if ncopy records has been reached.
		    if (intape == NO && RECS_RD(outparam) == ncopy)
			break

	        } until (ip > bytes_read)

		# Reset the input buffer pointer
	        ip = 1
	    }

	    # Update the record counter.
	    if (intape == YES)
		rec_count = BLKS_RD(outparam)
	    else
		rec_count = RECS_RD(outparam)

	} until (rec_count >= ncopy)

	# Output remainder of data
	if (reblock == YES) {

	    # Pad last record if short.
	    if (pad_record == YES) {
	        if (rem_in < szb_inrecord)
		    RECS_RD(outparam) = RECS_RD(outparam) + 1
		if (rem_out < szb_outrecord)
		    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
		while (rem_out < szb_outrecord) {
		    call reb_pad_record (Memc[outbuf], op, rem_out,
		        szb_outblock, szb_outrecord, padchar)
		    if (op > szb_outblock) {
			call reb_write_block (out, Memc[outbuf], szb_outblock,
			    offset, byteswap, wordswap)
			BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
			op = 1
		    }
		}
	    }

	    # Pad last block if short.
	    if (pad_block == YES && op > 1)
		call reb_pad_block (Memc[outbuf], op, rem_out, outparam,
		    szb_outblock, szb_outrecord, padchar)

	    # Write last block
	    if (op > 1) {
		call reb_write_block (out, Memc[outbuf], op - 1, offset,
		    byteswap, wordswap)
	        op = 1
	        BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
		if (pad_record == YES && rem_out < szb_outrecord)
		    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
		else if (rem_out < szb_outrecord)
		    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
	    }

	}

	call mfree (inbuf, TY_CHAR)
	if (outbuf != NULL)
	    call mfree (outbuf, TY_CHAR)
	call close (in)
	if (out != NULL)
	    call close (out)
end


# REB_PAD_RECORD -- Procedure for padding records.

procedure reb_pad_record (buffer, op, rem_out, szb_outblock, szb_outrecord,
	padchar)

char	buffer[ARB], padchar
int	szb_outblock, szb_outrecord, op, rem_out
int	i, junk

begin
	junk = rem_out
	for (i = 1; i <= junk && op <= szb_outblock; i = i + 1) {
	    call bytmov (padchar, 1, buffer, op, 1)
	    op = op + 1
	    rem_out = rem_out - 1
	}

	if (rem_out == 0)
	    rem_out = szb_outrecord
end


# REB_PAD_BLOCK -- Procedure to pad the last block so that all output blocks
# will have the same size.

procedure reb_pad_block (buffer, op, rem_out, outparam, szb_outblock,
	szb_outrecord, padchar)

char	buffer[ARB]	# data to be padded
int	op		# pointer to first element for padding
int	rem_out		# number of remaining bytes to be padded in a record
int	outparam[ARB]	# output parameters, number of records, blocks written
int	szb_outblock	# size in bytes of output block
int	szb_outrecord	# size in bytes of an output record
char	padchar		# character used for padding

int	i, junk

begin
	junk = szb_outblock - op + 1
	for (i = 1; i <= junk; i = i + 1) {
	    call bytmov (padchar, 1, buffer, op, 1)
	    op = op + 1
	    rem_out = rem_out - 1
	    if (rem_out == 0) {
		rem_out = szb_outrecord
		RECS_WRT(outparam) = RECS_WRT(outparam) + 1
	    }
	}
end


# REB_WRITE_BLOCK -- Procedure to write blocks  using the asynchronous read
# and write functions in file i/o. Writing must occur on block boundaries.

procedure reb_write_block (fd, buffer, nbytes, offset, byteswap, wordswap)

int	fd		# output file descriptor
char	buffer[ARB]	# data to be output
int	nbytes		# number of bytes of data
long	offset		# offset in chars in output file for writing
int	byteswap	# swap every other byte before output
int	wordswap	# swap every other word before output

int	nbread
int	awaitb()
errchk	awriteb, awaitb

begin
	if (byteswap == YES)
	    call bswap2 (buffer, 1, buffer, 1, nbytes)
	if (wordswap == YES)
	    call bswap4 (buffer, 1, buffer, 1, nbytes)
	call awriteb (fd, buffer, nbytes, offset)
	nbread = awaitb (fd)
	if (nbread == ERR)
	    call error (3, "Error writing block data")
	else
	    offset = offset + nbread
end


# REB_SKIPOVER -- Procedure to find the first byte containing data given the
# input block size and the number of input blocks to be skipped.

int procedure reb_skipover (fd, szb_inblock, nskip)

int	fd		# file descriptor
int	szb_inblock	# size of an input block
int	nskip		# number of blocks to skip

int	first_byte
long	szb_skip, szb_physkip, skip_diff, sz_charoff, loff
long	fstatl()
int	reb_roundup()
errchk	fstatl, seek

begin
        szb_skip = long (szb_inblock) * long (nskip)
        szb_physkip = reb_roundup (szb_skip, SZB_CHAR)
        skip_diff = szb_physkip - szb_skip

        if (skip_diff == 0) {
	    sz_charoff = (szb_physkip / SZB_CHAR) + 1
	    first_byte = 1
	} else {
	    sz_charoff = (szb_physkip / SZB_CHAR) - 1
	    first_byte = int (szb_skip - (SZB_CHAR * sz_charoff) + 1)
	}

	loff = long (sz_charoff)

        if (loff > fstatl (fd, F_FILESIZE)) {
	    call seek (fd, EOF)
	    return (EOF)
	} else {
            call seek (fd, loff)
	    return (first_byte)
	}
end


# REB_ROUNDUP -- Procedure to round a number to the next highest number
# divisible by  base.

int procedure reb_roundup (number, base)

int	number		# number to be rounded upwards
int	base		# base for rounding

int	value

begin
	if (mod(number, base) == 0)
	    return (number)
	else {
	    value = (number/base + 1) * base
	    return (value)
	}
end
