include <fset.h>
include <error.h>
include <ctype.h>
include <mach.h>
include "reblock.h"

define	MAX_RANGES	100
define	SZ_PADCHAR	10
define  SZ_EXTN		32

procedure t_fitscopy ()

char	infiles[SZ_FNAME]	# list of input files
char	file_list[SZ_LINE]	# list of tape file numbers
char	outfiles[SZ_FNAME]	# list of output files
bool	verbose			# print messages ?

char	in_fname[SZ_FNAME], out_fname[SZ_FNAME]
char    root[SZ_FNAME], extn[SZ_EXTN]
long	offset, file_number, ifile_number, nfiles, range[2 * MAX_RANGES + 1]
int	ip, status, onfiles, junk
pointer	list, olist
long	l_val

bool	clgetb()
int	fstati(), mtfile(), mtneedfileno(), decode_ranges()
int	imtgetim(), fnextn(), imtlen(), reb_doit()
long	get_next_number(), clgetl()
pointer imtopen()
include "reblock.com"

begin
	nskip = 0
	ncopy = MAX_LONG
	byteswap = NO
	wordswap = NO
	verbose = clgetb ("verbose")

	# Flush on a newline if the output is not redirected.
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Get the input and output file(s).
	call clgstr ("infiles", infiles, SZ_FNAME)
	call clgstr ("outfiles", outfiles, SZ_FNAME)


	outtape = NO
	intape = NO
	if (mtfile (infiles) == YES) { 
	   intape = YES
	   ip = 1
	   while (infiles[ip] != EOS && infiles[ip] != '[')
		 ip = ip + 1
           infiles[ip] = EOS
 	}
	if (mtfile (outfiles) == YES)
	    outtape = YES

	if (intape == YES)
	   call clgstr ("file_list", file_list, SZ_LINE)
	else
	    call strcpy ("1", file_list, SZ_LINE)

	# Decode the tape file number list.
	if (decode_ranges (file_list, range, MAX_RANGES, nfiles) == ERR)
	    call error (0, "Illegal file number list.")
	
	file_number = 1
	if (outtape == YES) {
	   if (mtneedfileno (outfiles) == YES) {
	      if (! clgetb ("newtape")) {
		 l_val = EOT
		 call mtfname (outfiles, l_val, out_fname, SZ_FNAME)
		 file_number = EOT
	      } else {
		 l_val = 1
		 call mtfname (outfiles, l_val, out_fname, SZ_FNAME)
	      }
	   } else {
	      call strcpy (outfiles, out_fname, SZ_FNAME)
	   }
	}


	# Get the block and record sizes.
        szb_outblock = clgetl ("blocking_factor")*2880      #FITS block factor
	if (outtape == NO)
	   szb_outblock = INDEFL

	szb_inrecord = INDEFL
	szb_outrecord = INDEFL

	# Get the pad and trim parameters.
	pad_block = NO
	pad_record = NO
	trim_record = NO

	# Tape to disk always requires reblocking.
	reblock = YES

	if (intape == YES && outtape == NO) {
	   # LOOP THROUGH THE TAPE AND CREATE
	   # DISK FILES.
	   # Allow for byte or word swapping
	   olist = imtopen (outfiles)
	   onfiles = imtlen (olist)
	   if (onfiles > 1) {           # We have an list template for
	      		 	        # the output files.
	      if (onfiles != nfiles) 
		 call error(13, "Number of input and output files don't match")
	   }
	   offset = clgetl ("offset")
	   call zfnbrk (outfiles, ip, junk)
	   call strcpy (outfiles, root, junk-1)
	   junk = fnextn (outfiles, extn, SZ_FNAME)

	   file_number = 0
	   while (get_next_number (range, file_number) != EOF) {

	       call mtfname (infiles, file_number, in_fname, SZ_FNAME)

	       if (nfiles > 1) {
		  if (onfiles == 1) {
	             call sprintf (outfiles, SZ_FNAME, "%s%03d")
		         call pargstr (root)
		         call pargl (file_number + offset)
	             call iki_mkfname(outfiles, extn, out_fname, SZ_FNAME)
		  } else
		     status = imtgetim (olist, out_fname, SZ_FNAME)
	       } else
	           call strcpy (outfiles, out_fname, SZ_FNAME)

	       status = reb_doit (in_fname, out_fname, verbose)
	       if (status == ERR) {
		  call error (0,"Error in input or output files\n")
		  break
	       } else if (status == EOF) {
	          call deletefg (out_fname, YES, YES)
		  break
	       }
	   }
	} else if (intape == NO && outtape == YES) {
	   # LOOP THROUGH THE INPUT IMAGES AND PUT ONTO
	   # TAPE.
	   list = imtopen(infiles)
	   while (imtgetim(list, in_fname, SZ_FNAME) != EOF) {

	      if (file_number == EOT) {
		 l_val = EOT
	         call mtfname (out_fname, l_val, out_fname, SZ_FNAME)
	      } else {
	         call mtfname (out_fname, file_number, out_fname, SZ_FNAME)
	         file_number = file_number + 1
	      }

	       status = reb_doit (in_fname, out_fname, verbose)
	       if (status == ERR) {
		  call error (0, "Error in input or output files\n")
		  break
	       } else if (status == EOF)
		  break
	   }
	   call imtclose(list)
	} else if (intape == YES && outtape == YES) {
	   # LOOP THROUGH THE INPUT TAPE AND WRITE
	   # ONTO OUTPUT TAPE.
	   #

	   ifile_number = 0
	   while (get_next_number (range, ifile_number) != EOF) {

	       call mtfname (infiles, ifile_number, in_fname, SZ_FNAME)

	       if (file_number == EOT) {
		  l_val = EOT
	          call mtfname (out_fname, l_val, out_fname, SZ_FNAME)
	       } else {
	          call mtfname (out_fname, file_number, out_fname, SZ_FNAME)
	          file_number = file_number + 1
	       }

	       status = reb_doit (in_fname, out_fname, verbose)
	       if (status == ERR) {
		  call error (0,"Error in input or output tape\n")
		  break
	       } else if (status == EOF)
		  break

	   }

	} else
	   call error (12,"No disk to disk transfer allowed")
	

end

int procedure reb_doit (in_fname, out_fname, verbose)
char 	in_fname[SZ_FNAME]
char 	out_fname[SZ_FNAME]
bool	verbose
long	outparam[LEN_OUTPARAM]

include "reblock.com"
begin

	iferr {
	   call reb_reblock_file (in_fname, out_fname, outparam)

	} then {
	   call flush (STDOUT)
	   return (ERR)
	} else if (BLKS_RD(outparam) == 0) {
	   call printf ("EOF encountered\n")
	   return (EOF)
	}


	if (verbose) {
	    call printf ("File: %s -> %s: \n")
		call pargstr (in_fname)
		call pargstr (out_fname)
	}
	
	return (OK)
end
# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <fset.h>
include <mach.h>
include "reblock.h"

# REB_REBLOCK_FILE -- Copy and optionally reblock files.

procedure reb_reblock_file (in_fname, out_fname, outparam)

char	in_fname[ARB]		# input file name
char	out_fname[ARB]		# output file name
long	outparam[ARB]		# output parameters

size_t	sz_val, c_1
long	l_val
char	padchar
int	in, out
long	rem_in, rem_out, nchars, rec_count, ntrim, offset, i
size_t	sz_charsin, sz_charsout, mov_nbytes, ip, op, first_byte, bytes_read
pointer	inbuf, outbuf

int	mtopen(), open()
long	read(), fstatl(), reb_skipover(), reb_roundup(), lmod()
errchk	open, mtopen, read, awriteb, awaitb, close, mfree, malloc, flush
errchk	reb_write_block, reb_pad_block, reb_pad_record, reb_skipover
include "reblock.com"

begin
	c_1 = 1

	# Open input and output files
	sz_val = 0
	in = mtopen (in_fname, READ_ONLY, sz_val)
	if (outtape == NO) {
	    out = open (out_fname, NEW_FILE, BINARY_FILE)
	} else {
	    sz_val = 0
	    out = mtopen (out_fname, WRITE_ONLY, sz_val)
	}

	# Allocate space for input buffer.
	sz_charsin = fstatl (in, F_BUFSIZE)
	call malloc (inbuf, sz_charsin, TY_CHAR)

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

	# Initialize reblocking parameters.
	if (reblock == YES) {

	    # Initialize block and record sizes
	    if (IS_INDEFL(szb_inrecord))
	        szb_inrecord = sz_charsin * SZB_CHAR
	    if (IS_INDEFL(szb_outblock))
	        szb_outblock = fstatl (out, F_BUFSIZE) * SZB_CHAR
	    if (IS_INDEFL(szb_outrecord))
	        szb_outrecord = szb_outblock

	    # Set pad character
	    if (pad_record == YES || pad_block == YES) {
		padchar = char (padvalue)
		call chrpak (padchar, c_1, padchar, c_1, c_1)
	    }

	    # Allocate space for the output buffer
	    sz_charsout = reb_roundup (szb_outblock, SZB_CHAR) / SZB_CHAR
	    call malloc (outbuf, sz_charsout, TY_CHAR)

	    # Intialize the record remainder counters
	    rem_in = szb_inrecord
	    rem_out = szb_outrecord

	    # Initialize input and output buffer pointers
	    ip = 1
	    op = 1
	}

	# Set up the record counter
	rec_count = 0

	# Set of the offset in output file for asyncrhronous i/o
	offset = 1

	# Loop over the input blocks.
	repeat {

	    # Read a block and update block counter.
	    nchars = read (in, Memc[inbuf], sz_charsin)
	    if (nchars == EOF)
		break
	    bytes_read = nchars * SZB_CHAR
	    l_val = SZB_CHAR
	    if (lmod (fstatl (in, F_SZBBLK), l_val) != 0)
		bytes_read = bytes_read - lmod (fstatl (in, F_SZBBLK), l_val)
	    BLKS_RD(outparam) = BLKS_RD(outparam) + 1

	    # Align to first byte.
	    if (rec_count == 0 && first_byte > 1) {
		bytes_read = bytes_read - first_byte + 1
		call bytmov (Memc[inbuf], first_byte, Memc[inbuf], c_1,
			     bytes_read)
	    }

	    # Binary copy
	    if (reblock == NO) {

		RECS_RD(outparam) = BLKS_RD(outparam)
		call reb_write_block  (out, Memc[inbuf], bytes_read, offset,
		    byteswap, wordswap)
		BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
		RECS_WRT(outparam) = BLKS_WRT(outparam)

	    # Reblock.
	    } else {

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

		    # Update pointers
		    ip = ip + mov_nbytes
		    op = op + mov_nbytes

		    # Pad records
		    if (pad_record == YES && rem_in == szb_inrecord) {

			# Pad records.
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
			sz_val = szb_outblock
		        call reb_write_block (out, Memc[outbuf], sz_val,
		            offset, byteswap, wordswap)
		        BLKS_WRT(outparam) = BLKS_WRT(outparam) + 1
		        op = 1
		    }

		    # Trim records.
		    if (trim_record == YES && rem_out == szb_outrecord) {

			# Trim a record.
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

		    # Simply count the records.
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

		# reset the input pointer
	        ip = 1
	    }

	    # Reset the record counter.
	    if (intape == YES)
		rec_count = BLKS_RD(outparam)
	    else
		rec_count = RECS_RD(outparam)

	} until (rec_count >= ncopy)

	# Output remainder of data
	if (reblock == YES) {

	    # Pad last record if short
	    if (pad_record == YES) {
	        if (rem_in < szb_inrecord)
		    RECS_RD(outparam) = RECS_RD(outparam) + 1
		if (rem_out < szb_outrecord)
		    RECS_WRT(outparam) = RECS_WRT(outparam) + 1
		while (rem_out < szb_outrecord) {
		    call reb_pad_record (Memc[outbuf], op, rem_out,
		        szb_outblock, szb_outrecord, padchar)
		    if (op > szb_outblock) {
			sz_val = szb_outblock
			call reb_write_block (out, Memc[outbuf], sz_val,
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
	if (reblock == YES)
	    call mfree (outbuf, TY_CHAR)
	call close (in)
	call close (out)
end


# REB_PAD_RECORD -- Procedure for padding records

procedure reb_pad_record (buffer, op, rem_out, szb_outblock, szb_outrecord,
	padchar)

char	buffer[ARB]
size_t	op
long	rem_out
long	szb_outblock
long	szb_outrecord
char	padchar

long	i, junk
size_t	c_1

begin
	c_1 = 1
	junk = rem_out
	for (i = 1; i <= junk && op <= szb_outblock; i = i + 1) {
	    call bytmov (padchar, c_1, buffer, op, c_1)
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
size_t	op		# pointer to first element for padding
long	rem_out		# number of remaining bytes to be padded in a record
long	outparam[ARB]	# output parameters, number of records, blocks written
long	szb_outblock	# size in bytes of output block
long	szb_outrecord	# size in bytes of an output record
char	padchar		# character used for padding

long	i, junk
size_t	c_1

begin
	c_1 = 1
	junk = szb_outblock - op + 1
	for (i = 1; i <= junk; i = i + 1) {
	    call bytmov (padchar, c_1, buffer, op, c_1)
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
size_t	nbytes		# number of bytes of data
long	offset		# offset in chars in output file for writing
int	byteswap	# swap every other byte before output
int	wordswap	# swap every other word before output

size_t	c_1
long	awaitb()
errchk	awriteb, awaitb

begin
	c_1 = 1
	if (byteswap == YES)
	    call bswap2 (buffer, c_1, buffer, c_1, nbytes)
	if (wordswap == YES)
	    call bswap4 (buffer, c_1, buffer, c_1, nbytes)
	call awriteb (fd, buffer, nbytes, offset)
	offset = offset + awaitb (fd)
end


# REB_SKIPOVER -- Procedure to find the first byte containing data given the
# input block size and the number of input blocks to be skipped.

long procedure reb_skipover (fd, szb_inblock, nskip)

int	fd		# file descriptor
long	szb_inblock	# size of an input block
long	nskip		# number of blocks to skip

long	l_val
long	first_byte
long	szb_skip, szb_physkip, skip_diff, sz_charoff, loff
long	fstatl(), reb_roundup()
errchk	fstatl, seek

begin
        szb_skip = szb_inblock * nskip
        szb_physkip = reb_roundup (szb_skip, SZB_CHAR)
        skip_diff = szb_physkip - szb_skip

        if (skip_diff == 0) {
	    sz_charoff = (szb_physkip / SZB_CHAR) + 1
	    first_byte = 1
	} else {
	    sz_charoff = (szb_physkip / SZB_CHAR) - 1
	    first_byte = szb_skip - (SZB_CHAR * sz_charoff) + 1
	}

	loff = sz_charoff

        if (loff > fstatl (fd, F_FILESIZE)) {
	    l_val = EOF
	    call seek (fd, l_val)
	    return (EOF)
	} else {
            call seek (fd, loff)
	    return (first_byte)
	}
end


# REB_ROUNDUP -- Procedure to round a number to the next highest number
# divisible by  base.

long procedure reb_roundup (number, base)

long	number		# number to be rounded upwards
int	base		# base for rounding

long	value, l_val
long	lmod()

begin
	l_val = base
	if (lmod(number, l_val) == 0)
	    return (number)
	else {
	    value = (number/base + 1) * base
	    return (value)
	}
end
