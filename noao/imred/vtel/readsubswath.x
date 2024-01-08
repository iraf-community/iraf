include <mach.h>
include <fset.h>
include	"vt.h"

define	SZ_VTRECFD	5120		# length, in chars, of full disk recs

# READSUBSWATH -- Read data from file whose logical unit is inputfd.
# Swap the bytes in each data word.

procedure readsubswath (inputfd, selfbuf, databuf, buflength, bp)

int	inputfd			# input file discriptor
int	buflength		# length of data buffer
bool	selfbuf			# self buffering flag
short	databuf[buflength]	# data buffer
pointer	bp			# buffer pointer structure pointer

int	num, bleft, last_recsize
int	read()
errchk	read

begin
	# If we are doing our own buffering, keep track of the number
	# of records in each file, else let mtio do it.

	last_recsize = 0
	if (selfbuf) {		# do our own buffering

	    # If there is enough data still in the buffer, just copy data
	    # to the output buffer and move the pointer, otherwise, read
	    # the next tape record.

	    if ((VT_BUFBOT(bp) - VT_BP(bp)) >= buflength) {
		# Copy the data into the data buffer, move the pointer.
		call amovs (Mems[VT_BP(bp)], databuf, buflength)
		VT_BP(bp) = VT_BP(bp) + buflength

	    } else {
		# Copy leftover data from the bottom of the input buffer
		# into the top of the input buffer, reset the flags.

		bleft = VT_BUFBOT(bp) - VT_BP(bp)
		call amovs (Mems[VT_BP(bp)], Mems[VT_BUFP(bp)], bleft)
		VT_BP(bp) = VT_BUFP(bp) + bleft

		# Read in another tape record.
		# Check the number of chars read.  If this number is EOF or
		# too short, error.  If it is too long, truncate to correct
		# length.  This is done because some data tapes are written
		# in a weird way and have some noise chars tacked on the end
		# of each tape record.

	        iferr (num = read (inputfd, Mems[VT_BP(bp)],
		    10000*SZB_SHORT/SZB_CHAR)) {
		    call fseti (inputfd, F_VALIDATE,
			SZ_VTRECFD*SZB_SHORT/SZB_CHAR)
		    call printf ("Error reading subswath.\n")
		    num = read (inputfd, Mems[VT_BP(bp)],
		        SZ_VTRECFD*SZB_SHORT/SZB_CHAR)
	        }
		if (num == EOF)
		    call error (0, "EOF encountered on tape read")
	        else if (num < SZ_VTRECFD*SZB_SHORT/SZB_CHAR)
	            call error (0, "error on tape read, record too short")
		else if (num >= SZ_VTRECFD*SZB_SHORT/SZB_CHAR &&
		    num < (SZ_VTRECFD+300)*SZB_SHORT/SZB_CHAR)
		    num = SZ_VTRECFD*SZB_SHORT/SZB_CHAR 
		else
	            call error (0, "error on tape read, record too long")

		# Update the pointers, move data into the data buffer.
		VT_BUFBOT(bp) = VT_BP(bp) + num
		call amovs (Mems[VT_BP(bp)], databuf, buflength)
		VT_BP(bp) = VT_BP(bp) + buflength
	    }
	} else {		# Let the mtio do the buffering.
	    iferr (num = read (inputfd, databuf,
		buflength*SZB_SHORT/SZB_CHAR)) {
		call fseti (inputfd, F_VALIDATE,
		    last_recsize*SZB_SHORT/SZB_CHAR)
		call printf ("Error on tape read.\n")
		num = read (inputfd, databuf, buflength*SZB_SHORT/SZB_CHAR)
	    }
	    last_recsize = num
	    if (num < buflength*SZB_SHORT/SZB_CHAR)
	        call error (0, "eof encountered when reading subswath")
	}

	if (BYTE_SWAP2 == YES)
	    call bswap2 (databuf, 1, databuf, 1, buflength * SZB_SHORT)
end
