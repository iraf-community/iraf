include	<mach.h>
include	<imhdr.h>
include	<error.h>
include "rrcopy.h"

# RC_UP_12 -- Unpack 12-bit unsigned integers from a stream of bits.  
# Each output integer word contains successive 12-bit increments 
# of the input bit stream in the least significant bit positions.
# It is assummed that the initial_bit_offset is the first bit of a 
# Cyber 60-bit word containing 5 packed 12-bit pixels, the first pixel
# in the highest 12 bits.

procedure rc_up_12 (input, initial_bit_offset, output, npix_unpk)

char	input[ARB]
int	output[npix_unpk], npix_unpk
int	initial_bit_offset, nbits, n, nn, bit_offset
int	npix_word, ncyb_words, index
int	bitupk()

begin
	nbits = 12
	npix_word = 5
	if (mod (npix_unpk, npix_word) == 0)
	    ncyb_words = (npix_unpk) / npix_word
	else
	    call error (0, "Incorrect number of pixels to be unpacked")
	index = 1

	do n = 1, ncyb_words {
	    bit_offset = initial_bit_offset + (n * 60)
	    do nn = 1, npix_word {
	        bit_offset = bit_offset - nbits
	        output[index] = bitupk (input, bit_offset, nbits)
	        if (output[index] == 7777B)
		    output[index] = BLANK
		index = index + 1
	    }
	}
end


# RC_UP_20 -- Unpack 20-bit signed integers from a stream of bits.
# Each output integer word contains sucessive 20-bit increments of the input.
# Conversion from one's complement to two's complement is performed.
# It is assummed that initial_bit_offset is the first bit of a Cyber 
# 60-bit word containing 3 packed 20-bit pixels, the first pixel in the
# highest 20 bits.

procedure rc_up_20 (input, initial_bit_offset, output, npix_unpk)

char	input[ARB]
int	output[npix_unpk], npix_unpk
int	nbits, n, index, bit_offset, initial_bit_offset
int	npix_word, ncyb_words, nn, pix_val
int	bitupk()

begin
	nbits = 20
	npix_word = 3
	if (mod (npix_unpk, npix_word) == 0)
	    ncyb_words = npix_unpk / npix_word
	else
	    call error (0, "Incorrect number of pixels to be unpacked")
	index = 1

	do n = 1, ncyb_words {
	    bit_offset = initial_bit_offset + (n * 60)
	    do nn = 1, npix_word {
	        bit_offset = bit_offset - nbits
	        pix_val = bitupk (input, bit_offset, nbits)
	        if (pix_val == 3777777B)
	   	    pix_val = BLANK
	        else if (and (pix_val, 2000000B) != 0) 
		    # negative pixel
		    pix_val = -and (3777777B, not(pix_val))
		output[index] = pix_val
		index = index + 1
	    }
	}
end


# RC_UP_30 -- unpack Cyber 30-bit floating point numbers from a stream of
# bits.  The input bit stream is unpacked in 30-bit increments into
# an integer array.  Procedure REPACK_FP is called to reconstruct the
# floating point numbers from this array.  It is assumed initial_bit_offset
# is the first bit of a Cyber 60-bit word containing 2 30-bit pixels, the
# first pixel in the higher 30 bits.

procedure rc_up_30 (input, initial_bit_offset, fp_value, npix)

char	input[ARB]
real	fp_value[npix]
pointer	int_buf, sp
int	initial_bit_offset, npix, bit_offset
int	nbits, n
int	bitupk()

begin
	# Allocate buffer space, allowing for maximum of 1 extraneous pixel
	call smark (sp)
	call salloc (int_buf, npix + 1, TY_INT) 

	nbits = 30
	bit_offset = initial_bit_offset - 60

	do n = 1, npix, 2 {
	    bit_offset = bit_offset + 90
	    Memi[int_buf + n - 1] = bitupk (input, bit_offset, 30)
	    bit_offset = bit_offset - nbits
	    Memi[int_buf + n] = bitupk (input, bit_offset, 30)
	}

	call rc_repack_fp (Memi[int_buf], fp_value, npix)
	call sfree (sp)
end


# RC_UP_60R -- Unpack Cyber 60-bit floating point numbers from a stream 
# of bits.  The 30 most significant bits from each 60-bit word are
# unpacked into an integer array.  Procedure REPACK_FP is called to
# reconstruct the floating point numbers from this array.
# An 18-bit mantissa, 11-bit exponent and a sign bit are unpacked into
# the lower 30 bits of each output word.

procedure rc_up_60r (input, initial_bit_offset, fp_value, nwords)

char	input[ARB]
real	fp_value[nwords]
int	initial_bit_offset, nwords, bit_offset
pointer	int_buf, sp
int	n, nbits_unpk, nbits
int	bitupk()

begin
	# Allocate space on stack
	call smark (sp)
	call salloc (int_buf, nwords, TY_INT)

	nbits = 60
	nbits_unpk = 30
	bit_offset = initial_bit_offset + 30
   
	do n = 1, nwords {
	    Memi[int_buf + n - 1] = bitupk (input, bit_offset, nbits_unpk)
	    bit_offset = bit_offset + 60
	}

	call rc_repack_fp (Memi[int_buf], fp_value, nwords)
	call sfree (sp)
end


# RC_UP_60I -- Unpack 60-bit integers from a stream of bits.  Each element
# of output contains only the lower 32 bits of each input word, as this
# procedure is called only for getting NROWS, NCOLS and a few other small
# positive integer values.  (A 60-bit intger is not a valid IPPS pixel type.)

procedure rc_up_60i (input, initial_bit_offset, output, nwords)

char	input[ARB]
int	output[nwords]
int	initial_bit_offset, nwords, bit_offset
int	n, nbits_unpk, nbits
int	bitupk()

begin
	nbits_unpk = NBITS_INT 
	nbits = 60
	bit_offset = initial_bit_offset

	do n = 1, nwords {
	    output[n] = bitupk (input, bit_offset, nbits_unpk)
	    bit_offset = bit_offset + 60
	}
end


# RC_UP_ID -- Unpacks ID string from input bit stream.  The IPPS ID string is 
# written in 7-bit ASCII, with eight characters per Cyber word.  The lowest 
# 4 bits of each 60-bit word is unused.  The highest 7 bits of the first Cyber 
# word in the bit stream contains the character count.

procedure rc_up_id (input, output)

char	input[SZ_HEADER]
char	output[SZ_HEADER]
int	nbits, nchar_offset, id_offset, nchars, n
int	nchars_word, ncyb_words, nn, index
int	bitupk()

begin
	nbits = 7
	nchar_offset = NBITS_CYBER_WORD - 6
	nchars = bitupk (input, nchar_offset, nbits)
	ncyb_words = (nchars + 7) / 8
	index = 1

	do n = 1, ncyb_words {
	    if (n == 1) {
		nchars_word = 7
		id_offset = nchar_offset - 7
	    } else {
		nchars_word = 8
		id_offset = (n * NBITS_CYBER_WORD) - 6
	    }
	    do nn = 1, nchars_word {
	        output[index] = bitupk (input, id_offset, nbits)
		index = index + 1
	        id_offset = id_offset - 7
	    }
	}
	output[nchars+1] = EOS
end


# RC_REPACK_FP -- returns a floating point number as the function value.
# The input to REPACK_FP is an integer containing a 30-bit Cyber floating
# point number in the least significant bits.  The exponent, mantissa
# and two bits indicating the sign are extracted and used to reassemble
# the floating point value.  Cyber blanks and overflows are returned as BLANK.

procedure rc_repack_fp (int_value, float_value, nvalues)

int	int_value[ARB], nvalues
real	float_value[nvalues]

int	i, pixel
int	exp, mantissa
real	tbl[255]
int	bitupk(), and(), not()
include	"../pow.inc"

begin
	do i=1, nvalues {	
	    pixel = int_value[i]
	    # Check for blanks
	    if (pixel == 1777000000B) {
		float_value[i] = BLANK
		next
	    }

	    # Check "bit59" and complement all bits if it is set
	    if (and (pixel, 4000000000B) != 0)  {
		pixel = not (pixel)
		mantissa = -and (pixel, 777777B)
	    } else
		mantissa = and (pixel, 777777B)
	    
	    # Extract and interpret exponent: remove Cyber bias of 2000B
	    # and convert to two's complement if negative number
	    exp = bitupk (pixel, 19, 11)
	    if (exp > 1777B) 
		# "bit58" is set, positive exponent
		exp = exp - 2000B
	    else
		# negative exponent
		exp = exp - 1777B

	    # Reconstruct the floating point value:  30 is added to the
	    # exponent because only the top 18 bits of the 48-bit mantissa
	    # were extracted;  the 129 is to register the data array index.
	    #     float_value[i] = real(mantissa) * 2 ** (exp + 30) 
	    #      (tbl[1] = 2 ** -128) ==> (2 ** n = tbl[n + 129]).

	    exp = exp + 30 + 129
	    if (exp <= 0) {
		#call eprintf (
		   #"RRCOPY_RPACK_FP: Exponent underflow in following record\n")
		float_value[i] = 0.0
	    } else if (exp > 255) {
		#call eprintf (
		   #"RRCOPY_REPACK_FP: Exponent overflow in following record\n")
		float_value[i] = MAX_REAL
	    } else if (exp > 0 && exp <= 255)
	        float_value[i] = double (mantissa) * tbl[exp]
	}
end
