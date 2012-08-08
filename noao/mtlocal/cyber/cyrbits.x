include	<mach.h>
include	<imhdr.h>
include	<error.h>
include "cyber.h"

# UNPK_12 -- Unpack 12-bit unsigned integers from an array containing 
# one sixty bit cyber word in each pair of array elements.
# Each output word contains successive 12-bit pixels from the
# input array.  
# Pixels are unpacked starting with element "first_word" of the input array.
# Each cyber 60-bit word contains 5 packed 12-bit pixels, the first pixel
# in the highest 12 bits.  The input array contains one cyber word per two
# array elements; the cyber word occupies the lower 60 bits of each pair
# of array values.

procedure unpk_12 (input, first_word, output, npix_unpk)

int	input[ARB]		#
int	first_word		#
int	output[npix_unpk]	#
int	npix_unpk		#

int	n, nn, i, offset[5], off
int	npix_word, ncyber_words, index
data 	(offset[i], i=1, 5) /15, 27, 39, 51, 63/
int	bitupk()

begin
	npix_word = 5
	if (mod (npix_unpk, npix_word) == 0)
	    ncyber_words = (npix_unpk) / npix_word
	else
	    call error (0, "Incorrect number of pixels to be unpacked")
	index = 1

	i = 1
	for (n = first_word; i <= npix_unpk; n = n + 2) {
	    do nn = 1, npix_word {
		off = (n + 1) * NBITS_INT - offset[nn]
	        output[i] = bitupk (input, off, 12)
	        if (output[i] == 7777B)
		    output[i] = BLANK
		i = i + 1
	    }
	}
end


# UNPK-20 -- Unpack 20-bit signed integers from an array containing
# one 60-bit Cyber word per pair of array elements.  Each output
# word contains successive 20-bit pixels from the input array.  Pixels
# are unpacked starting with array element "first_word".  Conversion
# from one's complement to two's complement is performed.  Each Cyber
# word contains 3 packed 20-bit pixels, the first pixel in the highest
# 20 bits.  

procedure unpk_20 (input, first_word, output, npix_unpk)

int	input[ARB]
int	output[npix_unpk], npix_unpk, first_word
int	n, i, nn, off
int	npix_word, ncyber_words, pix_val, offset[3]
data	(offset[i], i=1, 3) /23, 43, 63/
int	bitupk()

begin
	npix_word = 3
	if (mod (npix_unpk, npix_word) == 0)
	    ncyber_words = npix_unpk / npix_word
	else
	    call error (0, "Incorrect number of pixels to be unpacked")

	i = 1
	for (n = first_word; i <= npix_unpk; n = n + 2) {
	    do nn = 1, npix_word {
		off = (n + 1) * NBITS_INT - offset[nn]
	        pix_val = bitupk (input, off, 20)
	        if (pix_val == 3777777B)
	   	    pix_val = BLANK
	        else if (and (pix_val, 2000000B) != 0) 
		    # negative pixel
		    pix_val = -and (3777777B, not(pix_val))
		output[i] = pix_val
		i = i + 1
	    }
	}
end


# UNPK_60R -- Unpack Cyber 60-bit floating point numbers from an array
# containing one 60-bit word per pair of array elements.  
# The 30 most significant bits from each 60-bit word are
# unpacked and then reconstructed as a floating point number with
# with REPACK_FP.  The extracted bits include an 18-bit mantissa,
# 11-bit exponent and a sign bit.  This routine is used for getting
# the min and max values from the header;  no 60-bit IPPS pixels are
# expected.

procedure unpk_60r (input, first_word, fp_value, nwords)

int	input[ARB]
real	fp_value[nwords]
int	first_word, nwords, n, i
pointer	int_buf, sp
int	bitupk()

begin
	# Allocate space on stack
	call smark (sp)
	call salloc (int_buf, nwords, TY_INT)

	i = 1
	for (n = first_word; i <= nwords; n = n + 2) {
	    Memi[int_buf + i - 1] = bitupk (input[n], 31, 30)
	    i = i + 1
	}

	call repack_fp (Memi[int_buf], fp_value, nwords)
	call sfree (sp)
end


# UNPK_60I -- Unpack 60-bit integers from an array containing one Cyber
# word per each NINT_CYBER_WRD elements.  Each word
# of output contains only the lower 32 bits of each input word, as this
# procedure is called only for getting the reduction flags from the
# IDSFILE header.  

procedure unpk_60i (input, initial_bit_offset, output, nwords)

char	input[ARB]
int	output[nwords]
int	initial_bit_offset, nwords, bit_offset, n
int	bitupk()
errchk	bitupk

begin
	bit_offset = initial_bit_offset

	do n = 1, nwords {
	    output[n] = bitupk (input, bit_offset, NBITS_INT)
	    if (and (output[n], 2000000000B) != 0) 
		# negative value
		output[n] = -not(output[n])
	    bit_offset = bit_offset + (NINT_CYBER_WRD * NBITS_INT)
	}
end


# CONVERT_60BIT_FP -- returns a floating point number equivalent to the Cyber
# 60-bit number input.  The full 48-bit mantissa and 11-bit exponent
# is used in reconstructing the floating point value.

double procedure convert_60bit_fp (cyber_word, bit_offset)

int	cyber_word[ARB], bit_offset
int	temp1, temp2
double	float_value
int	exp, lower_mantissa, upper_mantissa, i
int	bitupk(), and(), not()
double	tbl[255]
include	"powd.inc"
errchk	bitupk

begin
	# Extract cyber word in question into temp array
	temp1 = bitupk (cyber_word, bit_offset, 30)
	temp2 = bitupk (cyber_word, bit_offset + 30, 30)

	# Check "bit59" and complement all bits if it is set
	if (bitupk (temp2, 30, 1) != 0) {
	    temp1 = not (temp1)
	    temp2 = not (temp2)
	    lower_mantissa = -and (temp1, 7777777777B)
	    upper_mantissa = -and (temp2, 777777B)
	} else {
	    lower_mantissa = temp1
	    upper_mantissa = and (temp2, 777777B)
	}

	# Extract and interpret exponent; remove Cyber bias of 2000B and
	# convert to two's complement if negative number
	exp = bitupk (temp2, 19, 11)
	if (exp > 1777B)
	    # "bit58" is set, positive exponent
	    exp = exp - 2000B
	else
	    # negative exponent
	    exp = exp - 1777B

	# Reconstruct the floating point number.  30 is added to the
	# exponent for the upper mantissa.  The 129 is to register the data
	# array matrix:  tbl[1] = 2 ** -128 ==> 2 ** n = tbl[n + 129]
	#   float_value = mantissa * 2 ** (exp + 129)
	# 
	#   float_value = (lower_mantissa) * 2 ** (exp + 129) +
	#        (upper_mantissa) * 2 ** (exp + 30 + 129)

	float_value = double (lower_mantissa) * tbl[exp + 129] +
	    double (upper_mantissa) * tbl[exp + 30 + 129]

	return (float_value)
end


# UNPK_30 -- unpack Cyber 30-bit floating point numbers from an array
# containing one 60-bit Cyber word in each pair of array elements.  Each
# 30-bit pixel is unpacked from this array; procedure REPACK_FP is called
# to reconstruct the floating point number.  Pixels are unpacked starting
# with array element "first_word".  Each Cyber word contains 2 30-bit
# pixels, the first pixel in the higher 30 bits.

procedure unpk_30 (input, first_word, fp_value, npix)

int	input[ARB]
int	first_word, npix
real	fp_value[npix]
pointer	int_buf, sp
int	n, i, off, offset[2]
int	bitupk()
data	(offset[i], i = 1, 2) /33, 63/
errchk	bitupk

begin
	# Allocate buffer space, allowing for maximum of 1 extraneous pixel
	call smark (sp)
	call salloc (int_buf, npix + 1, TY_INT) 

	i = 1
	for (n = first_word; i <= npix; n = n + 2) {
	    off = (n + 1) * NBITS_INT - offset[1]
	    Memi[int_buf + i - 1] = bitupk (input, off, 30)
	    off = (n + 1) * NBITS_INT - offset[2]
	    Memi[int_buf + i] = bitupk (input, off, 30)
	    i = i + 2
	}

	call repack_fp (Memi[int_buf], fp_value, npix)
	call sfree (sp)
end


# UNPK_ID -- Unpacks ID string from input array, which contains one Cyber
# word per two array elements.  The word_offset equals the number of Cyber
# words to skip before beginning to unpack.  If the character string
# begins in word one of "input", word_offset = 0.  The IPPS ID string
# is written in 7-bit ASCII, with eight characters per Cyber word.  The lowest 
# 4 bits of each 60-bit word is unused.  The highest 7 bits of the first Cyber 
# word contain the character count.

procedure unpk_id (input, word_offset, output)

int	input[ARB]
int	word_offset
char	output[SZ_IPPS_ID]
int	nbits, nchar_offset, id_offset, nchars, n
int	nchars_word, ncyber_words, nn, index
int	bitupk()

begin
	nbits = 7
	nchar_offset = (word_offset * NBITS_INT * NINT_CYBER_WRD) +
	    NBITS_CYBER_WORD - 6
	nchars = bitupk (input, nchar_offset, nbits)
	ncyber_words = (nchars + 6) / 7
	index = 1

	for (n = 1; n <= ncyber_words; n = n + 1) {
	    if (n == 1) {
		nchars_word = 7
		id_offset = nchar_offset - 7
	    } else {
		nchars_word = 8
		id_offset = nchar_offset + ((n-1) * NBITS_INT * NINT_CYBER_WRD)
	    }
	    do nn = 1, nchars_word {
	        output[index] = bitupk (input, id_offset, nbits)
		index = index + 1
	        id_offset = id_offset - 7
	    }
	}
	output[nchars+1] = EOS
end


# REPACK_FP -- Reconstruct a floating point number. The input to REPACK_FP
# is an array of integers containing Cyber 30-bit floating point numbers 
# in the least significant bits of each array element.  The exponent, mantissa
# and two bits indicating the sign are extracted and used to reassemble
# the floating point value.  Cyber blanks and overflows are returned as BLANK.

procedure repack_fp (int_value, float_value, nvalues)

int	int_value[ARB], nvalues
real	float_value[nvalues]

int	i, pixel
int	exp, mantissa
real	tbl[255]
int	bitupk(), and(), not()
include	"pow.inc"

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
		   #"RDUMPF_REPACK_FP: pixel with exponent underflow seen\n")
		float_value[i] = 0.0
	    } else if (exp > 255) {
		#call eprintf (
		   #"RDUMPF_REPACK_FP: pixel with exponent overflow seen\n")
		float_value[i] = MAX_REAL
	    } else if (exp > 0 && exp <= 255)
	        float_value[i] = real(mantissa) * tbl[exp]
	}
end


# DISPLAY_CODE -- returns the ascii character equivalent to the display
# code character input.  The Cyber uses the 63-character display code
# set internally, although the 64-character set is used for output.

procedure display_code (in_char, out_char)

char	in_char, out_char
char	dc[64]
int	i

data	(dc[i], i=1,8)   /072B, 101B, 102B, 103B, 104B, 105B, 106B, 107B/
data	(dc[i], i=9,16)  /110B, 111B, 112B, 113B, 114B, 115B, 116B, 117B/
data	(dc[i], i=17,24) /120B, 121B, 122B, 123B, 124B, 125B, 126B, 127B/
data	(dc[i], i=25,32) /130B, 131B, 132B, 060B, 061B, 062B, 063B, 064B/
data	(dc[i], i=33,40) /065B, 066B, 067B, 070B, 071B, 053B, 055B, 052B/
data	(dc[i], i=41,48) /057B, 050B, 051B, 044B, 075B, 040B, 054B, 056B/
data	(dc[i], i=49,56) /043B, 133B, 135B, 045B, 042B, 137B, 041B, 046B/
data	(dc[i], i=57,64) /047B, 077B, 074B, 076B, 100B, 134B, 136B, 073B/

begin
	out_char = dc[in_char + 1]
end
