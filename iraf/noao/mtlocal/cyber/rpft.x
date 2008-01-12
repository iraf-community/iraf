include	<mach.h>
include "cyber.h"

.help read_cyber
.nf ________________________________________________________________________
READ_CYBER -- Read binary chars from a file, ignoring short "noise" records.
Data is read in chunks from the file buffer into the output buffer.  The
file buffer is refilled as necessary by calling READ; the output buffer is
supplied by the calling procedure.  Cyber noise records (48 bits)
are not transferred to the output buffer and so are ignored.  READ_CYBER_INIT
must be called to initialize variables for CYBER_READ.  Variables marking the
current position and top of the buffer are initialized.
.endhelp ___________________________________________________________________

int procedure read_cyber (rd, out_buffer, maxch)

int	buf_pos, buf_top
int	rd, maxch
char	out_buffer[ARB]
char	block_buf[SZ_TAPE_BUFFER]
int	nchars, chunk_size, nchars_read
int	read(), read_cyber_init()

begin
	for (nchars = 0; nchars < maxch; nchars = nchars + chunk_size) {
	    # See if it is necessary to transfer more data from the binary file
	    # to the file buffer.  This will be necessary when all data from the
	    # file buffer has been moved to the output buffer.

	    if (buf_pos >= buf_top) {
	        # Read the next non-noise record into block_buf; reset buf_pos
    	        repeat {
		    nchars_read = read (rd, block_buf, SZ_TAPE_BUFFER)
	        } until (nchars_read > NCHARS_NOISE || nchars_read == EOF)
		buf_pos = 1
		buf_top = nchars_read
	    }

	    # The number of chars to output is the smaller of the number of
	    # characters requested or the number of characters left in the 
	    # buffer

	    if (nchars_read == EOF)
		break
	    else
	        chunk_size = min (maxch - nchars, buf_top - buf_pos + 1)
    
	    # Move data to output array, increment buffer offset
	    call amovc (block_buf[buf_pos], out_buffer[nchars+1], chunk_size)
	    buf_pos = buf_pos + chunk_size
	}

	if (nchars == 0)
	    return (EOF)
	else
	    return (nchars)

entry	read_cyber_init ()

	buf_pos = 1
	buf_top = 0
	return (OK)
end


.help order_cyber_bits
.nf __________________________________________________________________________
ORDER_CYBER_BITS -- Convert a raw data array from a 60-bit Cyber computer into
an SPP bit-array.  The output SPP bit-array is a bit-packed array of 60-bit
Cyber words, i.e., bits 1-60 are word 1, bits 61-120 are word 2, and so on.
The least significant Cyber bit is bit 1 in each output word and the most
significant bit is bit 60.  [MACHDEP].

When the Cyber outputs an array of 60 bit Cyber words it moves a stream of
bits into output bytes, filling however many bytes as necessary to output a
given number of Cyber words.  The most significant bits are output first,
i.e., bit 60 of the first word is moved to bit 8 of the first output byte,
bit 59 is moved to bit 7 of the first output byte, and so on.  If effect the
Cyber byte flips each 60-bit word.

To deal with Cyber words as an ordered bit stream we must reorder the bytes
and bits so that the least significant bits are first.  This function is
performed by the primitives CYBOOW and CYBOEW (order odd/even Cyber word)
for individual 60-bit Cyber words.  A portable (and less efficient) version
of order_cyber_bits is available which does not use these primitives.
.endhelp _____________________________________________________________________


procedure order_cyber_bits (raw_cyber, first_cyber_word, bit_array,
			    ncyber_words)

char	raw_cyber[ARB]		# raw Cyber array (e.g. from tape)
int	first_cyber_word	# first 60-bit Cyber word to be unpacked
char	bit_array[ARB]		# output bit-array
int	ncyber_words		# number of Cyber words to unpack

bool	odd_word
int	word, inbit, outbit

begin
	odd_word = (mod (first_cyber_word, 2) == 1)
	inbit = (first_cyber_word - 1) * NBITS_CYBER_WORD + 1
	outbit = 1

	do word = 1, ncyber_words {
	    # Call odd or even primitive to reorder bits and move 60-bit
	    # ordered Cyber word to the output array.

	    if (odd_word) {
		call cyboow (raw_cyber, inbit, bit_array, outbit)
		odd_word = false
	    } else {
		call cyboew (raw_cyber, inbit, bit_array, outbit)
		odd_word = true
	    }

	    inbit  = inbit  + NBITS_CYBER_WORD
	    outbit = outbit + NBITS_CYBER_WORD
	}
end


# The portable version of order_cyber_bits follows.
#.help order_cyber_bits
#.nf __________________________________________________________________________
#ORDER_CYBER_BITS -- Convert a raw data array from a 60-bit Cyber computer into
#an SPP bit-array.  The output SPP bit-array is a bit-packed array of 60-bit
#Cyber words, i.e., bits 1-60 are word 1, bits 61-120 are word 2, and so on.
#The least significant Cyber bit is bit 1 in each output word and the most
#significant bit is bit 60.  [MACHDEP].
#
#The byte stream from the Cyber contains bits 53-60 of the first word in the
#first byte, bits 45-52 in the second byte, and so on (most significant bytes
#first).  In essence we swap the order of the 7 8-bit bytes and the 4-bit half
#byte in each 60 bit word.  The bits in each byte are in the correct order.
#
#Each successive pair of Cyber words fits into 15 bytes.  Byte 8 contains the
#last 4 bits of word 1 in the most signficant half of the byte and the first
#4 bits of word 2 in the first half of the byte.  In each 60 bit word we must
#move bit segments (bytes or half bytes) as follows (for the VAX):
#
#Odd words (from N*60 bit-offset):
#      [from]   [to]  [nbits]
#	 1	53	8
#	 9	45	8
#	17	37	8
#	25	29	8
#	33	21	8
#	41	13	8
#	49	 5	8
#	61	 1	4
#
#Even words (from N*60 bit-offset):
#      [from]   [to]  [nbits]
#	-3	57	4
#	 5	49	8
#	13	41	8
#	21	33	8
#	29	25	8
#	37	17	8
#	45	 9	8
#	53	 1	8
#.endhelp _____________________________________________________________________
#
#define	NBITS_PER_WORD	60
#define	NSEGMENTS	8
#
#
#procedure order_cyber_bits (raw_cyber, first_cyber_word, bit_array,
#			    ncyber_words)
#
#char	raw_cyber[ARB]		# raw Cyber array (e.g. from tape)
#int	first_cyber_word	# first 60-bit Cyber word to be unpacked
#char	bit_array[ARB]		# output bit-array
#int	ncyber_words		# number of Cyber words to unpack
#
#int	word, inword, inbit, outbit, temp, i
#int	o_from[NSEGMENTS], o_to[NSEGMENTS], o_nbits[NSEGMENTS]
#int	e_from[NSEGMENTS], e_to[NSEGMENTS], e_nbits[NSEGMENTS]
#int	bitupk()
#
#data	o_from  / 1, 9,17,25,33,41,49,61/		# odd words
#data	o_to    /53,45,37,29,21,13, 5, 1/
#data	o_nbits / 8, 8, 8, 8, 8, 8, 8, 4/
#data	e_from  /-3, 5,13,21,29,37,45,53/		# even words
#data	e_to    /57,49,41,33,25,17, 9, 1/
#data	e_nbits / 4, 8, 8, 8, 8, 8, 8, 8/
#
#begin
#	do word = 1, ncyber_words {
#	    inword = first_cyber_word + word - 1
#	    inbit  = (inword - 1) * NBITS_PER_WORD
#	    outbit = (  word - 1) * NBITS_PER_WORD
#
#	    # Move bits to the output bit array.  Segment list used depends
#	    # on whether the word is an odd or even word.  This code will work
#	    # even if the caller only wishes to order a single word.
#
#	    if (mod (inword,2) == 1) {
#		do i = 1, NSEGMENTS {
#		    temp = bitupk (raw_cyber, inbit + o_from[i], o_nbits[i])
#		    call bitpak (temp, bit_array, outbit + o_to[i], o_nbits[i])
#		}
#	    } else {
#		do i = 1, NSEGMENTS {
#		    temp = bitupk (raw_cyber, inbit + e_from[i], e_nbits[i])
#		    call bitpak (temp, bit_array, outbit + e_to[i], e_nbits[i])
#		}
#	    }
#	}
#end
