include	<mach.h>
include	"cyber.h"

# READ_DUMPF -- Read data from an array of cyber words, passing the
# requested number of cyber words to the output buffer.  
# The word array contains control words and "Zero length PRU's" as well
# as data.  The control words are read and used to properly
# interpret the PRU that follows.  Each read by GET_CYBER_WORDS begins
# on a control word boundry.  It is expected that read_dumpf returns an
# integral number of PRU's; if not, the data remaining in the PRU is discarded,
# as the buffer pointer must be positioned to the control word preceeding
# a PRU upon entry to read_dumpf.  The number of cyber words read is 
# returned by read_dumpf.  Pointer ip is in units of cyber words; there
# are two array elements per cyber word.

int procedure read_dumpf (rd, out, max_cyb_words)

int	rd, max_cyb_words
int	out[ARB], pru_buf[NINT_CYBER_WRD * LEN_CYBER_READ]
int	nwords, nwords_read, word_count, ip
int	get_cyber_words(), read_dumpf_init(), bitupk()

begin
	if (mod (max_cyb_words, LEN_PRU) != 0)
	    call error (0, "READ_DUMPF: Non-integral PRU requested")

	for (nwords=0; nwords < max_cyb_words; nwords = nwords + word_count) {
	    # If necessary, read more data into the pru buffer.  This
	    # will be necessary when all data in the pru buffer has
	    # been transferred to the output buffer.

	    if (ip >= nwords_read) {
		# Read another chunk of cyber words; reset buffer position
		nwords_read = get_cyber_words (rd, pru_buf, LEN_CYBER_READ)
		ip = 1

	        if (nwords_read == EOF)
		    break
	    }

	    # Get the number of 12-bit bytes in the next pru from control word.
	    # This number stored in lowest 9 bits of the cyber word.
	    word_count = bitupk (pru_buf[(NINT_CYBER_WRD * ip) - 1], 1, 9) /
		NBYTES_WORD
	    ip = ip + 1

	    if (word_count == 0) {
	       # Zero length PRU to be skipped over
	       ip = ip + LEN_PRU
	       next
	    }

	    if (mod (word_count * 60, NBITS_CHAR) != 0)
		call error (0, "READ_DUMPF: Impossible PRU to CHAR Conversion")
	    
	    call amovi (pru_buf[(NINT_CYBER_WRD * ip) - 1], out[(nwords * 
		NINT_CYBER_WRD) + 1], word_count * NINT_CYBER_WRD)
	    ip = ip + LEN_PRU
	}

	if (nwords == 0)
	    return (EOF)
	else
	    return (nwords)

entry	read_dumpf_init ()

	ip = 1
	nwords_read = 0
end

.help read_cyber
.nf ________________________________________________________________________
GET_CYBER_WORDS -- Read binary chars from a file, ignoring short "noise" 
records and extraneous bits inserted to fill out a byte.  The requested
number of cyber words is returned, one cyber word per two array elements.
Data is read from
the file buffer and placed in the output array by UNPACK_CYBER_WORDS.  The
file buffer is refilled as necessary by calling READ; the output array is
supplied by the calling procedure.  Cyber noise records (48 bits) and bits
used to fill out a byte are not transferred to the output array.  
Variables marking the current position and top of the buffer are initialized
by GET_CYBER_WORDS_INIT; buf pos marks the buffer position in cyber words.
The number of cyber words read is returned as the function value; the number
of output array elements filled is twice the number of cyber words read.
.endhelp ___________________________________________________________________

int procedure get_cyber_words (rd, out, max_cyb_words)

int	rd, max_cyb_words
int	out[ARB]
char	cyber_buf[SZ_TAPE_BUFFER]
int	buf_pos, ncyber_words
int	nwords, word_chunk, nchars_read
int	read(), get_cyber_words_init()

begin
	for (nwords = 0; nwords < max_cyb_words; nwords = nwords + word_chunk) {
	    # See if it is necessary to transfer more data from the binary file
	    # to the file buffer.  This will be necessary when all data from the
	    # file buffer has been transferred to the output array.

	    if (buf_pos >= ncyber_words) {
	        # Read the next non-noise record into cyber_buf; reset buf_pos
    	        repeat {
		    nchars_read = read (rd, cyber_buf, SZ_TAPE_BUFFER)
	        } until (nchars_read >= NCHARS_NOISE || nchars_read == EOF)
		buf_pos = 1
		ncyber_words = (nchars_read * NBITS_CHAR) / NBITS_CYBER_WORD

	        if (nchars_read == EOF)
		    break
	    }

	    # The number of cyber words to output is the smaller of the number 
	    # requested or the number of words left in the buffer. 

   	    word_chunk = min (max_cyb_words - nwords, ncyber_words- buf_pos + 1)
	    call unpack_cyber_words (cyber_buf, buf_pos, out, 
		(NINT_CYBER_WRD * nwords) + 1, word_chunk)
	    buf_pos = buf_pos + word_chunk
	}

	if (nwords == 0)
	    return (EOF)
	else
	    return (nwords)

entry	get_cyber_words_init ()

	buf_pos = 1
	ncyber_words = 0
	return (OK)
end



.help unpack_cyber_words
.nf __________________________________________________________________________
UNPACK_CYBER_WORDS -- Convert a raw data array from a 60-bit Cyber computer into
an SPP array containing one Cyber word in each 64-bit increment.
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


procedure unpack_cyber_words (raw_cyber, first_cyber_word, int_array,
			      first_output_element, ncyber_words)

char	raw_cyber[ARB]		# raw Cyber array (e.g. from tape)
int	first_cyber_word	# first 60-bit Cyber word to be unpacked
int	int_array[ARB]		# output unpacked array of Cyber words
int	first_output_element	# first output integer to be filled
int	ncyber_words		# number of Cyber words to unpack

bool	odd_word
int	word, inbit, outbit

begin
	odd_word = (mod (first_cyber_word, 2) == 1)
	inbit = (first_cyber_word - 1) * NBITS_CYBER_WORD + 1
	outbit = (first_output_element - 1) * NBITS_INT + 1

	do word = 1, ncyber_words {
	    # Call odd or even primitive to reorder bits and move 60-bit
	    # ordered Cyber word to the output array.

	    if (odd_word) {
		call cyboow (raw_cyber, inbit, int_array, outbit)
		odd_word = false
	    } else {
		call cyboew (raw_cyber, inbit, int_array, outbit)
		odd_word = true
	    }

	    inbit  = inbit  + NBITS_CYBER_WORD
	    outbit = outbit + NINT_CYBER_WRD * NBITS_INT
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
