include	<error.h>
include	<mach.h>
include	"idsmtn.h"

# UNPK_VN_ID -- Unpack an ID string from an array of FORTH ascii characters,
# one 7-bit character per byte.  The first byte contains the character
# count for the string.  

procedure unpk_vn_id (varian, offset, output_string)

int	varian[ARB]		# Array with one byte per int
int	offset			# Word offset to first character to be unpacked
char	output_string[SZ_IDS_ID]     # Output array - one character per element

pointer	sp, id
int	nchars_id

begin
	call smark (sp)
	nchars_id = min (varian[offset], SZ_IDS_ID-1)
	call salloc (id, nchars_id, TY_CHAR)

	call achtic (varian[offset+1], Memc[id], nchars_id)
	call strcpy (Memc[id], output_string, nchars_id)

	call sfree (sp)
end


# VN_RRAW -- Read Varian long (32-bit) integers from a packed bit array.
# Raw pixels are written as Varian long integers.  Each  pixel is 
# 32-bits with bit 1 least significant, bit 16 unused and bit 32 the
# sign bit.  The bits are extracted and reassembled to form a real array of 
# IDS pixels, one pixel per array element.

procedure vn_rraw (varian, offset, pixels, nwords)

int	varian[ARB]		# Pointer to array of packed IDS record
int	offset			# Word offset to first word to unpack
real	pixels[nwords]		# Output array of unpacked IDS pixels
int	nwords			# Number of values to unpack

int	ip, op, bytes[4], int_value

include	"lut.com"

begin
	ip = offset
	for (op = 1; op <= nwords; op = op + 1) {

	    call amovi (varian[ip], bytes, 4)

	    if (bytes[1] < 127)
	        int_value = bytes[4] + (bytes[3] * (2 ** 8)) + (bytes[2] * 
	            (2 ** 15)) + (bytes[1] * (2 ** 23))
	    else {
		bytes[1] = neg_lut8[bytes[1]] * (2 ** 23)
		bytes[2] = neg_lut8[bytes[2]] * (2 ** 15)
		bytes[3] = neg_lut7[bytes[3]] * (2 ** 8)
		bytes[4] = neg_lut8[bytes[4]]
		int_value = -1 * (bytes[1] + bytes[2] + bytes[3] + bytes[4] + 1)
	    }

	    pixels[op] = real (int_value)
	    ip = ip + 4
	}
end


# VN_RRED -- Read 32-bit floating point pixels from a packed bit array.  
# The values are written in special (Jan Schwitters) 2 word Varian floating 
# point.  Reduced pixels are written in this format.

procedure vn_rred (varian, offset, pixels, nwords)

int	varian[ARB]		# Array of packed varian values
int	offset			# Word offset to first value to unpack
real	pixels[nwords]		# Output array of unpacked values
int	nwords			# Number of values to unpack

int	ip, op, mantissa, exp, bytes[4]

include	"lut.com"
include	"powersof2.com"

begin
	ip = offset

	do op = 1, nwords {

	    call amovi (varian[ip], bytes, 4)

	    if (mod (bytes[1], 2) == 0)
	        mantissa = bytes[4] + (bytes[3] * (2**8)) + (bytes[2] * (2**15))
	    else {
		bytes[4] = neg_lut8[bytes[4]]
		bytes[3] = neg_lut7[bytes[3]] * (2 ** 8)
		bytes[2] = neg_lut8[bytes[2]] * (2 ** 15)
		mantissa = -1 * (bytes[4] + bytes[3] + bytes[2] + 1)
	    }

	    # Divide out mantissa sign bit
	    exp = bytes[1]/2
	    if (bytes[1] > 127)
		exp = -1 * (neg_lut6[exp] + 1)

	    # Reconstruct the floating point number as a SPP real.  Powers of
	    # two are stored in the tbl[] array where 2 ** n = tbl[n + 129].
	    # The mantissa is divided by 2 ** 23 to move the binary point
	    # above bit 23.

	    exp = exp + 129 - 23

    	    if (exp <= 0)
	        pixels[op] = 0.0

            else if (exp > 255) 
	        pixels[op] = MAX_REAL

            else if (exp > 0 && exp <= 255)
	        pixels[op] = real (mantissa) * tbl [exp]

	    # Increment the input pointer for the next word to be unpacked
	    ip = ip + 4
	}
end
