# BITFIELDS -- Routines for inserting and extracting bitfields into integers.

# BITPAK -- Pack an integer into a bitfield of an array.  Set all nbits
# bits regardless of the value of the integer.

	.text
	.align	1
	.globl	_bitpak_

	# bitpak (intval, array, offset, nbits)
	.set	INTVAL,	4
	.set	ARRAY,	8
	.set	OFFSET,	12		# one-indexed bit offset
	.set	NBITS,	16

_bitpak_:
	.word	0x0

	subl3	$1, *OFFSET(ap), r1
	insv	*INTVAL(ap), r1, *NBITS(ap), *ARRAY(ap)
	ret
	.data

# BITUPK -- Unpack a bitfield from an array and return as the function
# value, an integer.  Do not sign extend.

	.text
	.align	1
	.globl	_bitupk_

	# bitupk (array, offset, nbits)
	.set	ARRAY,	4
	.set	OFFSET,	8			# one-indexed bit offset
	.set	NBITS,	12

_bitupk_:
	.word	0x0

	subl3	$1, *OFFSET(ap), r1
	extzv	r1, *NBITS(ap), *ARRAY(ap), r0
	ret
	.data
