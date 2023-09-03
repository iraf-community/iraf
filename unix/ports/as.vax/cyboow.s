# CYBOOW, CYBOEW -- Order the bits in an odd or even indexed 60-bit Cyber word.
# The operation may not be performed in-place.  The offsets and sizes of the
# bit segments which must be moved are as follows:
#
#	 --> Odd Words <--	        --> Even Words <--
#       [from]   [to]  [nbits]
# 	 1	53	8		-3	57	4
# 	 9	45	8		5	49	8
# 	17	37	8		13	41	8
# 	25	29	8		21	33	8
# 	33	21	8		29	25	8
# 	41	13	8		37	17	8
# 	49	 5	8		45	 9	8
# 	61	 1	4		53	 1	8
#
# Input bit-offsets must be a multiple of the Cyber word size, i.e., 1, 61,
# 121, etc.  An output word may begin at any bit-offset.

.globl	_cyboow_
.globl	_cyboew_

	.set	IN,	4
	.set	INBIT,	8
	.set	OUT,	12
	.set	OUTBIT,	16

	.data
	.align	2
W:	.long	0				# temp space for output word
	.long	0

	.text
	.align	1


# CYBOOW -- Order odd cyber word.  After swapping the first 8 bytes of IN the
# ordered 60-bit Cyber word is in bits 5-64 of the temporary storage area at W.

_cyboow_: # (in, inbit, out, outbit)
	.word	0x4

	subl3	$1, *INBIT(ap), r0		# bit offset into IN
	ashl	$-3, r0, r0
	addl2	IN(ap), r0			# input base address

	addl3	$8, $W, r1			# swap bytes into W temporary
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)

	movl	OUT(ap), r0			# output base address
	subl3	$1, *OUTBIT(ap), r1		# bit offset into OUT
	extzv	$4, $30, W, r2
	insv	r2, r1, $30, (r0)		# put first 30 bits
	extzv	$34, $30, W, r2
	addl2	$30, r1
	insv	r2, r1, $30, (r0)		# put second 30 bits
	ret

# CYBOEW -- Order even cyber word.  After swapping the 8 bytes the ordered
# Cyber word will be found in bits 1-60 of the temporary storage area at W.

_cyboew_: # (in, inbit, out, outbit)
	.word	0x4
	subl3	$5, *INBIT(ap), r0		# bit offset into IN
	ashl	$-3, r0, r0
	addl2	IN(ap), r0			# input base address

	addl3	$8, $W, r1			# swap bytes into W temporary
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)
	movb	(r0)+, -(r1)

	movl	OUT(ap), r0			# output base address
	subl3	$1, *OUTBIT(ap), r1		# bit offset into OUT
	movl	W, r2
	insv	r2, r1, $32, (r0)		# put first 32 bits
	extzv	$32, $30, W, r2
	addl2	$32, r1
	insv	r2, r1, $28, (r0)		# put remaining 28 bits

	ret
	.data
