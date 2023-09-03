# ACLR -- Zero a block of memory.

	.set	MASK,	07400
	.set	A,	4
	.set	NPIX,	8

.data
LZB:
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.align	2
.text
	.globl	_aclrb_				# aclr_ (a, npix)
	.globl	_aclrc_
	.globl	_aclrs_
	.globl	_aclri_
	.globl	_aclrl_
	.globl	_aclrr_
	.globl	_aclrd_
	.globl	_aclrx_
_aclrb_:
	.word	MASK
	movl	*NPIX(ap), r11
	jbr	L10
_aclrc_:
_aclrs_:
	.word	MASK
	mull3	$2, *NPIX(ap), r11
	jbr	L10
_aclri_:
_aclrl_:
_aclrr_:
	.word	MASK
	mull3	$4, *NPIX(ap), r11
	jbr	L10
_aclrd_:
_aclrx_:
	.word	MASK
	mull3	$8, *NPIX(ap), r11
L10:
	jleq	L20
	moval	LZB, r8
	movl	A(ap), r9
	ashl	$-6, r11, r10
	bleq	L12

	# Clear successive 64 byte blocks.
L11:
	movc3	$64, (r8), (r9)
	addl2	$64, r9
	sobgtr	r10, L11
L12:
	# Clear the remaining bytes.

	bicl2	$-64, r11
	movc3	r11, (r8), (r9)
L20:
	ret
