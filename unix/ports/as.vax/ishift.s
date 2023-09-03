# IAND, IOR, ISHIFT -- Bitwise boolean integer functions for the NCAR
# package.  The shift function must rotate the bits left and around
# if the nbits to shift argument is positive.

	.data				# Bitwise boolean AND
	.text
	.align	1
	.globl	_iand_
_iand_:
	.word	L12
	jbr 	L14
L15:
	mcoml	*8(ap),r0
	bicl3	r0,*4(ap),r0
	ret
	ret
	.set	L12,0x0
L14:
	jbr 	L15


	.data				# Bitwise boolean OR
	.text
	.align	1
	.globl	_ior_
_ior_:
	.word	L17
	jbr 	L19
L20:
	bisl3	*8(ap),*4(ap),r0
	ret
	ret
	.set	L17,0x0
L19:
	jbr 	L20


	.data				# Bitwise SHIFT
	.text
	.align	1
	.globl	_ishift_
_ishift_:
	.word	L22
	jbr 	L24
L25:
	movl	*8(ap),r11
	jlss	L26
	rotl	r11,*4(ap),r0		# left rotate longword
	ret
L26:
	ashl	r11,*4(ap),r0		# right shift with sign extension
	ret
	ret
	.set	L22,0x800
L24:
	jbr 	L25
	.data
