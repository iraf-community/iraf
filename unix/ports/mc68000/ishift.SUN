|# IAND, IOR, ISHIFT -- Bitwise boolean integer functions for the NCAR
|# package.  The shift function must rotate the bits left and around
|# if the nbits to shift argument is positive, and zero fill at the left
|# if the shift is negative (right shift).
|#
|# (SUN/UNIX MC68xxx version)

|# AND -- Bitwise boolean AND: C = AND (A, B)
	.text
	.globl	_iand_
_iand_:
	movl	sp@(4),a0
	movl	a0@,d0
	movl	sp@(8),a0
	andl	a0@,d0
	rts
	

|# OR -- Bitwise boolean OR: C = OR (A, B)
	.text
	.globl	_ior_
_ior_:
	movl	sp@(4),a0
	movl	a0@,d0
	movl	sp@(8),a0
	orl	a0@,d0
	rts


|# ISHIFT -- Bitwise shift:  C = ISHIFT (A, NBITS), +=left
	.text
	.globl	_ishift_
_ishift_:
	movl	sp@(4),a0
	movl	a0@,d0
	movl	sp@(8),a0
	movl	a0@,d1
	blt	L1
	roll	d1,d0		|# left rotate (high bits come in at right)
	rts
L1:
	negl	d1
	lsrl	d1,d0		|# logical shift right (zero at left)
	rts
