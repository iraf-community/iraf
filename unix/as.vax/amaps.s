# AMAPS -- Linear transformation, type short.  The range of pixel values
# A1 to A2 are mapped into the range B1 to B2 using a linear transformation.
# Values less than A1 or greater than A2 are mapped into the values B1 and
# B2 upon output.

	.data	0

	.set	A,	4
	.set	B,	8
	.set	NPIX,	12
	.set	A1,	16
	.set	A2,	20
	.set	B1,	24
	.set	B2,	28

	.align	2
	.globl	_amaps_
	.text

	# AMAPS (a, b, npix, a1, a2, b1, b2)
	#
	# scalar = real (b2 - b1) / real (a2 - a1)
	# minout = min (b1, b2)
	# maxout = max (b1, b2)
	# 
	# do i = 1, npix
	#     b[i] = max(minout, min(maxout,
	#	  PIXEL((a[i] - a1) * scalar) + b1))
	#
	# Registers:
	#	r0	last_a
	#	r1	a
	#	r2	b
	#	r3	scalar
	#	r4	minout
	#	r5	maxout
	#	r6	a1
	#	r7	b1

_amaps_:
	.word	01774			# save r2-r9
	movl	A(ap), r1
	movl	B(ap), r2
	mull3	$2, *NPIX(ap), r0
	addl2	r1, r0
	movw	*A1(ap), r6
	movw	*B1(ap), r7
	movw	*A2(ap), r8
	movw	*B2(ap), r9

	subw3	r7, r9, r3		# r3 = (b2 - b1) / (a2 - a1)
	cvtwf	r3, r3
	subw3	r6, r8, r4
	cvtwf	r4, r4
	divf2	r4, r3

	cmpw	r7, r9			# b1 <= b2
	bleq	L1
	movw	r9, r4			# no, min=b2, max=b1
	movw	r7, r5
	jbr	L2
L1:	movw	r7, r4			# yes, min=b1, max=b2
	movw	r9, r5
L2:
	subw3	r6, (r1)+, r8		# r8 = a[i] - a1
	cvtwf	r8, r8
	mulf2	r3, r8			# (..) * scalar
	cvtfw	r8, r8
	addw2	r7, r8			# (..) + b1

	cmpw	r8, r4			# r8 < minout?
	bgtr	L3
	movw	r4, (r2)+
	jbr	L5
L3:
	cmpw	r8, r5			# r8 > maxout?
	blss	L4
	movw	r5, (r2)+
	jbr	L5
L4:
	movw	r8, (r2)+		# new value in range
L5:
	cmpl	r1, r0
	blssu	L2			# loop again

	ret
